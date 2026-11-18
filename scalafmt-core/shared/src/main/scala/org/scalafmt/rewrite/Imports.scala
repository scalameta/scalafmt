package org.scalafmt
package rewrite

import org.scalafmt.config._
import org.scalafmt.util._

import scala.meta._
import scala.meta.tokens.{Token => T}

import java.util.LinkedList
import java.util.regex.Pattern

import scala.annotation.tailrec
import scala.collection.mutable

import metaconfig._

object Imports extends RewriteFactory {

  case class Settings(
      sort: Sort = Sort.none,
      selectors: Option[Newlines.SourceHints] = None,
      contiguousGroups: ContiguousGroups = ContiguousGroups.only,
      private val groups: Seq[Seq[String]] = Nil,
      removeRedundantSelectors: Boolean = false,
      sortCatchallGroup: SortCatchallGroup = SortCatchallGroup.tail,
  ) {
    private lazy val regex = groups.zipWithIndex
      .flatMap { case (patterns, index) => patterns.map((_, index)) }
      .sortBy(_._1)(Ordering.String.reverse) // longest pattern first
      .map { case (pattern, index) => Pattern.compile(pattern) -> index }

    private[scalafmt] val numGroups = groups.length

    def noGroups: Boolean = sort.eq(Sort.none) && numGroups == 0

    def group(str: String): Int = regex.findOrNull(_._1.matcher(str).matches())
      .nnFold(numGroups)(_._2)
  }

  object Settings {
    implicit val surface: generic.Surface[Settings] = generic.deriveSurface
    implicit val codec: ConfCodecEx[Settings] = generic
      .deriveCodecEx(new Settings).noTypos.withSectionRenames(
        annotation.SectionRename { case Conf.Bool(value) =>
          if (value) Conf.nameOf(Newlines.unfold) else Conf.Null()
        }("expand", "selectors"),
      )

  }

  sealed abstract class ContiguousGroups
  object ContiguousGroups {
    case object no extends ContiguousGroups
    case object only extends ContiguousGroups

    implicit val codec: ConfCodecEx[ContiguousGroups] = ConfCodecEx
      .oneOf(only, no)
  }

  sealed abstract class SortCatchallGroup
  object SortCatchallGroup {
    case object full extends SortCatchallGroup
    case object none extends SortCatchallGroup
    case object tail extends SortCatchallGroup

    implicit val codec: ConfCodecEx[SortCatchallGroup] = ConfCodecEx
      .oneOf(full, none, tail)
  }

  override def hasChanged(v1: RewriteSettings, v2: RewriteSettings): Boolean =
    v1.imports ne v2.imports

  override def create(implicit ctx: RewriteCtx): RewriteSession = {
    val settings = ctx.style.rewrite.imports
    val selectors = ctx.style.importSelectorsRewrite
    if (selectors eq Newlines.unfold) new ExpandFull
    else if (selectors eq Newlines.fold) new Fold
    else if (settings.numGroups != 0) new ExpandPart
    else if (settings.sort ne Sort.none) new ExpandNone
    else null
  }

  private val allImportRules: Set[Rewrite] =
    Set(Imports, ExpandImportSelectors, SortImports, AsciiSortImports)

  def validateImports(obj: RewriteSettings): Configured[RewriteSettings] = {
    val (importRules, nonImportRules) = obj.rules
      .partition(allImportRules.contains)
    if (importRules.isEmpty) return Configured.Ok(obj) // nothing to do

    val errBuf = Seq.newBuilder[String]

    val sortOriginal = importRules.contains(SortImports)
    val sortAscii = importRules.contains(AsciiSortImports)
    if (sortAscii && sortOriginal) errBuf += "SortImports and AsciiSortImports"

    val sortIn = obj.imports.sort
    if (sortIn ne Sort.none)
      if (sortOriginal && (sortIn ne Sort.original)) errBuf +=
        s"SortImports and Imports with `sort=$sortIn"
      else if (sortAscii && (sortIn ne Sort.ascii)) errBuf +=
        s"AsciiSortImports and Imports with `sort=$sortIn"

    val expandRule = importRules.contains(ExpandImportSelectors)
    val selectorsIn = obj.imports.selectors
    selectorsIn.foreach(x =>
      if (expandRule && (x ne Newlines.unfold)) errBuf +=
        s"ExpandImportSelectors and Imports with `selectors=$x`",
    )

    val errors = errBuf.result()
    if (errors.nonEmpty) Configured.error(
      if (errors.lengthCompare(1) == 0) s"Incompatible rewrites: ${errors.head}"
      else errors.mkString("Incompatible rewrites:\n  ", "\n  ", ""),
    )
    else {
      val selectorsOut = selectorsIn
        .orElse(if (expandRule) Some(Newlines.unfold) else None)
      val sortOut =
        if (sortIn ne Sort.none) sortIn
        else if (sortAscii) Sort.ascii
        else if (sortOriginal) Sort.original
        else if (selectorsIn.contains(Newlines.fold)) Sort.fold
        else if (obj.imports.removeRedundantSelectors) Sort.fold
        else Sort.none
      Configured.Ok(obj.copy(
        rules = Imports +: nonImportRules,
        imports = obj.imports.copy(selectors = selectorsOut, sort = sortOut),
      ))
    }
  }

  case class Selectors(
      pretty: String,
      raw: String,
      importees: Seq[Importee],
      commentsBefore: Seq[T] = Seq.empty,
      commentAfter: T = null,
  )

  private class Grouping(
      buffer: mutable.ListBuffer[GroupingEntry] = mutable.ListBuffer.empty,
      stats: mutable.HashSet[String] = mutable.HashSet.empty,
  ) {
    def add(kw: String, ref: String, selectors: Selectors): Unit = {
      val stat = s"$kw $ref${selectors.pretty}"
      if (stats.add(stat)) buffer += GroupingEntry(stat, ref, selectors)
    }
    def result(): Seq[GroupingEntry] =
      try buffer.result()
      finally {
        stats.clear()
        buffer.clear()
      }
  }

  case class GroupingEntry(stat: String, ref: String, selectors: Selectors) {
    lazy val labels = ref.split('.') :+ selectors.raw
  }

  class Selector(val tree: Importee, val text: String) {
    def this(tree: Importee) = this(tree, tree.text)
  }

  sealed abstract class Sort {
    def sortGrouping(buf: Seq[GroupingEntry]): Iterable[GroupingEntry]
    def sortGroupingTail(buf: Seq[GroupingEntry]): Iterable[GroupingEntry]
    def sortSelector(buf: Seq[Importee]): Iterable[Selector]

    protected final def toSelector(tree: Importee): Selector = new Selector(tree)
  }

  private[scalafmt] object Sort {

    implicit val reader: ConfCodecEx[Sort] = ConfCodecEx
      .oneOf[Sort](none, ascii, original, scalastyle)

    abstract class ByPosition extends Sort {

      /** Returns the named importees, then the others, each in source order. */
      override final def sortSelector(buf: Seq[Importee]): Iterable[Selector] =
        buf.sorted(NamedFirstOrdering).map(toSelector)

      def sortGrouping(buf: Seq[GroupingEntry]): Iterable[GroupingEntry] = buf
      def sortGroupingTail(buf: Seq[GroupingEntry]): Iterable[GroupingEntry] =
        buf

    }

    case object none extends ByPosition
    case object fold extends ByPosition

    private object NamedFirstOrdering extends Ordering[Importee] {
      override def compare(x: Importee, y: Importee): Int = {
        val cmp = Ordering.Boolean.compare(isNamed(y), isNamed(x))
        if (cmp != 0) cmp else Ordering.Int.compare(x.pos.start, y.pos.start)
      }
    }

    abstract class SortBase extends Sort {
      val selectorOrdering: Ordering[String]
      val groupingOrdering: Ordering[String]

      private object GroupingEntryOrdering extends Ordering[GroupingEntry] {
        override def compare(x: GroupingEntry, y: GroupingEntry): Int = {
          val xarr = x.labels
          val yarr = y.labels
          compareArraySlices(xarr, 0, xarr.length, yarr, 0, yarr.length)
        }
      }

      private object GroupingEntryTailOrdering extends Ordering[GroupingEntry] {
        override def compare(x: GroupingEntry, y: GroupingEntry): Int = {
          val xarr = x.labels
          val yarr = y.labels
          if (xarr(0) != yarr(0)) 0 // don't reorder; assumes stable sort
          else compareArraySlices(xarr, 1, xarr.length, yarr, 1, yarr.length)
        }
      }

      private def compareArraySlices(
          xarr: Array[String],
          xbeg: Int,
          xend: Int,
          yarr: Array[String],
          ybeg: Int,
          yend: Int,
      ): Int = {
        val xlen = xend - xbeg
        val ylen = yend - ybeg
        val lencmp = xlen - ylen
        val cnt = if (lencmp < 0) xlen else ylen
        @tailrec
        def iter(i: Int): Int =
          if (i == cnt) lencmp
          else {
            val xlabel = xarr(i + xbeg)
            val ylabel = yarr(i + ybeg)
            val cmp =
              if (xlabel.isEmpty) if (ylabel.isEmpty) 0 else -1
              else if (ylabel.isEmpty) 1
              else groupingOrdering.compare(xlabel, ylabel)
            if (cmp != 0) cmp else iter(i + 1)
          }
        iter(0)
      }

      def sortSelector(buf: Seq[Importee]): Iterable[Selector] = {
        // https://docs.scala-lang.org/scala3/reference/contextual/given-imports.html
        val others = new mutable.ListBuffer[Selector]
        val givens = new mutable.ListBuffer[Selector]
        val wildcards = new mutable.ListBuffer[Selector]
        buf.foreach(x =>
          (x match {
            case _: Importee.Given => givens
            case _: Importee.Wildcard | _: Importee.GivenAll => wildcards
            case _ => others
          }) += toSelector(x),
        )
        def sorted(obj: mutable.ListBuffer[Selector]) = obj.result()
          .sortBy(_.text)(selectorOrdering)
        // don't sort wildcards
        sorted(others) ++ sorted(givens) ++ wildcards
      }

      def sortGrouping(buf: Seq[GroupingEntry]): Iterable[GroupingEntry] = buf
        .view.sorted(GroupingEntryOrdering)

      def sortGroupingTail(buf: Seq[GroupingEntry]): Iterable[GroupingEntry] =
        buf.view.sorted(GroupingEntryTailOrdering)
    }

    case object ascii extends SortBase {
      override val selectorOrdering: Ordering[String] = Ordering.String
      override val groupingOrdering: Ordering[String] = Ordering.String
    }

    private abstract class FirstLetterRankOrdering extends Ordering[String] {
      protected def getRank(ch: Char): Int
      protected def compareTail(x: String, y: String): Int
      override final def compare(x: String, y: String): Int = {
        val cmp = Ordering.Int.compare(getRank(x.head), getRank(y.head))
        if (cmp != 0) cmp else compareTail(x, y)
      }
    }

    case object original extends SortBase {
      override val selectorOrdering: Ordering[String] = OriginalOrdering
      override val groupingOrdering: Ordering[String] = OriginalOrdering

      private object OriginalOrdering extends FirstLetterRankOrdering {
        protected def getRank(ch: Char): Int = Character.getType(ch) match {
          case Character.UPPERCASE_LETTER => 2
          case Character.LOWERCASE_LETTER => 1
          case _ => 0
        }
        override protected def compareTail(x: String, y: String): Int = x
          .compare(y)
      }
    }

    case object scalastyle extends SortBase {
      override val selectorOrdering: Ordering[String] = SelectorOrdering
      override val groupingOrdering: Ordering[String] = GroupingOrdering

      private object GroupingOrdering extends FirstLetterRankOrdering {
        protected def getRank(ch: Char): Int = Character.getType(ch) match {
          case Character.UPPERCASE_LETTER => 1
          case Character.LOWERCASE_LETTER => 2
          case _ => 0
        }
        override protected def compareTail(x: String, y: String): Int = x
          .compareToIgnoreCase(y)
      }

      private object SelectorOrdering extends FirstLetterRankOrdering {
        protected def getRank(ch: Char): Int = Character.getType(ch) match {
          case Character.UPPERCASE_LETTER => 2
          case Character.LOWERCASE_LETTER => 1
          case _ => 0
        }
        override protected def compareTail(x: String, y: String): Int = x
          .compareToIgnoreCase(y)
      }
    }

  }

  private final def isUnimport(importee: Importee): Boolean = importee
    .is[Importee.Unimport]

  private final def isWildcard(importee: Importee): Boolean = importee
    .is[Importee.Wildcard]

  private final def isGivenAll(importee: Importee): Boolean = importee
    .is[Importee.GivenAll]

  private final def isGiven(importee: Importee): Boolean = importee
    .is[Importee.Given]

  private final def isARename(importee: Importee): Boolean =
    isUnimport(importee) || importee.is[Importee.Rename]

  private final def isAWildcard(importee: Importee): Boolean =
    isWildcard(importee) || isGivenAll(importee)

  private final def isAGiven(importee: Importee): Boolean = isGiven(importee) ||
    isGivenAll(importee)

  private final def notWildcardOrRename(importee: Importee): Boolean =
    !isAWildcard(importee) && !isARename(importee)

  private final def isNamed(importee: Importee): Boolean = importee
    .isAny[Importee.Name, Importee.Rename, Importee.Unimport]

  private final def getRenamedMemberName(importee: Importee): Name =
    importee match {
      case t: Importee.Rename => t.name
      case t: Importee.Unimport => t.name
      case _ => null
    }

  private final def getRenamedMember(importee: Importee): String =
    getRenamedMemberName(importee).nnMap(_.value)

  /** The key that duplicates share: `*` and `_` are duplicates, as are `b => x`
    * and `b as x`.
    */
  private final def getDuplicateKey(importee: Importee): String = importee match {
    case t: Importee.Name => t.name.value
    case t: Importee.Rename => s"${t.name.value}\n${t.rename.value}"
    case t: Importee.Unimport => s"${t.name.value}\n"
    case _: Importee.Wildcard => ""
    case t: Importee.Given => s"\n${t.tpe.text}"
    case _: Importee.GivenAll => "\n"
    case _ => null
  }

  /** Returns the importers in the order of their earliest importee. */
  private final def sortImporters[A](
      importers: Iterable[A],
  )(importees: A => Seq[Importee]): Seq[A] = {
    val seq = importers.toSeq
    if (seq.lengthCompare(1) <= 0) seq
    else seq.map(x => (importees(x).foldLeft(Int.MaxValue)(_ min _.pos.start), x))
      .sortBy(_._1).map(_._2)
  }

  private implicit class ImplicitImportees(private val obj: Seq[Importee])
      extends AnyVal {

    def hasWildcard: Boolean = obj.exists(isWildcard)
    def hasUnimport: Boolean = obj.exists(isUnimport)
    def hasGiven: Boolean = obj.exists(isAGiven)
    def hasGivenAll: Boolean = obj.exists(isGivenAll)
    def hasRename: Boolean = obj.exists(isARename)

    /** Whether the importer is a wildcard unit: it has a wildcard and renames
      * or unimports that limit the wildcard.
      */
    def isWildcardUnit: Boolean = hasWildcard && hasRename

    /** Whether the importer is a given unit: it has given importees and
      * unimports that limit them.
      */
    def isGivenUnit: Boolean = hasGiven && hasUnimport
    def importsName(member: String): Boolean = obj.exists {
      case t: Importee.Name => t.name.value == member
      case _ => false
    }
    def renames(member: String): Boolean = obj.exists {
      case t: Importee.Rename => t.name.value == member
      case _ => false
    }
    def unimports(member: String): Boolean = obj.exists {
      case t: Importee.Unimport => t.name.value == member
      case _ => false
    }
    def hides(member: String): Boolean = obj
      .exists(getRenamedMember(_) == member)

    /** The renames and unimports that belong to a wildcard or given unit. */
    def unitRenames: Seq[Importee] =
      if (isWildcardUnit) obj.filter(isARename)
      else if (isGivenUnit) obj.filter(isUnimport)
      else Nil

  }

  private implicit class ImplicitClauses(private val obj: Seq[Seq[Importee]])
      extends AnyVal {
    def hasUnimport: Boolean = obj.exists(_.hasUnimport)
    def hasGivenAll: Boolean = obj.exists(_.hasGivenAll)
  }

  private final def filterWithImporteesToKeep(
      importeesToKeep: Set[Importee],
  )(someImportees: Seq[Importee]): Seq[Importee] =
    if (importeesToKeep.isEmpty) someImportees
    else someImportees.filter(importeesToKeep.contains)

  private abstract class Base(implicit ctx: RewriteCtx) extends RewriteSession {

    protected val settings = ctx.style.rewrite.imports

    protected def processImports(stats: Seq[Seq[ImportExportStat]]): Unit

    override final def rewrite(tree: Tree): Unit = tree match {
      case t: Source => processStats(t.stats)
      case t: Pkg.Body => processStats(t.stats)
      case t: Template.Body => processStats(t.stats)
      case t: ImportExportStat if t.parent.isEmpty => processStats(Seq(t))
      case _ =>
    }

    private def processStats(stats: Seq[Stat]): Unit = {
      val seqSeqBuilder = Seq.newBuilder[Seq[ImportExportStat]]
      val seqBuffer = new mutable.ArrayBuffer[ImportExportStat]

      def flush() = if (seqBuffer.nonEmpty) {
        seqSeqBuilder += seqBuffer.toList
        seqBuffer.clear()
      }

      stats.foreach {
        case t: ImportExportStat =>
          if (ctx.tokenTraverser.isExcluded(TreeOps.headTokenOrNull(t))) flush()
          else seqBuffer += t
        case _ => flush()
      }
      flush()

      val importStats = seqSeqBuilder.result()
      if (importStats.nonEmpty) processImports(importStats)
    }

    protected final def getSelector(
        selector: Importee,
        needRaw: Boolean = true,
    ): Selectors = {
      val selectorString = selector.text
      val (commentsBefore, commentAfter) = getCommentsAround(selector)
      if (mustUseBraces(selector)) {
        val sb = new StringBuilder
        sb.append('{')
        commentsBefore.foreach { x =>
          sb.appendNL
          if (x.pos.startColumn != 0) sb.append(' ')
          sb.append(x.text).appendNL
        }
        sb.append(selectorString)
        if (commentAfter ne null) sb.append(' ').append(commentAfter.text)
          .appendNL
        val pretty = sb.append('}').result()
        val hadComments = commentsBefore.nonEmpty || (commentAfter ne null)
        Selectors(
          pretty,
          if (needRaw && hadComments) s"{$selector}" else pretty,
          Seq(selector),
        )
      } else Selectors(
        selectorString,
        selectorString,
        Seq(selector),
        commentsBefore,
        commentAfter,
      )
    }

    protected final def getSelectors(
        selectors: Seq[Importee],
        needRaw: Boolean = true,
    ): Selectors = {
      val selectorCount = selectors.length
      if (selectorCount == 1) getSelector(selectors.head, needRaw = needRaw)
      else {
        val tuples = settings.sort.sortSelector(selectors)
        val sb = new StringBuilder
        var hadComments = false
        sb.append('{')
        var index = 0
        val iter = tuples.iterator
        while (iter.hasNext) {
          val selector = iter.next()
          index += 1
          val (commentBefore, commentAfter) = getCommentsAround(selector.tree)
          if (commentBefore.nonEmpty) hadComments = true
          commentBefore.foreach { x =>
            if (!sb.lastOption.contains('\n')) sb.appendNL
            sb.append(x).appendNL
          }
          sb.append(selector.text)
          if (index != selectorCount) sb.append(',')
          if (commentAfter ne null) {
            hadComments = true
            sb.append(' ').append(commentAfter).appendNL
          }
        }
        val pretty = sb.append('}').result()
        val raw =
          if (hadComments && needRaw) tuples.map(_.text).mkString("{", ",", "}")
          else pretty
        Selectors(pretty, raw, selectors)
      }
    }

    protected final def getImporteesToKeep(
        importees: => Iterable[Importee],
    ): Set[Importee] =
      if (!settings.removeRedundantSelectors) Set.empty
      else {
        val res = Set.newBuilder[Importee]
        var hadWildcard = false
        var hadGivenAll = false
        val names = mutable.HashMap.empty[String, Importee.Name]
        val givens = mutable.HashMap.empty[String, Importee.Given]
        val iter = importees.iterator
        while (iter.hasNext) iter.next() match {
          case x: Importee.Wildcard =>
            if (!hadWildcard) { res += x; hadWildcard = true }
          case x: Importee.GivenAll =>
            if (!hadGivenAll) { res += x; hadGivenAll = true }
          case x: Importee.Name =>
            if (!hadWildcard) names.update(x.name.value, x)
          case x: Importee.Given => if (!hadGivenAll) givens.update(x.text, x)
          case x => res += x
        }
        if (!hadWildcard) res ++= names.values
        if (!hadGivenAll) res ++= givens.values
        res.result()
      }

    protected final def filterImportees(importer: Importer): Seq[Importee] = {
      val seq = importer.importees
      filterWithImporteesToKeep(getImporteesToKeep(seq))(seq)
    }

    private final def mustUseBraces(tree: Importee): Boolean = {
      val name = getRenamedMemberName(tree)
      (null ne name) && // in scala3, `as` doesn't need braces
      ctx.tokenTraverser.nextNonTrivialToken(TreeOps.lastTokenOrNull(name))
        .is[T.RightArrow]
    }

    protected final def getCommentsAround(tree: Tree): (Seq[T], T) =
      getCommentsBefore(tree) -> getCommentAfter(tree)

    protected final def getCommentsBefore(tok: T): Seq[T] = {
      var hadLf = false
      val slc = new mutable.ListBuffer[T]
      ctx.tokenTraverser.findAtOrBefore(ctx.getIndex(tok) - 1) {
        case t: T.AtEOL =>
          if (hadLf || t.newlines > 1) MaybeBool.True
          else { hadLf = true; MaybeBool.Maybe }
        case t: T.Comment if TokenOps.isSingleLineIfComment(t) =>
          slc.prepend(t); hadLf = false; MaybeBool.Maybe
        case _: T.Whitespace => MaybeBool.Maybe
        case _: T.LeftBrace => MaybeBool.False
        case _ => if (!hadLf && slc.nonEmpty) slc.remove(0); MaybeBool.False
      }
      slc.result()
    }

    protected final def getCommentsBefore(tree: Tree): Seq[T] =
      getCommentsBefore(TreeOps.headTokenOrNull(tree))

    protected final def getCommentAfter(tok: T): T = ctx.tokenTraverser
      .findAtOrAfter(ctx.getIndex(tok) + 1) {
        case _: T.HSpace | _: T.Comma => MaybeBool.Maybe
        case t: T.Comment => MaybeBool(TokenOps.isSingleLineIfComment(t))
        case _ => MaybeBool.False
      }

    protected final def getCommentAfter(tree: Tree): T =
      getCommentAfter(TreeOps.lastTokenOrNull(tree))

    private val eol = LineEndings
      .eol(ctx.style.lineEndings.contains(LineEndings.windows))

    protected implicit class ImplicitStringBuilder(private val sb: StringBuilder) {
      def appendNL: StringBuilder = sb.append(eol)
    }

  }

  private abstract class ExpandBase(implicit ctx: RewriteCtx) extends Base {

    protected def addClausesToGroup(
        group: Grouping,
        kw: String,
        ref: String,
        importers: Seq[Importer],
    ): Unit

    protected val groups = Array.fill(settings.numGroups + 1)(new Grouping)

    protected def addSelectorsToGroup(
        group: Grouping,
        kw: String,
        ref: String,
        importees: Seq[Importee],
    ): Unit = if (importees.nonEmpty) group.add(kw, ref, getSelectors(importees))

    /** Returns the importees of each importer that keeps any, without
      * duplicates and without those that other importers make redundant.
      */
    protected final def removeRedundant(
        importers: Seq[Importer],
    ): IndexedSeq[Seq[Importee]] = {
      val removing = settings.removeRedundantSelectors
      var clauses: IndexedSeq[Seq[Importee]] = importers.iterator
        .map(x => removeDuplicates(x.importees)).toIndexedSeq
      if (removing) {
        // a plain `given` covers every `given T`
        if (clauses.hasGivenAll)
          clauses = clauses.map(_.filterNot(_.is[Importee.Given]))
        clauses = removeUnimports(clauses)
        clauses = replaceUnimports(clauses)
      }
      clauses = removeRepeatedRenames(clauses)
      clauses.filter(_.nonEmpty)
    }

    /** Returns the importees without duplicates, keeping the earliest. */
    private def removeDuplicates(importees: Seq[Importee]): Seq[Importee] =
      if (importees.lengthCompare(1) <= 0) importees
      else {
        val keys = mutable.HashSet.empty[String]
        importees.filter(x => keys.add(getDuplicateKey(x)))
      }

    /** Returns the importers, keeping an unimport only if they would import
      * more without it.
      */
    private def removeUnimports(
        clauses: IndexedSeq[Seq[Importee]],
    ): IndexedSeq[Seq[Importee]] =
      if (!clauses.hasUnimport) clauses
      else clauses.indices.map { i =>
        val clause = clauses(i)
        if (!clause.hasUnimport) clause
        else {
          val hasOwnWildcard = clause.hasWildcard
          val hasOwnGiven = clause.hasGiven
          def existsOther(f: Seq[Importee] => Boolean): Boolean = clauses
            .indices.exists(j => j != i && f(clauses(j)))
          clause.filter {
            case x: Importee.Unimport =>
              val member = x.name.value
              !clause.importsName(member) && !clause.renames(member) &&
              (hasOwnWildcard && !existsOther(y =>
                y.importsName(member) || y.hasWildcard && !y.hides(member),
              ) || hasOwnGiven && !existsOther(y =>
                y.importsName(member) || y.renames(member) ||
                  y.hasGivenAll && !y.unimports(member),
              ))
            case _ => true
          }
        }
      }

    /** Returns the importers with each unimport in a wildcard unit replaced by
      * the earliest rename of its member outside units, and without that rename
      * in its own importer.
      */
    private def replaceUnimports(
        clauses: IndexedSeq[Seq[Importee]],
    ): IndexedSeq[Seq[Importee]] =
      if (!clauses.exists(x => x.hasUnimport && x.hasWildcard)) clauses
      else {
        val unimported = mutable.HashSet.empty[String]
        clauses.foreach(x =>
          if (x.isWildcardUnit) x.foreach {
            case t: Importee.Unimport => unimported += t.name.value
            case _ =>
          },
        )
        // importers are in source order, so the first rename is the earliest
        val replacements = mutable.HashMap.empty[String, Importee]
        clauses.foreach(x =>
          if (!x.isWildcardUnit) x.foreach {
            case t: Importee.Rename if unimported.contains(t.name.value) =>
              replacements.getOrElseUpdate(t.name.value, t)
            case _ =>
          },
        )
        if (replacements.isEmpty) clauses
        else clauses.map(x =>
          if (x.isWildcardUnit) x.map {
            case t: Importee.Unimport => replacements.getOrElse(t.name.value, t)
            case t => t
          }
          else x.filter {
            case t: Importee.Rename => replacements
                .getOrElse(t.name.value, null) ne t
            case _ => true
          },
        )
      }

    /** Returns the importers without each rename or unimport outside units that
      * repeats one inside a unit.
      */
    private def removeRepeatedRenames(
        clauses: IndexedSeq[Seq[Importee]],
    ): IndexedSeq[Seq[Importee]] = {
      val unitRenames = clauses.map(_.unitRenames)
      val keys = mutable.HashSet.empty[String]
      unitRenames.foreach(_.foreach(keys += getDuplicateKey(_)))
      if (keys.isEmpty) clauses
      else clauses.zip(unitRenames).map { case (x, inUnit) =>
        x.filter(t =>
          !isARename(t) || inUnit.contains(t) ||
            !keys.contains(getDuplicateKey(t)),
        )
      }
    }

    private def processImports(stats: Iterable[ImportExportStat]): String = {
      val indent = {
        @tailrec
        def iter(off: Int, nonWs: T): String =
          if (off == 0) ""
          else {
            val nextOff = off - 1
            ctx.tokens(nextOff) match {
              case t: T.AtEOL => t.input.text.substring(t.end, nonWs.start)
              case _: T.Whitespace => iter(nextOff, nonWs)
              case t => iter(nextOff, t)
            }
          }
        val head = TreeOps.headTokenOrNull(stats.head)
        iter(ctx.tokenTraverser.getIndex(head), head)
      }
      val folding = settings.removeRedundantSelectors ||
        (ctx.style.importSelectorsRewrite eq Newlines.fold)
      val foldMap = mutable.LinkedHashMap
        .empty[(String, String), mutable.ListBuffer[Importer]]
      def addToGroup(kw: String, ref: String, importers: Seq[Importer]): Unit =
        addClausesToGroup(groups(settings.group(ref)), kw, ref, importers)
      stats.foreach { s =>
        val kw = TreeOps.headTokenOrNull(s).toString
        s.importers.foreach { i =>
          val ref = getRef(i)
          if (!folding) addToGroup(kw, ref, i :: Nil)
          else foldMap.getOrElseUpdate((kw, ref), mutable.ListBuffer.empty)
            .append(i)
        }
      }
      foldMap.foreach { case ((kw, ref), v) => addToGroup(kw, ref, v.toList) }

      val groupEntries = groups.map(_.result())
      val countsToWrite = mutable.HashMap.empty[Importer, Int]
      val countImportee: Importee => Unit = _.parent match {
        case Some(p: Importer) =>
          countsToWrite(p) = countsToWrite.getOrElse(p, 0) + 1
        case _ =>
      }
      groupEntries.foreach(_.foreach(_.selectors.importees.foreach(countImportee)))
      val seenImports = mutable.HashMap.empty[Importer, Int]
      val partialComments = mutable.ListBuffer.empty[T]
      val appendPartialComment: T => Unit = partialComments += _
      val sb = new StringBuilder()
      @inline
      def appendIndent(): Unit = if (sb.nonEmpty) sb.append(indent)
      def appendComment(token: T) = {
        if (token.pos.startColumn != 0) appendIndent()
        sb.append(token.text).appendNL
      }
      def processImporteeComments(
          appendTailComment: T => Unit,
      )(tree: Importee): Unit = tree.parent match {
        case Some(p: Importer) =>
          val newSeen = seenImports.updateWith(p)(x => Some(1 + x.getOrElse(0)))
          val isHead = newSeen.contains(1)
          val isLast = newSeen.contains(countsToWrite.getOrElse(p, 0))
          if (isLast) getCommentAfter(p).nnFor(
            if (newSeen.contains(p.importees.length)) appendTailComment
            else appendPartialComment,
          )
          p.parent match {
            case Some(pp: ImportExportStat) =>
              if (isHead && pp.importers.headOption.contains(p))
                getCommentsBefore(pp).foreach(appendComment)
            case _ =>
          }
          if (isHead) getCommentsBefore(p).foreach(appendComment)
        case _ =>
      }

      val groupsIterator = groupEntries.iterator
      def sortGroup(entries: Seq[GroupingEntry]): Iterable[GroupingEntry] = {
        val sortCatchallGroup =
          if (groupsIterator.hasNext || settings.numGroups == 0)
            SortCatchallGroup.full
          else settings.sortCatchallGroup
        sortCatchallGroup match {
          case SortCatchallGroup.none => entries
          case SortCatchallGroup.full => settings.sort.sortGrouping(entries)
          case SortCatchallGroup.tail => settings.sort.sortGroupingTail(entries)
        }
      }

      groupsIterator.foreach { entries =>
        if (entries.nonEmpty) {
          if (sb.nonEmpty) sb.appendNL
          // sort and add empty line in all groups
          sortGroup(entries).foreach { entry =>
            import entry._, selectors._
            val commentsAfterBuilder = Seq.newBuilder[T]
            val appendTailComment: T => Unit = commentsAfterBuilder += _
            importees.foreach(processImporteeComments(appendTailComment))
            commentsBefore.foreach(appendComment)
            // prefer the comment of an importer that we write whole
            if (partialComments.nonEmpty) {
              commentsAfterBuilder ++= partialComments
              partialComments.clear()
            }
            val commentsAfter = commentsAfterBuilder.result().iterator
            val trailingComment = commentAfter ?? {
              if (commentsAfter.hasNext) commentsAfter.next() else null
            }
            commentsAfter.foreach(comment =>
              if (commentAfter ne comment) appendComment(comment),
            )
            appendIndent()
            sb.append(stat)
            if (trailingComment ne null) sb.append(' ')
              .append(trailingComment.text)
            sb.appendNL
          }
        }
      }
      sb.result()
    }

    override protected def processImports(
        stats: Seq[Seq[ImportExportStat]],
    ): Unit =
      if (settings.noGroups) processEachLine(stats)
      else if (settings.contiguousGroups eq ContiguousGroups.only)
        processEachGroup(stats)
      else processAllGroups(stats)

    private def getTokenRange(x: Seq[ImportExportStat]): (T, T) = {
      val headTok = TreeOps.headTokenOrNull(x.head)
      val lastTok = TreeOps.lastTokenOrNull(x.last)
      (
        getCommentsBefore(headTok).headOption.getOrElse(headTok),
        getCommentAfter(lastTok) ?? lastTok,
      )
    }

    private def processEachLine(stats: Seq[Seq[ImportExportStat]]): Unit = stats
      .flatten.foreach { stat =>
        val group = Seq(stat)
        val importString = processImports(group)
        processTokenRanges(importString, getTokenRange(group))
      }

    private def processEachGroup(stats: Seq[Seq[ImportExportStat]]): Unit =
      stats.foreach { group =>
        val tokenRange = getTokenRange(group)
        val importString = processImports(group)
        processTokenRanges(importString, tokenRange)
      }

    private def processAllGroups(stats: Seq[Seq[ImportExportStat]]): Unit = {
      val tokenRanges = stats.map(getTokenRange)
      val importString = processImports(stats.flatten)
      processTokenRanges(importString, tokenRanges: _*)
    }

    private def processTokenRanges(
        importString: String,
        tokenRanges: (T, T)*,
    ): Unit = {
      implicit val patchBuilder = Seq.newBuilder[TokenPatch]

      // replace the first token
      val first = tokenRanges.head._1
      patchBuilder += TokenPatch.Replace(first, importString)

      // remove all tokens except first
      val tokens = ctx.tokens.view
      tokenRanges.foreach { case (beg, end) =>
        val begIdx = ctx.tokenTraverser.getIndex(beg)
        val endIdx = ctx.tokenTraverser.getIndex(end)
        tokens.slice(if (beg eq first) begIdx + 1 else begIdx, endIdx + 1)
          .foreach(patchBuilder += TokenPatch.Remove(_))
        ctx.removeLFToAvoidEmptyLine(begIdx, endIdx)
      }
      ctx.addPatchSet(patchBuilder.result(): _*)
    }

  }

  /** convert
    * {{{
    *   import a.{c, b}, d.{f, e}
    * }}}
    * to
    * {{{
    *   import a.b
    *   import a.c
    *
    *   import d.e
    *   import d.f
    * }}}
    */
  private class ExpandFull(implicit ctx: RewriteCtx) extends Fold {
    override protected final def getSelectorsToWrite(
        clauses: IndexedSeq[Seq[Importee]],
    ): Iterable[Selectors] = {
      val res = new mutable.ListBuffer[Seq[Importee]]
      foldImportees(clauses).foreach(expand(_, res))
      res.map(getSelectors(_))
    }

    private def expand(
        importees: Seq[Importee],
        dst: mutable.ListBuffer[Seq[Importee]],
    ): Unit =
      // if there's a wildcard, unimports and renames must come with it, cannot be expanded
      if (
        importees.dropWhile(notWildcardOrRename).drop(1).exists(isAWildcard)
      ) {
        val filtered = importees.filter { x =>
          val expanding = notWildcardOrRename(x)
          if (expanding) dst += x :: Nil
          !expanding
        }
        dst += filtered
      } else // expand all
        importees.foreach(dst += _ :: Nil)
  }

  /** convert
    * {{{
    *   import a.{c, b}, d.{f, e}
    * }}}
    * to
    * {{{
    *   import a.{b, c}
    *
    *   import d.{e, f}
    * }}}
    */
  private class ExpandPart(implicit ctx: RewriteCtx) extends ExpandBase {
    override protected final def addClausesToGroup(
        group: Grouping,
        kw: String,
        ref: String,
        importers: Seq[Importer],
    ): Unit = {
      val importeesToKeep = getImporteesToKeep(importers.flatMap(_.importees))
      importers.foreach { importer =>
        val importees =
          filterWithImporteesToKeep(importeesToKeep)(importer.importees)
        addSelectorsToGroup(group, kw, ref, importees)
      }
    }
  }

  /** convert
    * {{{
    *   import a.c, d.f
    *   import a.b, d.e
    * }}}
    * to
    * {{{
    *   import a.{b, c}
    *   import d.{e, f}
    * }}}
    */
  private class Fold(implicit ctx: RewriteCtx) extends ExpandBase {
    override protected final def addClausesToGroup(
        group: Grouping,
        kw: String,
        ref: String,
        importers: Seq[Importer],
    ): Unit = {
      val clauses = removeRedundant(importers)
      sortImporters(getSelectorsToWrite(clauses))(_.importees)
        .foreach(group.add(kw, ref, _))
    }

    /** Returns the selectors to write for the importees left after removal. */
    protected def getSelectorsToWrite(
        clauses: IndexedSeq[Seq[Importee]],
    ): Iterable[Selectors] = foldImportees(clauses).map(getSelectors(_))

    protected final def foldImportees(
        clauses: IndexedSeq[Seq[Importee]],
    ): Seq[Seq[Importee]] = {
      type Importees = LinkedList[Importee]
      def appendTo(src: Seq[Importee], dst: Importees): Importees = {
        src.foreach(dst.add)
        dst
      }
      var globalWildcard: Importee.Wildcard = null
      var globalGivenAll: Importee.GivenAll = null
      // below: variable names refer to presence of renames and wildcards
      val neither = new Importees
      val both = Seq.newBuilder[Seq[Importee]]
      val renamesOnly = new Importees
      val wildcardsOnly = Seq.newBuilder[Importee]
      clauses.foreach { clause =>
        var hasRename = false
        var wildcard: Importee.Wildcard = null
        val hasBoth = clause.exists { // stop if has both
          case x: Importee.Wildcard =>
            if (wildcard eq null) wildcard = x; hasRename
          case _: Importee.Rename | _: Importee.Unimport =>
            hasRename = true; wildcard ne null
          case x: Importee.GivenAll =>
            if (globalGivenAll eq null) globalGivenAll = x; false
          case _ => false
        }
        if (hasBoth) both += clause
        else if (hasRename) appendTo(clause, renamesOnly)
        else if (wildcard ne null) {
          globalWildcard = wildcard
          wildcardsOnly ++= clause
        } else appendTo(clause, neither)
      }

      val buffer = mutable.LinkedHashMap.empty[String, Importee]
      def tryFold(name: String, owner: Importee)(
          f: Option[Importee] => Int,
      ): Boolean = {
        var res = 0
        buffer.updateWith(name) { xOpt =>
          res = f(xOpt)
          if (res > 0) Some(owner) else xOpt
        }
        res != 0
      }

      def fold(
          hasBoth: Int, // 0 no, 1 part of importer with both, -1 both folded previously
      )(importee: Importee): Boolean = importee match {
        case x: Importee.Name => tryFold(x.name.value, x) {
            case Some(_: Importee.Name) => -1
            case None
                if settings.removeRedundantSelectors &&
                  (hasBoth != 0 || (globalWildcard ne null)) => -1
            case None => 1
            case _ => 0
          }
        case x: Importee.Rename => tryFold(x.name.value, x) {
            case Some(y: Importee.Rename) if x.rename.value == y.rename.value =>
              -1
            case None if hasBoth >= 0 => 1
            case _ => 0
          }
        case x: Importee.Unimport => tryFold(x.name.value, x) {
            case Some(_: Importee.Unimport) => -1
            case None if hasBoth >= 0 => 1
            case _ => 0
          }
        case x: Importee.Wildcard =>
          ((globalWildcard eq null) || (globalWildcard eq x)) &&
          tryFold("_", x)(y => if (y.isEmpty) 1 else -1)
        case x: Importee.GivenAll =>
          ((globalGivenAll eq null) || (globalGivenAll eq x)) &&
          tryFold("given", x)(y => if (y.isEmpty) 1 else -1)
        case x => tryFold(x.text, x)(y => if (y.isEmpty) 1 else -1)
      }

      val folded = new mutable.ListBuffer[Seq[Importee]]
      def flushFolded(): Unit = if (buffer.nonEmpty) {
        folded += buffer.values.toSeq
        buffer.clear()
      }

      def foldNoWildcards(list: Importees, hasBoth: Int): Unit = {
        val iter = list.listIterator()
        while (iter.hasNext) {
          val elem = iter.next()
          if (fold(hasBoth = hasBoth)(elem)) iter.remove()
        }
      }

      def flushWithNoWildcards(hasBoth: Int): Unit = {
        foldNoWildcards(neither, hasBoth = hasBoth)
        foldNoWildcards(renamesOnly, hasBoth = hasBoth)
        flushFolded()
      }

      both.result().foreach { clause =>
        clause.foreach(fold(hasBoth = 1))
        flushWithNoWildcards(hasBoth = -1)
      }

      while (!renamesOnly.isEmpty) flushWithNoWildcards(hasBoth = 0)

      wildcardsOnly.result().foreach(fold(hasBoth = 0))
      neither.forEach(fold(hasBoth = 0))
      flushFolded()
      folded.result()
    }
  }

  /** convert
    * {{{
    *   import a.{c, b}, d.{f, e}
    * }}}
    * to
    * {{{
    *   import a.{b, c}, d.{e, f}
    * }}}
    */
  private class ExpandNone(implicit ctx: RewriteCtx) extends Base {
    override protected def processImports(
        stats: Seq[Seq[ImportExportStat]],
    ): Unit = stats.flatten.foreach { t =>
      val patchBuilder = Seq.newBuilder[TokenPatch]
      t.importers.foreach { importer =>
        val replacement = getRef(importer) +
          getSelectors(filterImportees(importer), needRaw = false).pretty
        val tokens: Iterator[T] = importer.tokens.iterator
        // replace the first token
        patchBuilder += TokenPatch.Replace(tokens.next(), replacement)
        // remove all tokens except first
        tokens.foreach(patchBuilder += TokenPatch.Remove(_))
      }
      ctx.addPatchSet(patchBuilder.result(): _*)
    }
  }

  private def getRef(importer: Importer): String = {
    val ref = importer.ref.text
    if (ref.isEmpty) "" else ref + "."
  }

}

abstract class ShouldUseImports extends RewriteFactory {
  override final def create(implicit ctx: RewriteCtx): RewriteSession =
    throw new NotImplementedError("should use Imports")
}

object ExpandImportSelectors extends ShouldUseImports

/** Sort imports with symbols at the beginning, followed by lowercase and
  * finally uppercase
  */
object SortImports extends ShouldUseImports

/** Sort imports using the traditional ASCII sorting
  *
  * See:
  * http://support.ecisolutions.com/doc-ddms/help/reportsmenu/ascii_sort_order_chart.htm
  */
object AsciiSortImports extends ShouldUseImports
