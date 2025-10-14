package org.scalafmt.rewrite

import org.scalafmt.config._
import org.scalafmt.util._

import scala.meta._
import scala.meta.tokens.{Token => T}

import java.util.regex.Pattern

import scala.annotation.tailrec
import scala.collection.mutable.{
  ArrayBuffer, HashMap, HashSet, LinkedHashMap, ListBuffer,
}

import metaconfig._

object Imports extends RewriteFactory {

  case class Settings(
      sort: Sort = Sort.none,
      selectors: Option[Newlines.SourceHints] = None,
      contiguousGroups: ContiguousGroups = ContiguousGroups.only,
      private val groups: Seq[Seq[String]] = Nil,
      removeRedundantSelectors: Boolean = false,
  ) {
    private lazy val regex = groups.zipWithIndex
      .flatMap { case (patterns, index) => patterns.map((_, index)) }
      .sortBy(_._1)(Ordering.String.reverse) // longest pattern first
      .map { case (pattern, index) => Pattern.compile(pattern) -> index }

    private[scalafmt] val numGroups = groups.length

    def noGroups: Boolean = sort.eq(Sort.none) && numGroups == 0

    def group(str: String): Int = regex.find(_._1.matcher(str).matches())
      .fold(numGroups)(_._2)
  }

  object Settings {
    implicit val surface: generic.Surface[Settings] = generic.deriveSurface
    implicit val codec: ConfCodecEx[Settings] = generic
      .deriveCodecEx(new Settings).noTypos
      .withSectionRenames(annotation.SectionRename(
        "expand",
        "selectors",
        { case Conf.Bool(value) => if (value) Conf.Str("unfold") else Conf.Null() },
      ))

  }

  sealed abstract class ContiguousGroups
  object ContiguousGroups {
    case object no extends ContiguousGroups
    case object only extends ContiguousGroups

    implicit val codec: ConfCodecEx[ContiguousGroups] = ReaderUtil
      .oneOf(only, no)
  }

  override def hasChanged(v1: RewriteSettings, v2: RewriteSettings): Boolean =
    v1.imports ne v2.imports

  override def create(implicit ctx: RewriteCtx): RewriteSession = {
    val settings = ctx.style.rewrite.imports
    if (settings.selectors.contains(Newlines.unfold)) new ExpandFull
    else if (settings.selectors.contains(Newlines.fold)) new Fold
    else if (settings.numGroups != 0) new ExpandPart
    else if (settings.sort ne Sort.none) new ExpandNone
    else new RewriteSession.None
  }

  private val allImportRules: Set[Rewrite] =
    Set(Imports, ExpandImportSelectors, SortImports, AsciiSortImports)

  def validateImports(obj: RewriteSettings): Configured[RewriteSettings] = {
    val (importRules, nonImportRules) = obj.rules
      .partition(allImportRules.contains)

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
      cnt: Int,
      commentsBefore: Seq[T] = Seq.empty,
      commentAfter: Option[T] = None,
  )

  private class Grouping(
      buffer: ListBuffer[GroupingEntry] = ListBuffer.empty,
      stats: HashSet[String] = HashSet.empty,
  ) {
    def add(
        kw: String,
        ref: String,
        selectors: Selectors,
        owners: Seq[Importer],
    ): Unit = {
      val stat = s"$kw $ref${selectors.pretty}"
      if (stats.add(stat)) buffer += GroupingEntry(stat, ref, selectors, owners)
    }
    def result(): Seq[GroupingEntry] =
      try buffer.result()
      finally {
        stats.clear()
        buffer.clear()
      }
  }

  case class GroupingEntry(
      stat: String,
      ref: String,
      selectors: Selectors,
      owners: Seq[Importer],
  ) {
    lazy val labels = ref.split('.') :+ selectors.raw
  }

  sealed abstract class Sort {
    protected def sortSelectors(
        buf: Seq[(Importee, String)],
    ): Iterable[(Importee, String)]
    def sortGrouping(buf: Seq[GroupingEntry]): Iterable[GroupingEntry]

    def sortSelector(buf: Seq[Importee]): Iterable[(Importee, String)] = {
      // https://docs.scala-lang.org/scala3/reference/contextual/given-imports.html
      val others = new ListBuffer[(Importee, String)]
      val givens = new ListBuffer[(Importee, String)]
      val wildcards = new ListBuffer[(Importee, String)]
      buf.foreach { x =>
        val dst =
          if (x.is[Importee.Given]) givens
          else if (isWildcard(x)) wildcards
          else others
        dst += selectorToTuple(x)
      }
      def sorted(obj: ListBuffer[(Importee, String)]) = sortSelectors(obj.result())
      // don't sort wildcards
      Iterable.concat(sorted(others), sorted(givens), wildcards)
    }

    protected final def selectorToTuple(tree: Importee): (Importee, String) =
      (tree, tree.toString)
  }

  private[scalafmt] object Sort {

    implicit val reader: ConfCodecEx[Sort] = ReaderUtil
      .oneOf[Sort](none, ascii, original, scalastyle)

    case object none extends Sort {
      override def sortSelector(
          buf: Seq[Importee],
      ): Iterable[(Importee, String)] = buf.map(selectorToTuple)

      protected def sortSelectors(
          buf: Seq[(Importee, String)],
      ): Iterable[(Importee, String)] = buf

      def sortGrouping(buf: Seq[GroupingEntry]): Iterable[GroupingEntry] = buf
    }

    case object fold extends Sort {
      protected def sortSelectors(
          buf: Seq[(Importee, String)],
      ): Iterable[(Importee, String)] = buf

      def sortGrouping(buf: Seq[GroupingEntry]): Iterable[GroupingEntry] = buf
    }

    abstract class SortBase extends Sort {
      val selectorOrdering: Ordering[String]
      val groupingOrdering: Ordering[String]

      private object GroupingEntryOrdering extends Ordering[GroupingEntry] {
        override def compare(x: GroupingEntry, y: GroupingEntry): Int = {
          val xarr = x.labels
          val yarr = y.labels
          val lencmp = xarr.length - yarr.length
          val cnt = if (lencmp < 0) xarr.length else yarr.length
          @tailrec
          def iter(i: Int): Int =
            if (i == cnt) lencmp
            else {
              val xlabel = xarr(i)
              val ylabel = yarr(i)
              val cmp =
                if (xlabel.isEmpty) if (ylabel.isEmpty) 0 else -1
                else if (ylabel.isEmpty) 1
                else groupingOrdering.compare(xlabel, ylabel)
              if (cmp != 0) cmp else iter(i + 1)
            }
          iter(0)
        }
      }

      protected def sortSelectors(
          buf: Seq[(Importee, String)],
      ): Iterable[(Importee, String)] = buf.sortBy(_._2)(selectorOrdering)

      def sortGrouping(buf: Seq[GroupingEntry]): Iterable[GroupingEntry] = buf
        .view.sorted(GroupingEntryOrdering)
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

  private final def isRename(importee: Importee): Boolean = importee
    .isAny[Importee.Rename, Importee.Unimport]

  private final def isWildcard(importee: Importee): Boolean = importee
    .isAny[Importee.Wildcard, Importee.GivenAll]

  private final def notWildcardOrRename(importee: Importee): Boolean =
    !isWildcard(importee) && !isRename(importee)

  private final def filterWithAllowedImportees(
      allowedImportees: Set[Importee],
  )(someImportees: Seq[Importee]): Seq[Importee] =
    if (allowedImportees.isEmpty) someImportees
    else someImportees.filter(allowedImportees.contains)

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
      val seqBuffer = new ArrayBuffer[ImportExportStat]

      def flush() = if (seqBuffer.nonEmpty) {
        seqSeqBuilder += seqBuffer.toList
        seqBuffer.clear()
      }

      stats.foreach {
        case t: ImportExportStat =>
          if (ctx.tokenTraverser.isExcluded(t.tokens.head)) flush()
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
      val selectorString = selector.toString()
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
        commentAfter.foreach(x => sb.append(' ').append(x.text).appendNL)
        val pretty = sb.append('}').result()
        val hadComments = commentsBefore.nonEmpty || commentAfter.isDefined
        Selectors(
          pretty,
          if (needRaw && hadComments) s"{$selector}" else pretty,
          1,
        )
      } else Selectors(
        selectorString,
        selectorString,
        1,
        commentsBefore,
        commentAfter,
      )
    }

    protected final def getSelectors(
        importees: Seq[Importee],
        needRaw: Boolean = true,
    ): Selectors = {
      val selectorsToKeep = getImporteesToKeep(importees)
      val selectors = filterWithAllowedImportees(selectorsToKeep)(importees)
      val selectorCount = selectors.length
      if (selectorCount == 1) getSelector(selectors.head, needRaw = needRaw)
      else {
        val tuples = settings.sort.sortSelector(selectors)
        val sb = new StringBuilder
        var hadComments = false
        sb.append('{')
        var index = 0
        tuples.foreach { case (selector, selectorString) =>
          index += 1
          val (commentBefore, commentAfter) = getCommentsAround(selector)
          commentBefore.foreach { x =>
            hadComments = true
            if (!sb.lastOption.contains('\n')) sb.appendNL
            sb.append(x).appendNL
          }
          sb.append(selectorString)
          if (index != selectorCount) sb.append(',')
          commentAfter.foreach { x =>
            hadComments = true
            sb.append(' ').append(x).appendNL
          }
        }
        val pretty = sb.append('}').result()
        Selectors(
          pretty,
          if (hadComments && needRaw) tuples.map(_._2).mkString("{", ",", "}")
          else pretty,
          selectorCount,
        )
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
        val names = HashMap.empty[String, Importee.Name]
        val givens = HashMap.empty[String, Importee.Given]
        importees.foreach {
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

    private final def mustUseBraces(tree: Importee): Boolean = (tree match {
      case t: Importee.Rename => Some(t.name)
      case t: Importee.Unimport => Some(t.name)
      case _ => None
    }).exists(x =>
      // in scala3, `as` doesn't need braces
      ctx.tokenTraverser.nextNonTrivialToken(x.tokens.last).is[T.RightArrow],
    )

    protected final def getCommentsAround(tree: Tree): (Seq[T], Option[T]) = {
      val tokens = tree.tokens
      val beg = getCommentsBefore(tokens)
      val end = getCommentAfter(tokens)
      beg -> end
    }

    protected final def getCommentsBefore(tok: T): Seq[T] = {
      var hadLf = false
      val slc = new ListBuffer[T]
      ctx.tokenTraverser.findAtOrBefore(ctx.getIndex(tok) - 1) {
        case t: T.AtEOL =>
          if (hadLf || t.newlines > 1) Some(true) else { hadLf = true; None }
        case t: T.Comment if TokenOps.isSingleLineIfComment(t) =>
          slc.prepend(t); hadLf = false; None
        case _: T.Whitespace => None
        case _: T.LeftBrace => Some(false)
        case _ => if (!hadLf && slc.nonEmpty) slc.remove(0); Some(false)
      }
      slc.result()
    }

    protected final def getCommentsBefore(tokens: Tokens): Seq[T] =
      getCommentsBefore(tokens.head)

    protected final def getCommentAfter(tok: T): Option[T] = ctx.tokenTraverser
      .findAtOrAfter(ctx.getIndex(tok) + 1) {
        case _: T.HSpace | _: T.Comma => None
        case t: T.Comment => Some(TokenOps.isSingleLineIfComment(t))
        case _ => Some(false)
      }

    protected final def getCommentAfter(tokens: Tokens): Option[T] =
      getCommentAfter(tokens.last)

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

    protected final def addSelectorsToGroup(
        group: Grouping,
        kw: String,
        ref: String,
        importers: Seq[Importer],
        importees: Seq[Importee] = Nil,
    ): Unit = {
      val selectors =
        if (importees.isEmpty) importers.flatMap(_.importees) else importees
      if (selectors.nonEmpty) group
        .add(kw, ref, getSelectors(selectors), importers)
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
        val head = stats.head.tokens.head
        iter(ctx.tokenTraverser.getIndex(head), head)
      }
      val folding = settings.removeRedundantSelectors ||
        settings.selectors.contains(Newlines.fold)
      val foldMap = LinkedHashMap.empty[(String, String), ListBuffer[Importer]]
      def addToGroup(kw: String, ref: String, importers: Seq[Importer]): Unit =
        addClausesToGroup(groups(settings.group(ref)), kw, ref, importers)
      stats.foreach { s =>
        val kw = s.tokens.head.toString
        s.importers.foreach { i =>
          val ref = getRef(i)
          if (!folding) addToGroup(kw, ref, i :: Nil)
          else foldMap.getOrElseUpdate((kw, ref), ListBuffer.empty).append(i)
        }
      }
      foldMap.foreach { case ((kw, ref), v) => addToGroup(kw, ref, v.toList) }

      val seenImports = HashMap.empty[Importer, Int]
      val sb = new StringBuilder()
      @inline
      def appendIndent(): Unit = if (sb.nonEmpty) sb.append(indent)
      def appendComment(token: T) = {
        if (token.pos.startColumn != 0) appendIndent()
        sb.append(token.text).appendNL
      }
      def processOwner(
          tree: Importer,
          cntSeen: Int,
          commentAfter: Boolean,
          folded: Boolean,
      ): Option[T] = tree.parent match {
        case Some(p: ImportExportStat) =>
          val tokens = tree.tokens
          val newSeen =
            if (folded) None
            else seenImports.updateWith(tree)(x => Some(cntSeen + x.getOrElse(0)))
          if (folded || newSeen.contains(cntSeen)) {
            if (p.importers.headOption.contains(tree))
              getCommentsBefore(p.tokens.head).foreach(appendComment)
            getCommentsBefore(tokens.head).foreach(appendComment)
          }
          if (folded || !commentAfter || !newSeen.contains(tree.importees.length))
            None
          else getCommentAfter(tokens.last).orElse(
            if (!p.importers.lastOption.contains(tree)) None
            else getCommentAfter(p.tokens.last),
          )
        case _ => None
      }
      groups.foreach { group =>
        val entries = group.result()
        if (entries.nonEmpty) {
          if (sb.nonEmpty) sb.appendNL
          // sort and add empty line in all groups
          settings.sort.sortGrouping(entries).foreach { x =>
            val folded = x.owners.lengthCompare(1) > 0
            val numSelectors = x.selectors.cnt
            val needCommentAfter = x.selectors.commentAfter.isEmpty
            val commentAfter = x.owners.flatMap(owner =>
              processOwner(owner, numSelectors, needCommentAfter, folded),
            ).headOption.orElse(x.selectors.commentAfter)
            x.selectors.commentsBefore.foreach(appendComment)
            appendIndent()
            sb.append(x.stat)
            commentAfter.foreach(x => sb.append(' ').append(x.text))
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
      val headTok = x.head.tokens.head
      val lastTok = x.last.tokens.last
      (
        getCommentsBefore(headTok).headOption.getOrElse(headTok),
        getCommentAfter(lastTok).getOrElse(lastTok),
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
  private class ExpandFull(implicit ctx: RewriteCtx) extends ExpandBase {
    override protected final def addClausesToGroup(
        group: Grouping,
        kw: String,
        ref: String,
        importers: Seq[Importer],
    ): Unit = {
      val selectorsToKeep = getImporteesToKeep(importers.flatMap(_.importees))
      importers.foreach { importer =>
        def addSelectorToGroup(selector: Importee, importer: Importer): Unit =
          group.add(kw, ref, getSelector(selector), importer :: Nil)
        // if there's a wildcard, unimports and renames must come with it, cannot be expanded
        val importees =
          filterWithAllowedImportees(selectorsToKeep)(importer.importees)
        if (importees.dropWhile(notWildcardOrRename).drop(1).exists(isWildcard)) {
          val filtered = importees.filter { x =>
            val expanding = notWildcardOrRename(x)
            if (expanding) addSelectorToGroup(x, importer)
            !expanding
          }
          addSelectorsToGroup(group, kw, ref, importer :: Nil, filtered)
        } else // expand all
          importees.foreach(addSelectorToGroup(_, importer))
      }
    }
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
    ): Unit = importers
      .foreach(importer => addSelectorsToGroup(group, kw, ref, importer :: Nil))
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
      // we can't fold if there is a comment after `ref.{...}`
      val folding = importers.filter { importer =>
        val ok = importer.importees.lengthCompare(1) == 0 ||
          getCommentAfter(importer.tokens).isEmpty
        if (!ok) addSelectorsToGroup(group, kw, ref, importer :: Nil)
        ok
      }
      addSelectorsToGroup(group, kw, ref, folding)
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
          getSelectors(importer.importees, needRaw = false).pretty
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
    val ref = importer.ref.toString()
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
