package org.scalafmt.rewrite

import java.util.regex.Pattern

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.meta._
import scala.meta.tokens.Token

import org.scalafmt.config.ReaderUtil
import org.scalafmt.config.RewriteSettings
import org.scalafmt.util.TokenOps
import org.scalafmt.util.Whitespace

import metaconfig._

object Imports extends RewriteFactory {

  case class Settings(
      sort: Sort = Sort.none,
      expand: Boolean = false,
      contiguousGroups: ContiguousGroups = ContiguousGroups.only,
      groups: Seq[Seq[String]] = Nil
  ) {
    private lazy val regex = groups.map(_.map(Pattern.compile))

    def nonEmpty: Boolean = sort.ne(Sort.none) || expand || groups.nonEmpty

    def group(str: String): Int = {
      val index = regex.indexWhere(_.exists(_.matcher(str).matches()))
      if (index < 0) regex.length else index
    }
  }

  object Settings {
    implicit val surface: generic.Surface[Settings] = generic.deriveSurface
    implicit val codec: ConfCodecEx[Settings] =
      generic.deriveCodecEx(new Settings).noTypos
  }

  sealed abstract class ContiguousGroups
  object ContiguousGroups {
    case object no extends ContiguousGroups
    case object only extends ContiguousGroups

    implicit val codec: ConfCodecEx[ContiguousGroups] =
      ReaderUtil.oneOf(only, no)
  }

  override def hasChanged(v1: RewriteSettings, v2: RewriteSettings): Boolean =
    v1.imports ne v2.imports

  override def create(implicit ctx: RewriteCtx): RewriteSession = {
    val settings = ctx.style.rewrite.imports
    if (settings.expand)
      new ExpandFull
    else if (settings.groups.nonEmpty)
      new ExpandPart
    else if (settings.sort ne Sort.none)
      new ExpandNone
    else
      new RewriteSession.None()
  }

  private val allImportRules: Set[Rewrite] =
    Set(Imports, ExpandImportSelectors, SortImports, AsciiSortImports)

  def validateImports(obj: RewriteSettings): Configured[RewriteSettings] = {
    val (importRules, nonImportRules) =
      obj.rules.partition(allImportRules.contains)
    val ok = importRules.isEmpty ||
      importRules.lengthCompare(1) == 0 && importRules.head.eq(Imports)
    if (ok) Configured.Ok(obj)
    else {
      val sortOriginal = importRules.contains(SortImports)
      val sortAscii = importRules.contains(AsciiSortImports)
      if (sortAscii && sortOriginal) {
        val err = "Incompatible rewrites: SortImports and AsciiSortImports"
        Configured.NotOk(ConfError.message(err))
      } else {
        val expand =
          obj.imports.expand || importRules.contains(ExpandImportSelectors)
        val sort =
          if (sortAscii) Imports.Sort.ascii
          else if (sortOriginal) Imports.Sort.original
          else obj.imports.sort
        val validated = obj.copy(
          rules = nonImportRules :+ Imports,
          imports = obj.imports.copy(expand = expand, sort = sort)
        )
        Configured.Ok(validated)
      }
    }
  }

  case class Selectors(
      pretty: String,
      raw: String,
      cnt: Int,
      commentsBefore: Seq[Token] = Seq.empty,
      commentAfter: Option[Token] = None
  )

  private class Grouping(
      buffer: ListBuffer[GroupingEntry] = new ListBuffer[GroupingEntry],
      stats: HashSet[String] = new HashSet[String]
  ) {
    def add(
        kw: String,
        ref: String,
        selectors: Selectors,
        owner: Importer
    ): Unit = {
      val stat = s"$kw $ref.${selectors.pretty}"
      if (stats.add(stat))
        buffer += GroupingEntry(stat, ref, selectors, owner)
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
      owner: Importer
  ) {
    lazy val labels = ref.split('.') :+ selectors.raw
  }

  sealed abstract class Sort {
    def sortSelector(buf: Seq[Importee]): Seq[(Importee, String)]
    def sortGrouping(buf: Seq[GroupingEntry]): Iterable[GroupingEntry]

    protected final def selectorToTuple(tree: Importee): (Importee, String) =
      (tree, tree.toString)
  }

  private object Sort {

    implicit val reader: ConfCodecEx[Sort] =
      ReaderUtil.oneOf[Sort](none, ascii, original, scalastyle)

    case object none extends Sort {
      def sortSelector(buf: Seq[Importee]): Seq[(Importee, String)] =
        buf.map(selectorToTuple)

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
              val cmp = groupingOrdering.compare(xarr(i), yarr(i))
              if (cmp != 0) cmp else iter(i + 1)
            }
          iter(0)
        }
      }

      def sortSelector(buf: Seq[Importee]): Seq[(Importee, String)] = {
        val (wildcard, other) = buf.partition(_.is[Importee.Wildcard])
        other.map(selectorToTuple).sortBy(_._2)(selectorOrdering) ++
          wildcard.headOption.map(selectorToTuple)
      }

      def sortGrouping(buf: Seq[GroupingEntry]): Iterable[GroupingEntry] =
        buf.view.sorted(GroupingEntryOrdering)
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
        override protected def compareTail(x: String, y: String): Int =
          x.compare(y)
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
        override protected def compareTail(x: String, y: String): Int =
          x.compareToIgnoreCase(y)
      }

      private object SelectorOrdering extends FirstLetterRankOrdering {
        protected def getRank(ch: Char): Int = Character.getType(ch) match {
          case Character.UPPERCASE_LETTER => 2
          case Character.LOWERCASE_LETTER => 1
          case _ => 0
        }
        override protected def compareTail(x: String, y: String): Int =
          x.compareToIgnoreCase(y)
      }
    }
  }

  private abstract class Base(implicit ctx: RewriteCtx) extends RewriteSession {

    protected val settings = ctx.style.rewrite.imports

    protected def processImports(stats: Seq[Seq[ImportExportStat]]): Unit

    override final def rewrite(tree: Tree): Unit = tree match {
      case t: Source => processStats(t.stats)
      case t: Pkg => processStats(t.stats)
      case t: Template => processStats(t.stats)
      case t: ImportExportStat if t.parent.isEmpty => processStats(Seq(t))
      case _ =>
    }

    protected final def getWildcard(
        importees: collection.Seq[Importee]
    ): Option[String] =
      importees.collectFirst { case t: Importee.Wildcard => t.toString() }

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
        needRaw: Boolean
    ): Selectors = {
      val selectorString = selector.toString()
      val (commentsBefore, commentAfter) = getCommentsAround(selector)
      if (mustUseBraces(selector)) {
        val sb = new StringBuilder
        sb.append('{')
        commentsBefore.foreach { x =>
          sb.append('\n')
          if (x.pos.startColumn != 0) sb.append(' ')
          sb.append(x.text).append('\n')
        }
        sb.append(selectorString)
        commentAfter.foreach(x => sb.append(' ').append(x.text).append('\n'))
        val pretty = sb.append('}').result()
        val hadComments = commentsBefore.nonEmpty || commentAfter.isDefined
        Selectors(
          pretty,
          if (needRaw && hadComments) s"{$selector}" else pretty,
          1
        )
      } else {
        Selectors(
          selectorString,
          selectorString,
          1,
          commentsBefore,
          commentAfter
        )
      }
    }

    protected final def getSelectors(
        selectors: Seq[Importee],
        needRaw: Boolean
    ): Selectors = {
      val selectorCount = selectors.length
      if (selectorCount == 1) getSelector(selectors.head, needRaw)
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
            if (!sb.lastOption.contains('\n')) sb.append('\n')
            sb.append(x).append('\n')
          }
          sb.append(selectorString)
          if (index != selectorCount) sb.append(',')
          commentAfter.foreach { x =>
            hadComments = true
            sb.append(' ').append(x).append('\n')
          }
        }
        val pretty = sb.append('}').result()
        Selectors(
          pretty,
          if (hadComments && needRaw) tuples.map(_._2).mkString("{", ",", "}")
          else pretty,
          selectorCount
        )
      }
    }

    private final def mustUseBraces(tree: Importee): Boolean =
      (tree match {
        case t: Importee.Rename => Some(t.name)
        case t: Importee.Unimport => Some(t.name)
        case _ => None
      }).exists { x =>
        val tokenAfter = ctx.tokenTraverser.nextNonTrivialToken(x.tokens.last)
        // in scala3, `as` doesn't need braces
        tokenAfter.exists(_.is[Token.RightArrow])
      }

    protected final def getCommentsAround(
        tree: Tree
    ): (Seq[Token], Option[Token]) = {
      val tokens = tree.tokens
      val beg = getCommentsBefore(tokens.head)
      val end = getCommentAfter(tokens.last)
      beg -> end
    }

    protected final def getCommentsBefore(tok: Token): Seq[Token] = {
      var hadLf = false
      val slc = new ListBuffer[Token]
      ctx.tokenTraverser.findAtOrBefore(ctx.getIndex(tok) - 1) {
        case _: Token.LF =>
          if (hadLf) Some(true) else { hadLf = true; None }
        case t: Token.Comment if TokenOps.isSingleLineComment(t) =>
          slc.prepend(t); hadLf = false; None
        case Whitespace() => None
        case _ => if (!hadLf && !slc.isEmpty) slc.remove(0); Some(false)
      }
      slc.result()
    }

    protected final def getCommentAfter(tok: Token): Option[Token] =
      ctx.tokenTraverser.findAtOrAfter(ctx.getIndex(tok) + 1) {
        case _: Token.LF => Some(false)
        case t: Token.Comment if TokenOps.isSingleLineComment(t) =>
          Some(true)
        case Whitespace() | _: Token.Comma => None
        case _ => Some(false)
      }
  }

  private abstract class ExpandBase(implicit ctx: RewriteCtx) extends Base {

    protected def addClauseToGroup(
        group: Grouping,
        kw: String,
        ref: String,
        importer: Importer
    ): Unit

    protected val groups = Array.fill(settings.groups.length + 1)(new Grouping)

    protected def addToGroup(
        group: Grouping,
        kw: String,
        ref: String,
        selector: Importee,
        importer: Importer
    ): Unit =
      group.add(kw, ref, getSelector(selector, true), importer)

    protected def addToGroup(
        group: Grouping,
        kw: String,
        ref: String,
        selectors: Seq[Importee],
        importer: Importer
    ): Unit =
      if (selectors.nonEmpty)
        group.add(kw, ref, getSelectors(selectors, true), importer)

    private def processImports(stats: Iterable[ImportExportStat]): String = {
      stats.foreach { x =>
        val kw = x.tokens.head.toString
        x.importers.foreach { importer =>
          val ref = importer.ref.toString
          val grp = groups(settings.group(ref + "."))
          addClauseToGroup(grp, kw, ref, importer)
        }
      }
      val seenImports = new HashMap[Importer, Int]
      val sb = new StringBuilder()
      groups.foreach { group =>
        val entries = group.result()
        if (entries.nonEmpty) {
          if (sb.nonEmpty) sb.append('\n')
          // sort and add empty line in all groups
          settings.sort.sortGrouping(entries).foreach { x =>
            val (commentBefore, commentAfter) = x.owner.parent match {
              case Some(p: ImportExportStat) =>
                val oldImporteeCount = seenImports.getOrElse(x.owner, 0)
                val newImporteeCount = oldImporteeCount + x.selectors.cnt
                seenImports.put(x.owner, newImporteeCount)
                val headComments =
                  if (oldImporteeCount != 0) Seq.empty
                  else {
                    val headImportComments =
                      if (p.importers.headOption.contains(x.owner))
                        getCommentsBefore(p.tokens.head)
                      else Seq.empty
                    headImportComments ++ getCommentsBefore(x.owner.tokens.head)
                  }
                val tailComments = x.selectors.commentAfter.orElse {
                  if (newImporteeCount != x.owner.importees.length) None
                  else if (p.importers.lastOption.contains(x.owner))
                    getCommentAfter(p.tokens.last)
                  else
                    getCommentAfter(x.owner.tokens.last)
                }
                (headComments ++ x.selectors.commentsBefore, tailComments)
              case _ => (Seq.empty, None)
            }
            commentBefore.foreach { x =>
              if (x.pos.startColumn != 0) sb.append(' ')
              sb.append(x.text).append('\n')
            }
            sb.append(x.stat)
            commentAfter.foreach(x => sb.append(' ').append(x.text))
            sb.append('\n')
          }
        }
      }
      sb.result()
    }

    override protected def processImports(
        stats: Seq[Seq[ImportExportStat]]
    ): Unit =
      if (settings.groups.isEmpty && settings.sort.eq(Sort.none))
        processEachLine(stats)
      else
        processAllGroups(stats)

    private def getTokenRange(x: Seq[ImportExportStat]): (Token, Token) = {
      val headTok = x.head.tokens.head
      val lastTok = x.last.tokens.last
      (
        getCommentsBefore(headTok).headOption.getOrElse(headTok),
        getCommentAfter(lastTok).getOrElse(lastTok)
      )
    }

    private def processEachLine(stats: Seq[Seq[ImportExportStat]]): Unit =
      stats.flatten.foreach { stat =>
        val group = Seq(stat)
        val importString = processImports(group)
        processTokenRanges(importString, getTokenRange(group))
      }

    private def processAllGroups(stats: Seq[Seq[ImportExportStat]]): Unit = {
      val tokenRanges = stats.map(getTokenRange)
      val importString = processImports(stats.flatten)
      processTokenRanges(importString, tokenRanges: _*)
    }

    private def processTokenRanges(
        importString: String,
        tokenRanges: (Token, Token)*
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
        tokens
          .slice(if (beg eq first) begIdx + 1 else begIdx, endIdx + 1)
          .foreach(patchBuilder += TokenPatch.Remove(_))
        ctx.removeLFToAvoidEmptyLine(begIdx, endIdx)
      }
      ctx.addPatchSet(patchBuilder.result(): _*)
    }
  }

  /** convert import a.{c, b}, d.{f, e} to import a.b import a.c
    *
    * import d.e import d.f
    */
  private class ExpandFull(implicit ctx: RewriteCtx) extends ExpandBase {
    override protected final def addClauseToGroup(
        group: Grouping,
        kw: String,
        ref: String,
        importer: Importer
    ): Unit = {
      // if there's a wildcard, unimports and renames must come with it, cannot be expanded
      val importees = importer.importees
      val wildcard = getWildcard(importees)
      val filtered = importees.filter { x =>
        val buffering = wildcard.isDefined &&
          (x.is[Importee.Unimport] || x.is[Importee.Rename]
            || x.is[Importee.Wildcard])
        if (!buffering) addToGroup(group, kw, ref, x, importer)
        buffering
      }
      addToGroup(group, kw, ref, filtered, importer)
    }
  }

  /** convert import a.{c, b}, d.{f, e} to import a.{b, c}
    *
    * import d.{e, f}
    */
  private class ExpandPart(implicit ctx: RewriteCtx) extends ExpandBase {
    override protected final def addClauseToGroup(
        group: Grouping,
        kw: String,
        ref: String,
        importer: Importer
    ): Unit =
      addToGroup(group, kw, ref, importer.importees, importer)
  }

  /** convert import a.{c, b}, d.{f, e} to import a.{b, c}, d.{e, f}
    */
  private class ExpandNone(implicit ctx: RewriteCtx) extends Base {
    override protected def processImports(
        stats: Seq[Seq[ImportExportStat]]
    ): Unit = {
      stats.flatten.foreach { t =>
        val patchBuilder = Seq.newBuilder[TokenPatch]
        t.importers.foreach { importer =>
          val ref = importer.ref.toString
          val selectors = getSelectors(importer.importees, false).pretty
          val replacement = s"$ref.$selectors"
          val tokens: Iterator[Token] = importer.tokens.iterator
          // replace the first token
          patchBuilder += TokenPatch.Replace(tokens.next(), replacement)
          // remove all tokens except first
          tokens.foreach(patchBuilder += TokenPatch.Remove(_))
        }
        ctx.addPatchSet(patchBuilder.result(): _*)
      }
    }
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
