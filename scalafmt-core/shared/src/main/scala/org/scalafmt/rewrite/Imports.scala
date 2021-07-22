package org.scalafmt.rewrite

import scala.annotation.tailrec
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.meta._
import scala.meta.tokens.Token

import org.scalafmt.config.ReaderUtil
import org.scalafmt.config.RewriteSettings

import metaconfig._

object Imports {

  case class Settings(
      sort: Sort = Sort.none,
      expand: Boolean = false,
      groups: Seq[Seq[String]] = Nil
  ) {
    def nonEmpty: Boolean = sort.ne(Sort.none) || expand || groups.nonEmpty
  }

  object Settings {
    implicit val surface: generic.Surface[Settings] = generic.deriveSurface
    implicit val codec: ConfCodecEx[Settings] =
      generic.deriveCodecEx(new Settings).noTypos
  }

  private val allImportRules: Set[Rewrite] =
    Set(ExpandImportSelectors, SortImports, AsciiSortImports)

  def validateImports(obj: RewriteSettings): Configured[RewriteSettings] = {
    val importRules = obj.rules.filter(allImportRules.contains)
    if (importRules.lengthCompare(1) > 0) {
      val msg = importRules.mkString("Incompatible rewrites: ", ", ", "")
      Configured.NotOk(ConfError.message(msg))
    } else Configured.Ok(obj)
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
      finally buffer.clear()
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

}
