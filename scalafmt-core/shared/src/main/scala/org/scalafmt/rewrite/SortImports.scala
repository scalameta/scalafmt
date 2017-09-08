package org.scalafmt.rewrite

import scala.util.matching.Regex
import scala.meta._
import scala.meta.tokens.Token._

/**
  * Sorts imports inside curly braces.
  *
  * For example
  *
  * import a.{c, b}
  *
  * into
  *
  * import a.{b, c}
  */
sealed trait SortImports extends Rewrite with SortImportees {

  import TokenPatch._
  import Matcher._

  override def rewrite(code: Tree, ctx: RewriteCtx): Seq[Patch] = {

    import ctx.dialect
    type Acc = Seq[(Import, Importer)]
    val base: (Seq[Acc], Acc) = (Seq.empty, Seq.empty)
    val (rest, lastOne) =
      code match {
        // DESNOTE(2017-09-07, pjrt): Special case where the only statement is
        // an import statement. Very odd and likely never found in real Scala
        // code.
        case imp @ Import(imports) =>
          (Nil, imports.map(imp -> _))
        case otherwise =>
          otherwise.children.foldLeft(base) {
            case ((fAcc, acc), stat @ Import(imports)) =>
              (fAcc, acc ++ imports.map(stat -> _))
            case ((fAcc, curr), d) =>
              (fAcc :+ curr, Nil)

          }
      }

    (rest :+ lastOne).flatMap { d =>
      val (toRemove, decopImps) = d.unzip
      if (decopImps.isEmpty) Nil
      else {
        val rms = toRemove.flatMap(_.tokens.map(Remove(_)))
        val sortedStr =
          replaceWithSorted(decopImps, ctx.style.rewrite.importGroups)
        rms :+ AddRight(rms.head.tok, sortedStr)
      }
    }
  }

  // DESNOTE(2017-09-07, pjrt): Given a set of import statements (flatten) and a
  // list of import groups: sort them vertically, split them in groups and
  // sort their importees.
  private def replaceWithSorted(
      decopImps: Seq[Importer],
      importGroups: List[String]): String = {

    def startsWith(imp: Importer, p: String) = {
      val ps = p.split(',')
      val str = imp.toString
      ps.exists(str.startsWith(_))
    }
    val groups: Seq[Seq[Importer]] = decopImps
      .map { imp =>
        val group = importGroups.zipWithIndex.foldLeft(None: Option[Matcher]) {
          case (None, ("*", i)) => Some(GeneralFind(i))
          case (None, (p, i)) if (startsWith(imp, p)) => Some(ExactFind(i))
          case (Some(GeneralFind(_)), (p, i)) if (startsWith(imp, p)) =>
            Some(ExactFind(i))
          case (acc, _) => acc
        }
        // DESNOTE(2017-09-06, pjrt): If we don't find any pattern that
        // matches, just place it at the end. Only way this can happen is
        // if the user doesn't place a `*` anywhere in the pattern list.
        // If no patterns are given in the setting, they are all sorted into
        // the same group.
        val sortedImps =
          imp.copy(importees = sortImportees(imp.importees).toList)
        sortedImps -> group.fold(importGroups.length)(_.index)
      }
      .groupBy(_._2)
      .map { case (i, imps) => i -> imps.map(_._1) }
      .toSeq
      .sortBy(_._1)
      .map(_._2)

    groups
      .map(is => is.map(importerToString).sorted.mkString("\n"))
      .mkString("\n\n")
  }

  // DESNOTE(2017-09-08, pjrt): There seems to be a bug in scalameta where the
  // ` (back tick) character isn't printed if you just do `Impoter.toString`
  // or `.syntax`. So we get around it by printing importees directly.
  private def importerToString(imp: Importer): String = {
    val importeesStr =
      if (imp.importees.isEmpty) ""
      else s".{ ${imp.importees.mkString(", ")} }"

    s"import ${imp.ref}$importeesStr"
  }
}

trait SortImportees {

  /**
    * The sorting scheme to use when sorting importees
    */
  def sortImportees(imps: Seq[Importee]): Seq[Importee]
}

trait LowercaseSortImportees extends SortImportees {

  // sort contributed by @djspiewak: https://gist.github.com/djspiewak/127776c2b6a9d6cd3c21a228afd4580f
  private val LCase = """([a-z].*)""".r
  private val UCase = """([A-Z].*)""".r
  private val Other = """(.+)""".r

  override def sortImportees(strs: Seq[Importee]): Seq[Importee] = {
    // we really want partition, but there is no ternary version of it
    val (syms, lcs, ucs) =
      strs.foldLeft(
        (
          Vector.empty[Importee],
          Vector.empty[Importee],
          Vector.empty[Importee])) {
        case ((syms, lcs, ucs), imp) =>
          val str = imp.tokens.mkString
          str match {
            case LCase(_) => (syms, lcs :+ imp, ucs)
            case UCase(_) => (syms, lcs, ucs :+ imp)
            case Other(_) => (syms :+ imp, lcs, ucs)
          }
      }
    syms.sorted ++ lcs.sorted ++ ucs.sorted
  }
}

trait AsciiSortImportees extends SortImportees {

  override def sortImportees(strs: Seq[Importee]): Seq[Importee] = strs.sorted
}

/**
  * Sort imports with symbols at the beginning, followed by lowercase and
  * finally uppercase
  */
case object SortImports extends SortImports with LowercaseSortImportees

/**
  * Sort imports using the traditional ASCII sorting
  *
  * See: http://support.ecisolutions.com/doc-ddms/help/reportsmenu/ascii_sort_order_chart.htm
  */
case object AsciiSortImports extends SortImports with AsciiSortImportees

private sealed trait Matcher {

  def index: Int
}

private object Matcher {

  final case class ExactFind(index: Int) extends Matcher
  final case class GeneralFind(index: Int) extends Matcher
}
