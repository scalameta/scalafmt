package org.scalafmt.rewrite

import scala.collection.mutable
import scala.meta._
import scala.meta.tokens.Token._
import scala.util.matching.Regex

import org.scalafmt.util.TreeOps._

/**
  * Sort importees with symbols at the beginning, followed by lowercase and
  * finally uppercase
  */
case object SortImports extends SortImports with LowercaseSortImportees

/**
  * Sort importees using traditional ASCII sorting
  *
  * See: http://support.ecisolutions.com/doc-ddms/help/reportsmenu/ascii_sort_order_chart.htm
  */
case object AsciiSortImports extends SortImports with AsciiSortImportees

/**
  * Sorts imports into groups using RewriteSettings.importGroups
  *
  * Also sort importees inside curly braces
  *
  * For example, with an importGroups of `[a, b]`:
  *
  * import b.d
  * import a.{c, b}
  * import b.f
  *
  * into
  *
  * import a.{b, c}
  *
  * import b.d
  * import b.f
  */
sealed trait SortImports extends Rewrite with SortImportees {

  import TokenPatch._
  import Matcher._

  override def rewrite(code: Tree, ctx: RewriteCtx): Seq[Patch] = {

    import ctx.dialect
    type Acc[F[_]] = F[(Import, Importer)]
    val (fAcc, acc): (mutable.ListBuffer[Acc[List]], Acc[mutable.ListBuffer]) =
      (mutable.ListBuffer(), mutable.ListBuffer())

    def addIfNotEmpty =
      if (acc.isEmpty) ()
      else {
        fAcc += acc.toList
        acc.clear
      }

    // DESNOTE(2017-09-09, pjrt): Boy, mutability is complex
    // This is essentially trying to replicate a foldLeft but due to the
    // mutable Apis of scalameta, we can't currently just use a foldLeft.
    // The idea here is to collect all the import groups along with their
    // location.
    code.traverse {
      case stat @ Import(imports) =>
        acc ++= imports.map(stat -> _)
      case stat if isPartOfImport(stat) =>
        ()
      case _ =>
        addIfNotEmpty
    }

    // We need to make another call to addIfNotEmpty since the last stat
    // may be an import (which would mean we never ad the last group to fAcc.
    addIfNotEmpty

    fAcc.toList.flatMap { d =>
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
    val importees = imp.importees
    val importeesStr =
      if (importees.isEmpty) ""
      else if (importees.length == 1)
        s".${importees.head.toString}"
      else s".{ ${importees.mkString(", ")} }"

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

private sealed trait Matcher {

  def index: Int
}

private object Matcher {

  final case class ExactFind(index: Int) extends Matcher
  final case class GeneralFind(index: Int) extends Matcher
}
