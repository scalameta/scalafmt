package org.scalafmt.rewrite

import scala.meta.Importee
import scala.meta.Tree
import scala.meta._

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

  override def rewrite(code: Tree, ctx: RewriteCtx): Seq[Patch] = {
    import ctx.dialect
    code.collect {
      case Import(imports) =>
        imports.flatMap { `import` =>
          if (`import`.importees.exists(!_.is[Importee.Name])) {
            // Do nothing if an importee has for example rename
            // import a.{d, b => c}
            // I think we are safe to sort these, just want to convince myself
            // it's 100% safe first.
            Nil
          } else {
            val sortedImportees: Seq[String] =
              sortImportees(`import`.importees).map(_.tokens.mkString)
            `import`.importees.zip(sortedImportees).map {
              case (oldImp, newImp) =>
                TokenPatch.AddRight(oldImp.tokens.head, newImp)
            }
          }
        }
    }.flatten
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
