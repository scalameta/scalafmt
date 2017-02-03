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
object SortImports extends Rewrite {
  // sort contributed by @djspiewak: https://gist.github.com/djspiewak/127776c2b6a9d6cd3c21a228afd4580f
  private val LCase = """([a-z].*)""".r
  private val UCase = """([A-Z].*)""".r
  private val Other = """(.+)""".r

  private def sorted(strs: Seq[String]): Seq[String] = {
    // we really want partition, but there is no ternary version of it
    val (syms, lcs, ucs) = strs.foldLeft(
      (Vector.empty[String], Vector.empty[String], Vector.empty[String])) {
      case ((syms, lcs, ucs), str) =>
        str match {
          case LCase(s) => (syms, lcs :+ s, ucs)
          case UCase(s) => (syms, lcs, ucs :+ s)
          case Other(s) => (syms :+ s, lcs, ucs)
        }
    }
    syms.sorted ++ lcs.sorted ++ ucs.sorted
  }

  override def rewrite(code: Tree, ctx: RewriteCtx): Seq[Patch] = {
    code.collect {
      case q"import ..$imports" =>
        imports.flatMap { `import` =>
          if (`import`.importees.exists(!_.is[Importee.Name])) {
            // Do nothing if an importee has for example rename
            // import a.{d, b => c}
            // I think we are safe to sort these, just want to convince myself
            // it's 100% safe first.
            Nil
          } else {
            val sortedImporteesByIndex: Map[Int, String] =
              sorted(`import`.importees.map(_.syntax)).zipWithIndex
                .map(_.swap)
                .toMap
            `import`.importees.zipWithIndex.collect {
              case (importee, i) =>
                TokenPatch.AddRight(importee.tokens.head,
                                    sortedImporteesByIndex(i))
            }
          }
        }
    }.flatten
  }
}
