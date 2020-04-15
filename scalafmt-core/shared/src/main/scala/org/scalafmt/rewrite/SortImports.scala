package org.scalafmt.rewrite

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
sealed abstract class SortImports(implicit ctx: RewriteCtx)
    extends RewriteSession {

  import ctx.dialect

  /**
    * The sorting scheme to use when sorting the imports
    */
  protected def sorted(str: Seq[String]): IndexedSeq[String]

  override def rewrite(tree: Tree): Unit =
    tree match {
      case Import(imports) =>
        val builder = Seq.newBuilder[TokenPatch]
        imports.foreach { `import` =>
          val importees = `import`.importees
          if (!importees.exists(!_.is[Importee.Name])) {
            // Do nothing if an importee has for example rename
            // import a.{d, b => c}
            // I think we are safe to sort these, just want to convince myself
            // it's 100% safe first.
            val sortedImportees = sorted(importees.map(_.tokens.mkString))
            var i = 0
            importees.foreach { importee =>
              builder +=
                TokenPatch.AddRight(importee.tokens.head, sortedImportees(i))
              i += 1
            }
          }
        }
        ctx.addPatchSet(builder.result(): _*)

      case _ =>
    }
}

/**
  * Sort imports with symbols at the beginning, followed by lowercase and
  * finally uppercase
  */
object SortImports extends Rewrite {

  // sort contributed by @djspiewak: https://gist.github.com/djspiewak/127776c2b6a9d6cd3c21a228afd4580f
  private val LCase = """([a-z].*)""".r
  private val UCase = """([A-Z].*)""".r
  private val Other = """(.+)""".r

  override def create(implicit ctx: RewriteCtx): RewriteSession =
    new Impl

  private class Impl(implicit ctx: RewriteCtx) extends SortImports {
    override def sorted(strs: Seq[String]): IndexedSeq[String] = {
      // we really want partition, but there is no ternary version of it
      val (syms, lcs, ucs) = strs.foldLeft(
        (Vector.empty[String], Vector.empty[String], Vector.empty[String])
      ) {
        case ((syms, lcs, ucs), str) =>
          str match {
            case LCase(s) => (syms, lcs :+ s, ucs)
            case UCase(s) => (syms, lcs, ucs :+ s)
            case Other(s) => (syms :+ s, lcs, ucs)
          }
      }
      syms.sorted ++ lcs.sorted ++ ucs.sorted
    }
  }
}

/**
  * Sort imports using the traditional ASCII sorting
  *
  * See: http://support.ecisolutions.com/doc-ddms/help/reportsmenu/ascii_sort_order_chart.htm
  */
object AsciiSortImports extends Rewrite {

  override def create(implicit ctx: RewriteCtx): RewriteSession =
    new Impl

  private class Impl(implicit ctx: RewriteCtx) extends SortImports {
    override def sorted(strs: Seq[String]): IndexedSeq[String] =
      strs.sorted.toIndexedSeq
  }
}
