package org.scalafmt.rewrite

import scala.collection.mutable
import scala.meta._

object ExpandImportSelectors extends Rewrite {

  override def create(implicit ctx: RewriteCtx): RewriteSession =
    new ExpandImportSelectors

  private case class Group(
      patches: mutable.Builder[TokenPatch, Seq[TokenPatch]] = Seq.newBuilder,
      imports: mutable.ArrayBuffer[String] = new mutable.ArrayBuffer
  )

}

class ExpandImportSelectors(implicit ctx: RewriteCtx) extends RewriteSession {

  import ExpandImportSelectors.Group

  override def rewrite(tree: Tree): Unit =
    tree match {
      case q"import ..$imports" =>
        val groupedPatches = mutable.Map.empty[Token, Group]
        imports.foreach { `import` =>
          val parentTokens = `import`.parent.get.tokens
          val group =
            groupedPatches.getOrElseUpdate(parentTokens.head, new Group)
          parentTokens.tail.foreach(x => group.patches += TokenPatch.Remove(x))

          `import`.traverse {
            case importer @ Importer(path, importees) =>
              val hasRenamesOrUnimports = importees.exists(importee =>
                importee.is[Importee.Rename] || importee.is[Importee.Unimport]
              )

              val hasWildcards = importees.exists(_.is[Importee.Wildcard])

              if (hasWildcards && hasRenamesOrUnimports)
                group.imports += s"import ${importer.syntax}"
              else
                importees.foreach { importee =>
                  val importString = importee.toString
                  val replacement =
                    if (importString.contains("=>"))
                      s"import $path.{$importString}"
                    else
                      s"import $path.$importString"
                  group.imports += replacement
                }
          }
        }

        groupedPatches.foreach {
          case (tok, group) =>
            group.patches +=
              TokenPatch.AddRight(tok, group.imports.mkString("\n"))
            ctx.addPatchSet(group.patches.result(): _*)
        }

      case _ =>
    }

}
