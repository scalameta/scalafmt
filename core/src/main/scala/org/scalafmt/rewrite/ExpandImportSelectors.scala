package org.scalafmt.rewrite

import scala.meta.{Importee, Tree, _}

object ExpandImportSelectors extends Rewrite {

  override def rewrite(code: Tree, ctx: RewriteCtx): Seq[Patch] = {
    val builder = Seq.newBuilder[Patch]

    code.traverse {
      case q"import ..$imports" =>
        val groupedPatches: Map[Token, Seq[TokenPatch]] = imports
          .map { `import` =>
            val expandedImport = `import`.collect {
              case importer @ Importer(path, importees) =>
                val hasRenames = importees.collect {
                  case importee: Importee.Rename =>
                    importee
                }.nonEmpty

                val hasUnimports = importees.collect {
                  case importee: Importee.Unimport =>
                    importee
                }.nonEmpty

                val hasWildcards = importees.collect {
                  case importee: Importee.Wildcard =>
                    importee
                }.nonEmpty

                if (hasWildcards && (hasRenames || hasUnimports))
                  Seq(s"import ${importer.syntax}")
                else
                  importees.collect {
                    case importee: Importee =>
                      if (importee.toString.contains("=>"))
                        s"import $path.{$importee}"
                      else
                        s"import $path.$importee"
                    case importee @ _ => importee
                  }
            }.flatten
            val patchStr = expandedImport.mkString("\n")

            //TODO move up this side effect
            `import`.parent.get.tokens.tail.foreach { tok =>
              builder += TokenPatch.Remove(tok)
            }

            TokenPatch.AddRight(`import`.parent.get.tokens.head, patchStr)
          }
          .groupBy(_.tok)

        val mergedPatches: Seq[Patch] = groupedPatches.values.map { patches =>
          patches.reduce((p1: TokenPatch, p2: TokenPatch) =>
            TokenPatch.AddRight(p1.tok, p1.newTok + "\n" + p2.newTok))
        }.toSeq

        builder ++= mergedPatches
    }

    builder.result()
  }
}
