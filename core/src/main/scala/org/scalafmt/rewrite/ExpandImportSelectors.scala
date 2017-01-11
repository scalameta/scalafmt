package org.scalafmt.rewrite

import scala.meta.{Importee, Tree, _}

object ExpandImportSelectors extends Rewrite {

  override def rewrite(code: Tree, ctx: RewriteCtx): Seq[Patch] = {
    code.collect {
      case q"import ..$imports" =>
        val groupedPatches: Map[String, Seq[Patch]] = imports
          .map { `import` =>
            val expandedImport = `import`.collect {
              case Importer(path, importees) =>
                importees.collect {
                  case importee: Importee =>
                    if (importee.toString.contains("=>"))
                      s"import $path.{$importee}"
                    else
                      s"import $path.$importee"
                }
            }.flatten
            val patchStr = expandedImport.mkString("\n")
            Patch(`import`.parent.get.tokens.head,
                  `import`.parent.get.tokens.last,
                  patchStr)
          }
          .groupBy(p => p.from.toString + p.to.toString)

        val mergedPatches: Seq[Patch] = groupedPatches.values.map { patches =>
          patches.reduce((p1: Patch, p2: Patch) =>
            Patch(p1.from, p1.to, p1.replace + "\n" + p2.replace))
        }.toSeq

        mergedPatches
    }.flatten
  }
}
