package org.scalafmt.rewrite

import org.scalafmt.config.RewriteSettings

import metaconfig._

object Imports {

  private val allImportRules: Set[Rewrite] =
    Set(ExpandImportSelectors, SortImports, AsciiSortImports)

  def validateImports(obj: RewriteSettings): Configured[RewriteSettings] = {
    val importRules = obj.rules.filter(allImportRules.contains)
    if (importRules.lengthCompare(1) > 0) {
      val msg = importRules.mkString("Incompatible rewrites: ", ", ", "")
      Configured.NotOk(ConfError.message(msg))
    } else Configured.Ok(obj)
  }

}
