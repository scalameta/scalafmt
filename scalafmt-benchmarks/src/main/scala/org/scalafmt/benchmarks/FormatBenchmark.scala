package org.scalafmt.benchmarks

import org.scalafmt.Scalafmt
import org.scalafmt.config.{RewriteSettings, ScalafmtConfig}
import org.scalafmt.rewrite.{Imports, RedundantBraces}

trait FormatBenchmark {
  // `SortImports` is a deprecated shim that throws NotImplementedError; the
  // modern equivalent is the unified `Imports` rule with `imports.sort`.
  def formatRewrite(
      code: String,
      base: ScalafmtConfig = ScalafmtConfig.default,
  ): String = Scalafmt.formatCode(
    code,
    baseStyle = base.copy(rewrite =
      RewriteSettings(
        rules = Seq(Imports, RedundantBraces),
        imports = Imports.Settings(sort = Imports.Sort.ascii),
      ),
    ),
  ).get
}
