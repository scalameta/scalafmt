package org.scalafmt.benchmarks

import org.scalafmt.Scalafmt
import org.scalafmt.config.RewriteSettings
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.rewrite.RedundantBraces
import org.scalafmt.rewrite.SortImports

trait FormatBenchmark {
  def formatRewrite(code: String): String = Scalafmt.formatCode(
    code,
    baseStyle = ScalafmtConfig.default
      .copy(rewrite = RewriteSettings(rules = Seq(SortImports, RedundantBraces))),
  ).get
}
