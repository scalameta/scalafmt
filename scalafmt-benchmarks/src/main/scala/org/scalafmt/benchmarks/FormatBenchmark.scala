package org.scalafmt.benchmarks

import org.scalafmt.Scalafmt
import org.scalafmt.config.{RewriteSettings, ScalafmtConfig}
import org.scalafmt.rewrite.{RedundantBraces, SortImports}

trait FormatBenchmark {
  def formatRewrite(code: String): String = {
    Scalafmt
      .formatCode(
        code,
        style = ScalafmtConfig.default.copy(
          rewrite = RewriteSettings(
            rules = Seq(SortImports, RedundantBraces)
          )
        )
      )
      .get
  }
}
