package org.scalafmt.rewrite

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal._

object RemoveSemicolons extends Rewrite with FormatTokensRewrite.RuleFactory {

  override def enabled(implicit style: ScalafmtConfig): Boolean = false

  override def create(implicit ftoks: FormatTokens): FormatTokensRewrite.Rule =
    ???

}
