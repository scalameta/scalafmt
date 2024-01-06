package org.scalafmt.rewrite

import scala.meta._
import scala.meta.tokens.Token

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.FormatToken
import org.scalafmt.internal.FormatTokens
import org.scalafmt.internal.FormatWriter

object RemoveEmptyDocstrings
    extends FormatTokensRewrite.Rule
    with FormatTokensRewrite.RuleFactory {

  import FormatTokensRewrite._

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    style.docstrings.removeEmpty

  override def create(ftoks: FormatTokens): FormatTokensRewrite.Rule = this

  override def onToken(implicit
      ft: FormatToken,
      session: Session,
      style: ScalafmtConfig
  ): Option[Replacement] = {
    val skip = ft.right.is[Token.Comment] &&
      FormatWriter.isEmptyDocstring(ft.meta.right.text)
    if (skip) Some(removeToken) else None
  }

  override def onRight(lt: Replacement, hasFormatOff: Boolean)(implicit
      ft: FormatToken,
      session: Session,
      style: ScalafmtConfig
  ): Option[(Replacement, Replacement)] = None

}
