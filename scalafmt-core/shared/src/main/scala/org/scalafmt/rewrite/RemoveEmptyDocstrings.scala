package org.scalafmt.rewrite

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal._

import scala.meta._
import scala.meta.tokens.{Token => T}

object RemoveEmptyDocstrings
    extends FormatTokensRewrite.Rule with FormatTokensRewrite.RuleFactory {

  import FormatTokensRewrite._

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    style.docstrings.removeEmpty

  override def create(implicit ftoks: FormatTokens): FormatTokensRewrite.Rule =
    this

  override def onToken(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Option[Replacement] = {
    val skip = ft.right.is[T.Comment] &&
      FormatWriter.isEmptyDocstring(ft.meta.right.text)
    if (skip) Some(removeToken) else None
  }

  override def onRight(lt: Replacement, hasFormatOff: Boolean)(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Option[(Replacement, Replacement)] = None

}
