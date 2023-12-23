package org.scalafmt.rewrite

import scala.meta.Importee
import scala.meta.tokens.Token

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.FormatToken
import org.scalafmt.internal.FormatTokens

object ConvertImportWildcardToStar extends FormatTokensRewrite.RuleFactory {

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    style.rewrite.scala3.convertToNewSyntax || style.rewrite.scala3.convertImportWildcardsToStar

  override def create(ftoks: FormatTokens): FormatTokensRewrite.Rule =
    new ConvertImportWildcardToStar(ftoks)

}

private class ConvertImportWildcardToStar(ftoks: FormatTokens)
    extends FormatTokensRewrite.Rule {

  import FormatTokensRewrite._

  @inline
  private def dialect(implicit style: ScalafmtConfig) = style.dialect

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    ConvertImportWildcardToStar.enabled

  override def onToken(implicit
      ft: FormatToken,
      style: ScalafmtConfig
  ): Option[FormatTokensRewrite.Replacement] = Option {
    ft.right match {
      case _: Token.Underscore =>
        ft.meta.rightOwner match {
          case _: Importee.Wildcard if dialect.allowStarWildcardImport =>
            replaceTokenIdent("*", ft.right)
          case _ => null
        }
      case _ => null
    }
  }

  override def onRight(
      left: FormatTokensRewrite.Replacement,
      hasFormatOff: Boolean
  )(implicit ft: FormatToken, style: ScalafmtConfig): Option[
    (FormatTokensRewrite.Replacement, FormatTokensRewrite.Replacement)
  ] = None

}
