package org.scalafmt.rewrite

import scala.meta._
import scala.meta.tokens.Token

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.config.TrailingCommas
import org.scalafmt.internal.FormatToken
import org.scalafmt.internal.FormatTokens
import org.scalafmt.util.TreeOps

object RewriteTrailingCommas extends FormatTokensRewrite.RuleFactory {

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    style.runner.dialect.allowTrailingCommas &&
      style.trailingCommas != TrailingCommas.preserve

  override def create(ftoks: FormatTokens): FormatTokensRewrite.Rule =
    new RewriteTrailingCommas(ftoks)

}

private class RewriteTrailingCommas(ftoks: FormatTokens)
    extends FormatTokensRewrite.Rule {

  /* even if this rule is called for 'always', we'll still remove any trailing
   * commas here, to avoid any idempotence problems caused by an extra comma,
   * with a different AST, in a subsequent runs; in FormatWriter, we'll add
   * them back if there is a newline before the closing bracket/paren/brace */

  import FormatTokensRewrite._

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    RewriteTrailingCommas.enabled

  override def onToken(implicit
      ft: FormatToken,
      style: ScalafmtConfig
  ): Option[Replacement] = {
    val ok = ft.right.is[Token.Comma] && {
      val rightOwner = ft.meta.rightOwner
      val nft = ftoks.nextNonComment(ftoks.next(ft))

      // comma and paren/bracket/brace need to have the same owner
      (rightOwner eq nft.meta.rightOwner) && (nft.right match {
        case _: Token.RightBracket =>
          rightOwner match {
            case TreeOps.SplitDefnIntoParts(_) => true
            case _ => false
          }

        case _: Token.RightParen =>
          rightOwner match {
            case TreeOps.SplitDefnIntoParts(_) => true
            case TreeOps.SplitCallIntoParts(_) => true
            case _ => false
          }

        case _: Token.RightBrace =>
          rightOwner match {
            case _: Importer => true
            case _ => false
          }

        case _ => false
      })
    }
    if (ok) Some(removeToken) else None
  }

  override def onRight(lt: Replacement, hasFormatOff: Boolean)(implicit
      ft: FormatToken,
      style: ScalafmtConfig
  ): Option[(Replacement, Replacement)] = None

}
