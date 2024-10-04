package org.scalafmt.rewrite

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.config.TrailingCommas
import org.scalafmt.internal.FormatToken
import org.scalafmt.internal.FormatTokens

import scala.meta._
import scala.meta.tokens.Token

object RewriteTrailingCommas extends FormatTokensRewrite.RuleFactory {

  import FormatTokensRewrite._

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    style.dialect.allowTrailingCommas &&
      style.getTrailingCommas.ne(TrailingCommas.keep)

  override def create(implicit ftoks: FormatTokens): FormatTokensRewrite.Rule =
    new RewriteTrailingCommas

  private[rewrite] def checkIfPrevious(implicit
      ft: FormatToken,
      session: Session,
      ftoks: FormatTokens,
  ): Boolean = ft.right match {
    case _: Token.RightParen =>
      val maybeCommaFt = ftoks.prevNonComment(ft)
      !maybeCommaFt.left.is[Token.Comma] ||
      session.isRemovedOnLeft(maybeCommaFt, true)
    case _ => true
  }

}

private class RewriteTrailingCommas(implicit val ftoks: FormatTokens)
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
      session: Session,
      style: ScalafmtConfig,
  ): Option[Replacement] = if (shouldRemove(ft)) Some(removeToken) else None

  private[rewrite] def shouldRemove(
      ft: FormatToken,
  )(implicit session: Session): Boolean = ft.right.is[Token.Comma] && {
    val nft = ftoks.nextNonCommentAfter(ft)
    def delimOwner = nft.meta.rightOwner

    // comma and paren/bracket/brace should generally have the same owner
    // however, with optional-braces comma could be before outdent
    // and hence owned by the previous expression
    nft.right match {
      case rp: Token.RightParen => delimOwner
          .isAny[Member.SyntaxValuesClause, Member.Tuple] ||
        ftoks.matchingOpt(rp).exists { lp =>
          val claimant = session.claimedRule(ftoks.prev(lp))
          claimant.forall(_.rule.isInstanceOf[RedundantParens])
        }

      case _: Token.RightBracket => delimOwner.is[Member.SyntaxValuesClause]

      case _: Token.RightBrace => delimOwner.is[Importer]

      case _ => false
    }
  }

  override def onRight(lt: Replacement, hasFormatOff: Boolean)(implicit
      ft: FormatToken,
      session: Session,
      style: ScalafmtConfig,
  ): Option[(Replacement, Replacement)] = None

}
