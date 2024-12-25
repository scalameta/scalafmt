package org.scalafmt.rewrite

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.config.TrailingCommas
import org.scalafmt.internal._

import scala.meta._
import scala.meta.tokens.{Token => T}

object RewriteTrailingCommas extends FormatTokensRewrite.RuleFactory {

  import FormatTokensRewrite._

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    style.dialect.allowTrailingCommas &&
      style.getTrailingCommas.ne(TrailingCommas.keep)

  override def create(implicit ftoks: FormatTokens): FormatTokensRewrite.Rule =
    new RewriteTrailingCommas

  private[rewrite] def checkIfPrevious(implicit
      ft: FT,
      session: Session,
      ftoks: FormatTokens,
  ): Boolean = ft.right match {
    case _: T.RightParen =>
      val maybeCommaFt = ftoks.prevNonComment(ft)
      !maybeCommaFt.left.is[T.Comma] ||
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
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Option[Replacement] = if (shouldRemove(ft)) Some(removeToken) else None

  private[rewrite] def shouldRemove(ft: FT): Boolean = ft.right.is[T.Comma] &&
    // comma and paren/bracket/brace should generally have the same owner
    // however, with optional-braces comma could be before outdent
    // and hence owned by the previous expression
    // there's no construct which requires a comma before a closing delim
    ftoks.nextNonCommentAfter(ft).right.is[T.CloseDelim]

  override def onRight(lt: Replacement, hasFormatOff: Boolean)(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Option[(Replacement, Replacement)] = None

}
