package org.scalafmt.rewrite

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal._

import scala.meta._
import scala.meta.tokens.{Token => T}

object RemoveSemicolons extends Rewrite with FormatTokensRewrite.RuleFactory {

  override def enabled(implicit style: ScalafmtConfig): Boolean = true

  override def create(implicit ftoks: FormatTokens): FormatTokensRewrite.Rule =
    new RemoveSemicolons

}

class RemoveSemicolons(implicit val ftoks: FormatTokens)
    extends FormatTokensRewrite.Rule {

  import FormatTokensRewrite._

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    RemoveSemicolons.enabled

  override def onToken(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Option[Replacement] = {
    val skip = ft.right.is[T.Semicolon] &&
      (ft.rightOwner match {
        case t: Term.EnumeratorsBlock => t.enums.lengthCompare(1) == 0 ||
          ftoks.nextNonCommentAfter(ft).right.is[T.KwIf] ||
          !ftoks.isEnclosedWithinParens(t)
        case _: Tree.Block | _: Case => true
        case _ => false
      })
    if (skip) Some(removeToken) else None
  }

  override def onRight(lt: Replacement, hasFormatOff: Boolean)(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Option[(Replacement, Replacement)] = None

}
