package org.scalafmt.rewrite

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.FormatToken
import org.scalafmt.internal.FormatTokens

import scala.meta._
import scala.meta.tokens.Token

import metaconfig._

object PreferCurlyFors extends Rewrite with FormatTokensRewrite.RuleFactory {

  override def enabled(implicit style: ScalafmtConfig): Boolean = true

  override def create(implicit ftoks: FormatTokens): FormatTokensRewrite.Rule =
    new PreferCurlyFors

  @inline
  private def hasMultipleNonGuardEnums(enums: Seq[Enumerator]): Boolean =
    // first one is never a guard
    enums.view.drop(1).exists(!_.is[Enumerator.Guard])

  case class Settings(removeTrailingSemicolonsOnly: Boolean = false)

  object Settings {
    implicit lazy val surface: generic.Surface[Settings] = generic.deriveSurface
    implicit lazy val codec: ConfCodecEx[Settings] = generic
      .deriveCodecEx(Settings()).noTypos
  }

}

/** Replaces multi generator For / ForYield Expression parens and semi-colons
  * with braces and new-lines.
  *
  * For example,
  * {{{
  *   for(a <- as; b <- bs if b > 2) yield (a, b)
  * }}}
  * becomes
  * {{{
  *   for {
  *     a <- as
  *     b <- bs
  *     if b > 2
  *   } yield (a, b)
  * }}}
  */
private class PreferCurlyFors(implicit val ftoks: FormatTokens)
    extends FormatTokensRewrite.Rule {

  import FormatTokensRewrite._
  import PreferCurlyFors._
  import ftoks._

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    PreferCurlyFors.enabled

  override def onToken(implicit
      ft: FormatToken,
      session: Session,
      style: ScalafmtConfig,
  ): Option[Replacement] = Option {
    ft.right match {
      case x: Token.LeftParen if prevNonComment(ft).left.is[Token.KwFor] =>
        val ok = ft.meta.rightOwner match {
          case t: Term.For => hasMultipleNonGuardEnums(t.enums)
          case t: Term.ForYield => hasMultipleNonGuardEnums(t.enums)
          case _ => false
        }
        if (ok)
          replaceToken("{")(new Token.LeftBrace(x.input, x.dialect, x.start))
        else null

      case _: Token.Semicolon
          if !style.rewrite.preferCurlyFors.removeTrailingSemicolonsOnly ||
            hasBreakAfterRightBeforeNonComment(ft) =>
        ft.meta.rightOwner match {
          case t @ (_: Term.For | _: Term.ForYield)
              if nextNonCommentAfter(ft).right.is[Token.KwIf] || {
                val parenOrBrace = nextNonComment(getHead(t))
                parenOrBrace.right.is[Token.LeftBrace] ||
                session.claimedRule(parenOrBrace).exists(_.rule eq this)
              } => removeToken
          case _ => null
        }

      case _ => null
    }
  }

  override def onRight(left: Replacement, hasFormatOff: Boolean)(implicit
      ft: FormatToken,
      session: Session,
      style: ScalafmtConfig,
  ): Option[(Replacement, Replacement)] = ft.right match {
    case x: Token.RightParen
        if left.how == ReplacementType.Replace &&
          left.ft.right.is[Token.LeftBrace] =>
      val right =
        replaceToken("}")(new Token.RightBrace(x.input, x.dialect, x.start))
      Some((left, right))
    case _ => None
  }

}
