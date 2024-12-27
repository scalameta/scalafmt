package org.scalafmt.rewrite

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal._

import scala.meta._
import scala.meta.tokens.{Token => T}

import metaconfig._

object PreferCurlyFors extends Rewrite with FormatTokensRewrite.RuleFactory {

  override def enabled(implicit style: ScalafmtConfig): Boolean = true

  override def create(implicit ftoks: FormatTokens): FormatTokensRewrite.Rule =
    new PreferCurlyFors

  @inline
  private def hasMultipleNonGuardEnums(t: Term.EnumeratorsBlock): Boolean =
    // first one is never a guard
    t.enums.view.drop(1).exists(!_.is[Enumerator.Guard])

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
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Option[Replacement] = Option {
    ft.right match {
      case x: T.LeftParen if (ft.meta.rightOwner match {
            case t: Term.EnumeratorsBlock if hasMultipleNonGuardEnums(t) =>
              style.dialect.allowInfixOperatorAfterNL || hasNoLeadingInfix(t)
            case _ => false
          }) => replaceToken("{")(new T.LeftBrace(x.input, x.dialect, x.start))

      case _: T.Semicolon
          if !style.rewrite.preferCurlyFors.removeTrailingSemicolonsOnly ||
            hasBreakAfterRightBeforeNonComment(ft) =>
        ft.meta.rightOwner match {
          case t: Term.EnumeratorsBlock
              if nextNonCommentAfter(ft).right.is[T.KwIf] || {
                val parenOrBrace = tokenJustBefore(t)
                parenOrBrace.right.is[T.LeftBrace] ||
                session.claimedRule(parenOrBrace).exists(_.rule eq this)
              } => removeToken
          case _ => null
        }

      case _ => null
    }
  }

  override def onRight(left: Replacement, hasFormatOff: Boolean)(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Option[(Replacement, Replacement)] = ft.right match {
    case x: T.RightParen
        if left.how == ReplacementType.Replace &&
          left.ft.right.is[T.LeftBrace] =>
      val right = replaceToken("}")(new T.RightBrace(x.input, x.dialect, x.start))
      Some((left, right))
    case _ => None
  }

  private def hasNoLeadingInfix(t: Term.EnumeratorsBlock)(implicit
      head: FT,
  ): Boolean = findTokenWith(nextNonCommentAfter(head), next)(ft =>
    ft.meta.rightOwner match {
      case ro: Name if (ro.parent match {
            case Some(p: Member.Infix)
                if (p.op eq ro) && ft.right.is[T.Ident] =>
              prevNonCommentSameLine(ft).hasBreak
            case _ => false
          }) => Some(false)
      case `t` if ft.right.is[T.RightParen] => Some(true) // closing delimiter
      case _ => None
    },
  ).contains(true)

}
