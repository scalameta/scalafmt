package org.scalafmt.rewrite

import scala.meta._
import scala.meta.tokens.Token

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.FormatToken
import org.scalafmt.internal.FormatTokens

object PreferCurlyFors extends Rewrite with FormatTokensRewrite.RuleFactory {

  override def enabled(implicit style: ScalafmtConfig): Boolean = true

  override def create(ftoks: FormatTokens): FormatTokensRewrite.Rule =
    new PreferCurlyFors

  private def hasMultipleGenerators(enums: Seq[Enumerator]): Boolean =
    enums.count(_.is[Enumerator.Generator]) > 1

}

/** Replaces multi generator For / ForYield Expression parens and semi-colons
  * with braces and new-lines.
  *
  * For example,
  *
  *   for(a <- as; b <- bs if b > 2) yield (a, b)
  *
  * becomes,
  *
  *   for {
  *     a <- as
  *     b <- bs if b > 2
  *   } yield (a, b)
  */
private class PreferCurlyFors extends FormatTokensRewrite.Rule {

  import FormatTokensRewrite._
  import PreferCurlyFors._

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    PreferCurlyFors.enabled

  override def onToken(implicit
      ft: FormatToken,
      style: ScalafmtConfig
  ): Option[Replacement] = Option {
    ft.right match {
      case x: Token.LeftParen =>
        val ok = ft.meta.rightOwner match {
          case t: Term.For => hasMultipleGenerators(t.enums)
          case t: Term.ForYield => hasMultipleGenerators(t.enums)
          case _ => false
        }
        if (ok)
          replaceToken("{")(new Token.LeftBrace(x.input, x.dialect, x.start))
        else null

      case _: Token.Semicolon =>
        ft.meta.rightOwner match {
          case _: Term.For | _: Term.ForYield => removeToken
          case _ => null
        }

      case _ => null
    }
  }

  override def onRight(left: Replacement, hasFormatOff: Boolean)(implicit
      ft: FormatToken,
      style: ScalafmtConfig
  ): Option[(Replacement, Replacement)] =
    ft.right match {
      case x: Token.RightParen if left.exists(_.right.is[Token.LeftBrace]) =>
        val right =
          replaceToken("}")(new Token.RightBrace(x.input, x.dialect, x.start))
        Some((left, right))
      case _ => None
    }

}
