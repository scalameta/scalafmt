package org.scalafmt.rewrite

import scala.meta.Importee
import scala.meta.Pat
import scala.meta.Term
import scala.meta.Type
import scala.meta.tokens.Token

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.FormatToken
import org.scalafmt.internal.FormatTokens

object ConvertToNewScala3Syntax extends FormatTokensRewrite.RuleFactory {

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    style.runner.dialect.allowSignificantIndentation &&
      style.rewrite.scala3.convertToNewSyntax

  override def create(ftoks: FormatTokens): FormatTokensRewrite.Rule =
    new ConvertToNewScala3Syntax(ftoks)

}

private class ConvertToNewScala3Syntax(ftoks: FormatTokens)
    extends FormatTokensRewrite.Rule {

  import FormatTokensRewrite._

  @inline
  private def dialect(implicit style: ScalafmtConfig) = style.runner.dialect

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    ConvertToNewScala3Syntax.enabled

  override def onToken(implicit
      ft: FormatToken,
      style: ScalafmtConfig
  ): Option[Replacement] = Option {
    ft.right match {

      case _: Token.LeftParen =>
        ft.meta.rightOwner match {
          case t: Term.If if ftoks.prevNonComment(ft).left.is[Token.KwIf] =>
            removeToken
          case t: Term.While
              if ftoks.prevNonComment(ft).left.is[Token.KwWhile] =>
            removeToken
          case t: Term.For if ftoks.prevNonComment(ft).left.is[Token.KwFor] =>
            removeToken
          case t: Term.ForYield
              if ftoks.prevNonComment(ft).left.is[Token.KwFor] =>
            removeToken
          case _ => null
        }

      case _: Token.Colon =>
        ft.meta.rightOwner match {
          case _: Term.Repeated if dialect.allowPostfixStarVarargSplices =>
            removeToken // trick: to get "*", just remove ":" and "_"
          case _ => null
        }

      case _: Token.At =>
        ft.meta.rightOwner match {
          case Pat.Bind(_, _: Pat.SeqWildcard)
              if dialect.allowPostfixStarVarargSplices =>
            removeToken // trick: to get "*", just remove "@" and "_"
          case _ => null
        }

      case _: Token.Underscore =>
        ft.meta.rightOwner match {
          case _: Importee.Wildcard if dialect.allowStarWildcardImport =>
            replaceTokenIdent("*", ft.right)
          case _: Type.Placeholder if dialect.allowQuestionMarkPlaceholder =>
            replaceTokenIdent("?", ft.right)
          case _: Term.Repeated | _: Pat.SeqWildcard
              if dialect.allowPostfixStarVarargSplices =>
            removeToken // see above, under Colon and At
          case _ => null
        }

      case _: Token.RightArrow =>
        ft.meta.rightOwner match {
          case _: Importee.Rename | _: Importee.Unimport
              if dialect.allowAsForImportRename =>
            replaceTokenIdent("as", ft.right)
          case _ => null
        }

      case _ => null
    }
  }

  override def onRight(left: Replacement, hasFormatOff: Boolean)(implicit
      ft: FormatToken,
      style: ScalafmtConfig
  ): Option[(Replacement, Replacement)] = Option {
    ft.right match {

      case x: Token.RightParen if left.isLeft =>
        ft.meta.rightOwner match {
          case _: Term.If =>
            if (!ftoks.nextNonComment(ftoks.next(ft)).right.is[Token.KwThen])
              replaceToken("then")(
                new Token.KwThen(x.input, x.dialect, x.start)
              )
            else removeToken
          case _: Term.While | _: Term.For =>
            if (!ftoks.nextNonComment(ftoks.next(ft)).right.is[Token.KwDo])
              replaceToken("do")(new Token.KwDo(x.input, x.dialect, x.start))
            else removeToken
          case _ => null
        }

      case _ => null
    }
  }.map((left, _))

}
