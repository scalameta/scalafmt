package org.scalafmt.rewrite

import scala.meta.{Importee, Pat, Term, Type}
import scala.meta.tokens.Token

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.FormatToken
import org.scalafmt.internal.FormatTokens

object ConvertToNewScala3Syntax extends FormatTokensRewrite.RuleFactory {

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    style.rewrite.scala3.convertToNewSyntax

  override def create(ftoks: FormatTokens): FormatTokensRewrite.Rule =
    new ConvertToNewScala3Syntax(ftoks)

}

private class ConvertToNewScala3Syntax(ftoks: FormatTokens)
    extends FormatTokensRewrite.Rule {

  import FormatTokensRewrite._

  @inline
  private def dialect(implicit style: ScalafmtConfig) = style.dialect

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    ConvertToNewScala3Syntax.enabled

  override def onToken(implicit
      ft: FormatToken,
      style: ScalafmtConfig
  ): Option[Replacement] = Option {
    ft.right match {

      case _: Token.LeftParen if dialect.allowSignificantIndentation =>
        ft.meta.rightOwner match {
          case _: Term.If if ftoks.prevNonComment(ft).left.is[Token.KwIf] =>
            removeToken
          case _: Term.While
              if ftoks.prevNonComment(ft).left.is[Token.KwWhile] =>
            removeToken
          case _: Term.For if ftoks.prevNonComment(ft).left.is[Token.KwFor] =>
            removeToken
          case _: Term.ForYield
              if ftoks.prevNonComment(ft).left.is[Token.KwFor] =>
            removeToken
          case _ => null
        }

      case _: Token.Colon if dialect.allowPostfixStarVarargSplices =>
        ft.meta.rightOwner match {
          case t: Term.Repeated if isSimpleRepeated(t) =>
            removeToken // trick: to get "*", just remove ":" and "_"
          case _ => null
        }

      case _: Token.At if dialect.allowPostfixStarVarargSplices =>
        ft.meta.rightOwner match {
          case Pat.Bind(_, _: Pat.SeqWildcard) =>
            removeToken // trick: to get "*", just remove "@" and "_"
          case _ => null
        }

      case _: Token.Underscore =>
        ft.meta.rightOwner match {
          case _: Importee.Wildcard if dialect.allowStarWildcardImport =>
            replaceTokenIdent("*", ft.right)
          case t: Type.Wildcard
              if dialect.allowQuestionMarkAsTypeWildcard &&
                t.parent.exists(_.is[Type.ArgClause]) =>
            replaceTokenIdent("?", ft.right)
          case t: Term.Repeated
              if dialect.allowPostfixStarVarargSplices && isSimpleRepeated(t) =>
            removeToken // see above, under Colon
          case t: Pat.SeqWildcard
              if dialect.allowPostfixStarVarargSplices &&
                t.parent.exists(_.is[Pat.Bind]) =>
            removeToken // see above, under At
          case _ => null
        }

      case _: Token.RightArrow if dialect.allowAsForImportRename =>
        ft.meta.rightOwner match {
          case _: Importee.Rename | _: Importee.Unimport =>
            replaceTokenIdent("as", ft.right)
          case _ => null
        }

      case Token.Ident("*") =>
        ft.meta.rightOwner match {
          case _: Type.AnonymousParam
              if dialect.allowUnderscoreAsTypePlaceholder =>
            replaceTokenBy("_")(t =>
              new Token.Underscore(t.input, t.dialect, t.start)
            )
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

      case x: Token.RightParen if left.how eq ReplacementType.Remove =>
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

  private def isSimpleRepeated(t: Term.Repeated): Boolean =
    t.expr.isNot[Term.ApplyInfix] || ftoks.isEnclosedInParens(t.expr)

}
