package org.scalafmt.rewrite

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal._

import scala.meta._
import scala.meta.tokens.{Token => T}

object ConvertToNewScala3Syntax extends FormatTokensRewrite.RuleFactory {

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    style.rewrite.scala3.convertToNewSyntax

  override def create(implicit ftoks: FormatTokens): FormatTokensRewrite.Rule =
    new ConvertToNewScala3Syntax

}

private class ConvertToNewScala3Syntax(implicit val ftoks: FormatTokens)
    extends FormatTokensRewrite.Rule {

  import FormatTokensRewrite._

  @inline
  private def dialect(implicit style: ScalafmtConfig) = style.dialect

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    ConvertToNewScala3Syntax.enabled

  override def onToken(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Option[Replacement] = Option {
    val flag = style.rewrite.scala3.newSyntax
    def pft = ftoks.prevNonComment(ft)
    def left = pft.left

    def removeAndClaimUnderscore(): Replacement = {
      val nft = ftoks.nextNonCommentAfter(ft)
      if (nft.right.is[T.Underscore]) removeToken(nft.idx :: Nil) else null
    }
    def removeUnderscoreIfPreviousRemoved(): Replacement =
      if (session.isRemovedOnLeft(pft, ok = true)) removeToken else null

    ft.right match {

      case _: T.LeftParen if flag.control && dialect.allowQuietSyntax =>
        ft.meta.rightOwner match {
          case _: Term.If if left.is[T.KwIf] => removeToken
          case _: Term.While if left.is[T.KwWhile] => removeToken
          case _: Term.EnumeratorsBlock if left.is[T.KwFor] => removeToken
          case _ => null
        }

      case _: T.Colon
          if flag.deprecated && dialect.allowPostfixStarVarargSplices =>
        // trick: to get "*", just remove ":" and "_"
        ft.meta.rightOwner match {
          case t: Term.Repeated if isSimpleRepeated(t) =>
            removeAndClaimUnderscore()
          case Pat.Bind(_, _: Pat.SeqWildcard) => removeAndClaimUnderscore()
          case _ => null
        }

      case _: T.At
          if flag.deprecated && dialect.allowPostfixStarVarargSplices =>
        // trick: to get "*", just remove "@" and "_"
        ft.meta.rightOwner match {
          case Pat.Bind(_, _: Pat.SeqWildcard) => removeAndClaimUnderscore()
          case _ => null
        }

      case _: T.Underscore if flag.deprecated =>
        ft.meta.rightOwner match {
          case _: Importee.Wildcard if dialect.allowStarWildcardImport =>
            replaceTokenIdent("*", ft.right)
          case t: Type.Wildcard
              if dialect.allowQuestionMarkAsTypeWildcard &&
                t.parent.is[Type.ArgClause] => replaceTokenIdent("?", ft.right)
          case t: Term.Repeated
              if dialect.allowPostfixStarVarargSplices && isSimpleRepeated(t) =>
            removeUnderscoreIfPreviousRemoved()
          case t: Pat.SeqWildcard
              if dialect.allowPostfixStarVarargSplices &&
                t.parent.is[Pat.Bind] => removeUnderscoreIfPreviousRemoved()
          case _ => null
        }

      case _: T.LeftBrace
          if flag.deprecated && dialect.allowAsForImportRename =>
        ft.meta.rightOwner match {
          case t: Importer if t.importees.lengthCompare(1) == 0 => removeToken
          case _ => null
        }

      case _: T.RightArrow
          if flag.deprecated && dialect.allowAsForImportRename =>
        ft.meta.rightOwner match {
          case _: Importee.Rename | _: Importee.Unimport =>
            replaceTokenIdent("as", ft.right)
          case _ => null
        }

      case T.Ident("*") if flag.deprecated =>
        ft.meta.rightOwner match {
          case _: Type.AnonymousParam
              if dialect.allowUnderscoreAsTypePlaceholder =>
            replaceTokenBy("_")(t =>
              new T.Underscore(t.input, t.dialect, t.start),
            )
          case _ => null
        }

      case _ => null
    }
  }

  override def onRight(left: Replacement, hasFormatOff: Boolean)(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Option[(Replacement, Replacement)] = Option {
    def nextRight = ftoks.nextNonComment(ftoks.next(ft)).right
    ft.right match {

      case x: T.RightParen if left.isRemove =>
        ft.meta.rightOwner match {
          case _: Term.If =>
            if (!nextRight.is[T.KwThen])
              replaceToken("then")(new T.KwThen(x.input, x.dialect, x.start))
            else removeToken
          case _: Term.While =>
            if (!nextRight.is[T.KwDo])
              replaceToken("do")(new T.KwDo(x.input, x.dialect, x.start))
            else removeToken
          case t: Term.EnumeratorsBlock if t.parent.is[Term.For] =>
            if (!nextRight.is[T.KwDo]) replaceToken("do", t.parent)(
              new T.KwDo(x.input, x.dialect, x.start),
            )
            else removeToken
          case _ => null
        }

      case _: T.RightBrace if left.isRemove => removeToken

      case _ => null
    }
  }.map((left, _))

  private def isSimpleRepeated(t: Term.Repeated): Boolean = t.expr
    .isNot[Term.ApplyInfix] || ftoks.isEnclosedWithinParens(t.expr)

}
