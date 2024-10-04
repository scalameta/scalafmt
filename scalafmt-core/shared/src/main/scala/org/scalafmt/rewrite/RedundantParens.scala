package org.scalafmt.rewrite

import org.scalafmt.config.RedundantParensSettings
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.FormatToken
import org.scalafmt.internal.FormatTokens
import org.scalafmt.internal.Side
import org.scalafmt.internal.SyntacticGroupOps
import org.scalafmt.internal.TreeSyntacticGroup
import org.scalafmt.util.InfixApp._
import org.scalafmt.util.TreeOps

import scala.meta._
import scala.meta.tokens.Token

import scala.annotation.tailrec

object RedundantParens extends Rewrite with FormatTokensRewrite.RuleFactory {

  private type Enclosed = (Int, Tree)

  override def enabled(implicit style: ScalafmtConfig): Boolean = true

  override def create(implicit ftoks: FormatTokens): FormatTokensRewrite.Rule =
    new RedundantParens

  private def infixNeedsParens(outer: Member.Infix, inner: Tree): Boolean = {
    val sgOuter = TreeSyntacticGroup(outer)
    val sgInner = TreeSyntacticGroup(inner)
    val side = if (outer.lhs eq inner) Side.Left else Side.Right
    SyntacticGroupOps.groupNeedsParenthesis(sgOuter, sgInner, side)
  }

  private val precedenceVeryHigh = getPrecedence("+")
  private val precedenceHigh = getPrecedence("=")
  private val precedenceMedium = getPrecedence("<")
  private val precedenceLowest = getPrecedence("foo")

  object IsExprBody {
    def unapply(t: Tree): Option[Boolean] = {
      @inline
      def okIf(body: Tree) = if (body eq t) Some(true) else None
      t.parent.flatMap {
        case p: Case => okIf(p.body)
        case p: Term.If if p.cond ne t =>
          Some(p.thenp.ne(t) || !TreeOps.ifWithoutElse(t))
        case p: Term.While => okIf(p.body)
        case p: Tree.WithBody if t eq p.body => Some(true)
        case _: Term.Return | _: Term.Throw | _: Term.QuotedMacroExpr |
            _: Term.SplicedMacroExpr | _: Term.Block => Some(true)
        case _ => None
      }
    }
  }

  object IsInBraces {
    def unapply(t: Tree): Option[Boolean] = t match {
      case _: Term.Block | _: Term.PartialFunction => t.parent.collect {
          case _: Term.ArgClause => true
          case _: Term.Block => false
        }
      case _ => None
    }
  }

}

class RedundantParens(implicit val ftoks: FormatTokens)
    extends FormatTokensRewrite.Rule {

  import FormatTokensRewrite._
  import RedundantParens._

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    RedundantParens.enabled

  override def onToken(implicit
      ft: FormatToken,
      session: Session,
      style: ScalafmtConfig,
  ): Option[Replacement] = ft.right match {
    case _: Token.LeftParen => findEnclosed.flatMap { case (cnt, tree) =>
        if (okToReplaceWithCount(cnt, tree)) Some(removeToken) else None
      }
    case _ => None
  }

  override def onRight(left: Replacement, hasFormatOff: Boolean)(implicit
      ft: FormatToken,
      session: Session,
      style: ScalafmtConfig,
  ): Option[(Replacement, Replacement)] =
    if (left.isRemove && RewriteTrailingCommas.checkIfPrevious)
      Some((left, removeToken))
    else None

  private def okToReplaceWithCount(numParens: Int, tree: Tree)(implicit
      style: ScalafmtConfig,
  ): Boolean = tree match {
    case _: Lit.Unit | _: Member.Tuple => numParens >= 3
    case _ if numParens >= 2 => true

    case _: Term.AnonymousFunction | _: Term.Param => false
    case _: Type.FunctionType => false
    case _: Init => false

    case t: Member.ArgClause => okToReplaceArgClause(t)
    case Term.ParamClause(t :: Nil, _) => tree.parent.exists {
        case _: Term.FunctionTerm => t.decltpe.isEmpty && t.mods.isEmpty
        case _ => false
      }
    case _: Member.SyntaxValuesClause => false

    case IsInBraces(ok) => ok
    case t @ IsExprBody(ok) => ok && canRewriteBody(t)

    case t => t.parent.forall {
        case _: Enumerator.Guard => RewriteCtx.isPostfixExpr(t)
        case p: Case => p.cond.contains(t) && RewriteCtx.isPostfixExpr(t)
        case _: Term.Do => false
        case p: Term.While => p.expr.eq(t) && style.dialect.allowQuietSyntax &&
          ftoks.tokenBefore(p.body).left.is[Token.KwDo]
        case p: Term.If => p.cond.eq(t) && style.dialect.allowQuietSyntax &&
          ftoks.tokenBefore(p.thenp).left.is[Token.KwThen]
        case p: Term.TryClause =>
          (style.dialect.allowTryWithAnyExpr || p.expr.ne(t)) &&
          canRewriteBody(t)
        case p: Term.ArgClause => p.parent.exists {
            case pia: Member.Infix => !infixNeedsParens(pia, t) &&
              okToReplaceInfix(pia, t)
            case _ => true
          }
        case pia: Member.Infix if !infixNeedsParens(pia, t) =>
          okToReplaceInfix(pia, t)
        case p: Tree.WithDeclTpe if p.decltpe eq t => true
        case p: Tree.WithDeclTpeOpt if p.decltpe.contains(t) => true
        case _ => okToReplaceOther(t)
      }
  }

  private def isSelectWithDot(t: Term.Select): Boolean = ftoks
    .tokenBefore(t.name).left.is[Token.Dot]

  private def okToReplaceOther(
      t: Tree,
  )(implicit style: ScalafmtConfig): Boolean = t match {
    case _: Lit => t.tokens.length == 1 || !t.parent.is[Term.Ref]
    case _: Term.ApplyUnary => !t.parent.is[Term.Ref]
    case _: Member.Apply | _: Term.Interpolate | _: Term.PartialFunction => true
    case t: Term.Select => isSelectWithDot(t)
    case _: Ref => true // Ref must be after Select and ApplyUnary
    case t: Term.Match => style.dialect.allowMatchAsOperator &&
      ftoks.tokenAfter(t.expr).right.is[Token.Dot] && // like select
      ftoks.getHead(t.casesBlock).left.is[Token.LeftBrace]
    case _ => false
  }

  private def okToReplaceArgClause(
      t: Member.ArgClause,
  )(implicit style: ScalafmtConfig): Boolean = t.values match {
    case arg :: Nil => arg match {
        case _: Term.Block | _: Term.PartialFunction => !t.parent.isOpt[Init]
        case _: Lit.Unit | _: Member.Tuple => false
        case t: Term.Select if !isSelectWithDot(t) => false
        case _ => t.parent.exists {
            case pia: Member.Infix =>
              val keep = infixNeedsParens(pia, arg)
              if (keep) okToReplaceOther(arg) else okToReplaceInfix(pia, arg)
            case _ => false
          }
      }
    case _ => false
  }

  private def okToReplaceInfix(pia: Member.Infix, tia: Member.Infix)(implicit
      style: ScalafmtConfig,
  ): Boolean = !breaksBeforeOp(tia) &&
    style.rewrite.redundantParens.infixSide.exists {
      case RedundantParensSettings.InfixSide.many
          if tia.op.value != pia.op.value =>
        val tiaPrecedence = tia.precedence
        tiaPrecedence <= precedenceHigh ||
        tiaPrecedence < precedenceLowest && pia.precedence >= precedenceLowest
      case RedundantParensSettings.InfixSide.some =>
        val tiaPrecedence = tia.precedence
        tiaPrecedence <= precedenceVeryHigh ||
        tiaPrecedence <= precedenceMedium && pia.precedence >= precedenceLowest
      case _ => true
    }

  private def okToReplaceInfix(pia: Member.Infix, t: Tree)(implicit
      style: ScalafmtConfig,
  ): Boolean = t match {
    case tia: Member.Infix => okToReplaceInfix(pia, tia)
    case _: Lit | _: Name | _: Term.Interpolate => true
    case _: Term.PartialFunction => true
    case _: Term.AnonymousFunction => false
    case t: Term.Select if !isSelectWithDot(t) => false
    case _ => style.rewrite.redundantParens.infixSide.isDefined
  }

  private def breaksBeforeOpAndNotEnclosed(ia: Member.Infix): Boolean =
    !ftoks.isEnclosedInParens(ia) && breaksBeforeOp(ia)

  private def breaksBeforeOp(ia: Member.Infix): Boolean = {
    val beforeOp = ftoks.tokenJustBefore(ia.op)
    ftoks.prevNonCommentSameLine(beforeOp).hasBreak ||
    ia.nestedInfixApps.exists(breaksBeforeOpAndNotEnclosed)
  }

  private def canRewriteBody(tree: Tree): Boolean = tree match {
    case ia: Member.Infix => !breaksBeforeOp(ia)
    case _ => true
  }

  private def findEnclosed(implicit ft: FormatToken): Option[Enclosed] = {
    // counts consecutive parent pairs starting with the given one as the innermost
    // the parens could belong to tree they are enclosing, or its parent
    @tailrec
    def iter(lt: FormatToken, rt: FormatToken, cnt: Int): Option[Enclosed] =
      (ftoks.prevNonComment(lt), ftoks.nextNonComment(rt)) match {
        case (
              prev @ FormatToken(_: Token.LeftParen, _, _),
              next @ FormatToken(_, _: Token.RightParen, _),
            ) => iter(ftoks.prev(prev), ftoks.next(next), cnt + 1)
        case _ => TreeOps
            .findEnclosedBetweenParens(lt.right, rt.left, ft.meta.rightOwner)
            .map((cnt, _))
      }

    ftoks.matchingOpt(ft.right).flatMap(rt => iter(ft, rt, 1))
  }

}
