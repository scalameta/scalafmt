package org.scalafmt.rewrite

import org.scalafmt.config.{RedundantParensSettings, ScalafmtConfig}
import org.scalafmt.internal._
import org.scalafmt.util.InfixApp._
import org.scalafmt.util.TreeOps

import scala.meta._
import scala.meta.tokens.{Token => T}

import scala.annotation.tailrec

object RedundantParens extends Rewrite with FormatTokensRewrite.RuleFactory {

  override def enabled(implicit style: ScalafmtConfig): Boolean = true

  override def priority: Int = RedundantBraces.priority

  override def create(implicit ftoks: FormatTokens): FormatTokensRewrite.Rule =
    new RedundantParens

  private def infixNeedsParens(outer: Member.Infix, inner: Tree)(implicit
      style: ScalafmtConfig,
  ): Boolean = {
    import style.dialect
    TreeSyntacticGroup.groupNeedsParens(outer, inner)
  }

  def breaksBeforeOp(
      ia: Member.Infix,
  )(implicit style: ScalafmtConfig, ftoks: FormatTokens): Boolean = {
    val keepInfix = !style.newlines.infix.sourceIgnored(ia)
    def impl(ia: Member.Infix): Boolean = {
      val beforeOp = ftoks.prevNonCommentSameLine(ftoks.tokenJustBefore(ia.op))
      beforeOp.hasBreak && (keepInfix || beforeOp.left.is[T.Comment]) ||
      ia.nestedInfixApps.exists(x => !ftoks.isEnclosedWithinParens(x) && impl(x))
    }
    impl(ia)
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

  private def okToReplaceDeclTpe(p: Tree.WithDeclTpe, t: Tree): Boolean =
    (t, p) match {
      // https://dotty.epfl.ch/docs/reference/syntax.html#declarations-and-definitions-1
      // given requires AnnotType, not any Type
      case (_: Type.ApplyInfix, _: Stat.GivenLike) => false
      case _ => true
    }

}

class RedundantParens(implicit val ftoks: FormatTokens)
    extends FormatTokensRewrite.Rule {

  import FormatTokensRewrite._
  import RedundantParens._

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    RedundantParens.enabled

  override def onToken(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Option[Replacement] = ft.right match {
    case _: T.LeftParen if okToReplace => Some(removeToken)
    case _ => None
  }

  override def onRight(left: Replacement, hasFormatOff: Boolean)(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Option[(Replacement, Replacement)] =
    if (left.isRemove && RewriteTrailingCommas.checkIfPrevious)
      Some((left, removeToken))
    else None

  private def okToReplaceWithCount(numParens: Int, tree: Tree, lpOuter: FT)(
      implicit style: ScalafmtConfig,
  ): Boolean = tree match {
    case _: Lit.Unit => numParens >= 3
    case _: Member.Tuple => numParens >=
        (if (lpOuter.rightOwner.is[Member.SyntaxValuesClause]) 3 else 2)
    case _ if numParens >= 2 => true

    case _: Term.AnonymousFunction | _: Term.Param => false
    case _: Type.ParamFunctionType => false
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
          ftoks.tokenBefore(p.body).left.is[T.KwDo]
        case p: Term.If => p.cond.eq(t) && style.dialect.allowQuietSyntax &&
          ftoks.tokenBefore(p.thenp).left.is[T.KwThen]
        case p: Term.TryClause =>
          (style.dialect.allowTryWithAnyExpr || p.expr.ne(t)) &&
          canRewriteBody(t)
        case p: Term.ArgClause => p.parent.exists {
            case pia: Member.Infix => okToReplaceInfix(pia, t)
            case _ => true
          }
        case pia: Member.Infix => okToReplaceInfix(pia, t)
        case p: Tree.WithDeclTpe if p.decltpe eq t => okToReplaceDeclTpe(p, t)
        case p: Tree.WithDeclTpeOpt if p.decltpe.contains(t) => true
        case _ => okToReplaceOther(t)
      }
  }

  private def okToReplaceOther(t: Tree): Boolean = t match {
    case _: Lit => t.tokens.length == 1 || !t.parent.is[Term.Ref]
    case _: Term.ApplyUnary => !t.parent.is[Term.Ref]
    case _: Member.Apply | _: Term.Interpolate | _: Term.PartialFunction => true
    case _: Term.SelectPostfix => false
    case _: Ref => true // Ref must be after SelectPostfix and ApplyUnary
    case t: Term.SelectMatch => ftoks.getHead(t.casesBlock).left.is[T.LeftBrace]
    case _ => false
  }

  private def okToReplaceArgClause(
      ac: Member.ArgClause,
  )(implicit style: ScalafmtConfig): Boolean = ac.values match {
    case arg :: Nil => arg match {
        case b: Term.Block => !(ac.parent.isOpt[Init] ||
            RedundantBraces.used && RedundantBraces.canRewriteStatWithParens(b))
        case _: Term.PartialFunction => !ac.parent.isOpt[Init]
        case _: Lit.Unit | _: Member.Tuple => false
        case _: Term.SelectPostfix => false
        case _ => ac.parent.exists {
            case pia: Member.Infix => okToReplaceInfix(pia, arg)
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
        tiaPrecedence >= precedenceHigh ||
        tiaPrecedence > precedenceLowest && pia.precedence <= precedenceLowest
      case RedundantParensSettings.InfixSide.some =>
        val tiaPrecedence = tia.precedence
        tiaPrecedence >= precedenceVeryHigh ||
        tiaPrecedence >= precedenceMedium && pia.precedence <= precedenceLowest
      case _ => true
    }

  private def okToReplaceInfix(pia: Member.Infix, t: Tree)(implicit
      style: ScalafmtConfig,
  ): Boolean = t match {
    case _: Term.AnonymousFunction | _: Term.SelectPostfix => false
    case _ if infixNeedsParens(pia, t) => false
    case tia: Member.Infix => okToReplaceInfix(pia, tia)
    case _: Lit | _: Name | _: Term.Interpolate => true
    case _ => style.rewrite.redundantParens.infixSide.isDefined
  }

  private def canRewriteBody(
      tree: Tree,
  )(implicit style: ScalafmtConfig): Boolean = tree match {
    case ia: Member.Infix => !breaksBeforeOp(ia)
    case _ => true
  }

  private def okToReplace(implicit ft: FT, style: ScalafmtConfig): Boolean = {
    // counts consecutive paren pairs starting with the given one as the innermost
    // the parens could belong to tree they are enclosing, or its parent
    @tailrec
    def iter(lt: FT, rt: FT, cnt: Int): Boolean =
      (ftoks.prevNonComment(lt), ftoks.nextNonComment(rt)) match {
        case (
              prev @ FT(_: T.LeftParen, _, _),
              next @ FT(_, _: T.RightParen, _),
            ) => iter(ftoks.prev(prev), ftoks.next(next), cnt + 1)
        case _ => TreeOps
            .findEnclosedBetweenParens(lt.right, rt.left, ft.meta.rightOwner)
            .exists(okToReplaceWithCount(cnt, _, lt))
      }

    ftoks.matchingOptRight(ft).exists(rt => iter(ft, rt, 1))
  }

}
