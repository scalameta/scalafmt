package org.scalafmt.rewrite

import scala.annotation.tailrec
import scala.meta._
import scala.meta.tokens.Token

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.{FormatToken, FormatTokens}
import org.scalafmt.internal.{Side, SyntacticGroupOps, TreeSyntacticGroup}
import org.scalafmt.util.{InfixApp, TreeOps}

object RedundantParens extends Rewrite with FormatTokensRewrite.RuleFactory {

  private type Enclosed = (Int, Tree)

  override def enabled(implicit style: ScalafmtConfig): Boolean = true

  override def create(ftoks: FormatTokens): FormatTokensRewrite.Rule =
    new RedundantParens(ftoks)

  private def infixNeedsParens(outer: InfixApp, inner: Tree): Boolean = {
    val sgOuter = TreeSyntacticGroup(outer.all)
    val sgInner = TreeSyntacticGroup(inner)
    val side = if (outer.lhs eq inner) Side.Left else Side.Right
    SyntacticGroupOps.groupNeedsParenthesis(sgOuter, sgInner, side)
  }

}

class RedundantParens(ftoks: FormatTokens) extends FormatTokensRewrite.Rule {

  import FormatTokensRewrite._
  import RedundantParens._

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    RedundantParens.enabled

  override def onToken(implicit
      ft: FormatToken,
      style: ScalafmtConfig
  ): Option[Replacement] =
    ft.right match {
      case _: Token.LeftParen =>
        findEnclosed.flatMap { case (cnt, tree) =>
          if (okToReplaceWithCount(cnt, tree)) Some(removeToken) else None
        }
      case _ => None
    }

  override def onRight(left: Replacement, hasFormatOff: Boolean)(implicit
      ft: FormatToken,
      style: ScalafmtConfig
  ): Option[(Replacement, Replacement)] =
    ft.right match {
      case _: Token.RightParen if left.isLeft =>
        Some((left, removeToken))
      case _ => None
    }

  private def okToReplaceWithCount(numParens: Int, tree: Tree)(implicit
      style: ScalafmtConfig
  ): Boolean =
    tree match {
      case _: Term.Tuple | _: Type.Tuple | _: Lit.Unit => numParens >= 3

      case _ if numParens >= 2 => true

      case _: Term.AnonymousFunction | _: Term.Param => false

      case b @ (_: Term.Block | _: Term.PartialFunction)
          if b.tokens.headOption.exists(_.is[Token.LeftBrace]) =>
        b.parent.forall {
          case p: Term.Apply => p.fun.eq(b) || p.args.lengthCompare(1) == 0
          case p: Term.ApplyInfix => p.lhs.eq(b) || p.args.lengthCompare(1) == 0
          case _ => false
        }

      case t =>
        t.parent.forall {
          case TreeOps.SplitCallIntoParts(_, args) =>
            !args.fold(_.contains(t), _.exists(_.contains(t)))
          case TreeOps.SplitAssignIntoParts((body, _)) =>
            body.eq(t) && (t match {
              case InfixApp(ia) => !breaksBeforeOp(ia)
              case _ => true
            })
          case _: Enumerator.Guard => RewriteCtx.isPostfixExpr(t)
          case p: Case =>
            p.cond.contains(t) && RewriteCtx.isPostfixExpr(t)
          case p: Term.While =>
            style.dialect.allowSignificantIndentation && p.expr == t &&
            ftoks.tokenBefore(p.body).left.is[Token.KwDo]
          case p: Term.If =>
            style.dialect.allowSignificantIndentation && p.cond == t &&
            ftoks.tokenBefore(p.thenp).left.is[Token.KwThen]
          case p =>
            t match {
              case _: Lit | _: Name | _: Term.Interpolate => true
              case _: Term.PartialFunction | _: Term.Apply => true
              case t: Term.Match if style.dialect.allowMatchAsOperator =>
                !p.is[Term.ApplyInfix] ||
                ftoks.tokenAfter(t.expr).right.is[Token.Dot] &&
                ftoks.tokenBefore(t.cases).left.is[Token.LeftBrace]
              case _ =>
                p match {
                  case InfixApp(pia) => !infixNeedsParens(pia, t)
                  case _ => false
                }
            }
        }
    }

  private def breaksBeforeOpAndNotEnclosed(ia: InfixApp): Boolean = {
    val allToks = ia.all.tokens
    !ftoks.areMatching(allToks.head)(allToks.last) && breaksBeforeOp(ia)
  }

  private def breaksBeforeOp(ia: InfixApp): Boolean = {
    val beforeOp = ftoks.tokenJustBefore(ia.op)
    ftoks.prevNonCommentSameLine(beforeOp).hasBreak || (ia.lhs match {
      case InfixApp(lhsApp) if breaksBeforeOpAndNotEnclosed(lhsApp) => true
      case _ =>
        ia.rhs match {
          case Seq(InfixApp(rhsApp)) => breaksBeforeOpAndNotEnclosed(rhsApp)
          case _ => false
        }
    })
  }

  private def findEnclosed(implicit ft: FormatToken): Option[Enclosed] = {
    // counts consecutive parent pairs starting with the given one as the innermost
    // the parens could belong to tree they are enclosing, or its parent
    @tailrec
    def iter(lt: FormatToken, rt: FormatToken, cnt: Int): Option[Enclosed] =
      (ftoks.prevNonComment(lt), ftoks.nextNonComment(rt)) match {
        case (
              prev @ FormatToken(_: Token.LeftParen, _, _),
              next @ FormatToken(_, _: Token.RightParen, _)
            ) =>
          iter(ftoks.prev(prev), ftoks.next(next), cnt + 1)
        case _ =>
          TreeOps
            .findEnclosedBetweenParens(lt.right, rt.left, ft.meta.rightOwner)
            .map((cnt, _))
      }

    ftoks.matchingOpt(ft.right).flatMap { rt => iter(ft, ftoks.after(rt), 1) }
  }

}
