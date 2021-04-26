package org.scalafmt.rewrite

import scala.annotation.tailrec
import scala.meta._
import scala.meta.tokens.Token

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.{FormatToken, FormatTokens}
import org.scalafmt.util.{InfixApp, TreeOps}

object RedundantParens extends Rewrite with FormatTokensRewrite.RuleFactory {

  override def enabled(implicit style: ScalafmtConfig): Boolean = true

  override def create(ftoks: FormatTokens): FormatTokensRewrite.Rule =
    new RedundantParens(ftoks)

}

class RedundantParens(ftoks: FormatTokens) extends FormatTokensRewrite.Rule {

  import FormatTokensRewrite._

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    RedundantParens.enabled

  override def onToken(implicit
      ft: FormatToken,
      style: ScalafmtConfig
  ): Option[Replacement] = Option {
    ft.right match {
      case _: Token.LeftParen =>
        val numParens = countParens
        if (numParens == 0)
          replaceNotEnclosed
        else
          replaceEnclosed(numParens)

      case _ => null
    }
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

  private def replaceEnclosed(
      numParens: Int
  )(implicit ft: FormatToken, style: ScalafmtConfig): Replacement = {
    val ok = ft.meta.rightOwner match {
      case _: Term.Tuple | _: Type.Tuple | _: Lit.Unit => numParens >= 3

      case _ if numParens >= 2 => true

      case t if t.parent.exists {
            case p: Term.ApplyInfix if p.lhs ne t =>
              avoidInfixMatcher.exists(_.matches(p.op.value))
            case _ => false
          } =>
        false // supersedes Term.Name below

      case _: Lit | _: Name | _: Term.Interpolate => true

      case Term.Apply(_, List(b: Term.Block))
          if b.tokens.headOption.exists(_.is[Token.LeftBrace]) =>
        true

      case t =>
        t.parent.exists {
          case TreeOps.SplitAssignIntoParts((body, _)) =>
            body.eq(t) && (t match {
              case InfixApp(ia) => !breaksBeforeOp(ia)
              case _ => true
            })
          case _: Enumerator.Guard => RewriteCtx.isPostfixExpr(t)
          case p: Case =>
            p.cond.contains(t) && RewriteCtx.isPostfixExpr(t)
          case _ => false
        }
    }
    if (ok) removeToken else null
  }

  private def replaceNotEnclosed(implicit ft: FormatToken): Replacement = {
    val ok = ft.meta.rightOwner match {
      case Term.Apply(_, List(b: Term.Block)) =>
        b.tokens.headOption.exists(_.is[Token.LeftBrace])
      case _ => false
    }
    if (ok) removeToken else null
  }

  private def avoidInfixMatcher(implicit style: ScalafmtConfig) =
    if (style.rewrite.rules.contains(AvoidInfix))
      Some(style.rewrite.neverInfix.matcher)
    else None

  private def breaksBeforeOpAndNotEnclosed(ia: InfixApp): Boolean = {
    val allToks = ia.all.tokens
    !ftoks.areMatching(allToks.head)(allToks.last) && breaksBeforeOp(ia)
  }

  private def breaksBeforeOp(ia: InfixApp): Boolean = {
    val beforeOp = ftoks(ia.op.tokens.head, -1)
    ftoks.prevNonCommentSameLine(beforeOp).hasBreak || (ia.lhs match {
      case InfixApp(lhsApp) if breaksBeforeOpAndNotEnclosed(lhsApp) => true
      case _ =>
        ia.rhs match {
          case Seq(InfixApp(rhsApp)) => breaksBeforeOpAndNotEnclosed(rhsApp)
          case _ => false
        }
    })
  }

  private def countParens(implicit ft: FormatToken): Int = {
    val tree = ft.meta.rightOwner
    val head = tree.tokens.head
    val last = tree.tokens.last
    @tailrec
    def iter(lt: FormatToken, rt: FormatToken, cnt: Int = 0): Int = {
      if (lt.right.eq(head) && rt.left.eq(last)) cnt + 1
      else {
        val prev = ftoks.prevNonComment(ftoks.prev(lt))
        if (!prev.right.is[Token.LeftParen]) 0
        else {
          val next = ftoks.nextNonComment(ftoks.next(rt))
          if (!next.left.is[Token.RightParen]) 0
          else iter(prev, next, cnt + 1)
        }
      }
    }
    ftoks.matchingOpt(ft.right).fold(0)(rt => iter(ft, ftoks.after(rt)))
  }

}
