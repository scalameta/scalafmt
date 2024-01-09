package org.scalafmt.rewrite

import scala.annotation.tailrec
import scala.meta._
import scala.meta.internal.trees.PlaceholderChecks.hasPlaceholder

import org.scalafmt.config.RewriteSettings
import org.scalafmt.util.InfixApp

object AvoidInfix extends RewriteFactory {

  override def hasChanged(v1: RewriteSettings, v2: RewriteSettings): Boolean =
    v2.avoidInfix ne v1.avoidInfix

  override def create(implicit ctx: RewriteCtx): RewriteSession =
    new AvoidInfix

}

class AvoidInfix(implicit ctx: RewriteCtx) extends RewriteSession {

  private val matcher = ctx.style.rewrite.avoidInfix

  // In a perfect world, we could just use
  // Tree.transform {
  //   case t: Term.ApplyInfix => Term.Apply(Term.Select(t.lhs, t.op), t.args)
  // }
  // and be done with it. However, until transform becomes token aware (see https://github.com/scalameta/scalameta/pull/457)
  // we will do these dangerous rewritings by hand.

  override def rewrite(tree: Tree): Unit =
    tree match {
      case (x: Term.ApplyInfix) if checkMatchingInfix(x) =>
        val builder = Seq.newBuilder[TokenPatch]

        val (opHead, opLast) = ends(x.op)
        builder += TokenPatch.AddLeft(opHead, ".", keepTok = true)

        def moveOpenDelim(prev: Token, open: Token): Unit = {
          // move delimiter (before comment or newline)
          builder += TokenPatch.AddRight(prev, open.syntax, keepTok = true)
          builder += TokenPatch.Remove(open)
        }

        // move the left bracket if targs
        val beforeLp =
          if (x.targClause.values.isEmpty) opLast
          else {
            val (targsHead, targsLast) = ends(x.targClause)
            moveOpenDelim(opLast, targsHead)
            targsLast
          }
        // move the left paren if enclosed, else enclose
        val (argsHead, argsLast) = ends(x.argClause)
        if (ctx.getMatchingOpt(argsHead).exists(argsLast.end <= _.end))
          moveOpenDelim(beforeLp, argsHead)
        else {
          builder += TokenPatch.AddRight(beforeLp, "(", keepTok = true)
          builder += TokenPatch.AddRight(argsLast, ")", keepTok = true)
        }

        val (lhsHead, lhsLast) = ends(x.lhs)
        val shouldWrapLhs = !isWrapped(lhsHead, lhsLast) && (x.lhs match {
          case y: Term.ApplyInfix => !checkMatchingInfix(y)
          // foo _ compose bar => (foo _).compose(bar)
          // new Foo compose bar => (new Foo).compose(bar)
          case _: Term.Eta | _: Term.New => true
          case _ => false
        })
        if (shouldWrapLhs) {
          builder += TokenPatch.AddLeft(lhsHead, "(", keepTok = true)
          builder += TokenPatch.AddRight(lhsLast, ")", keepTok = true)
        }

        ctx.addPatchSet(builder.result(): _*)

      case _ =>
    }

  @tailrec
  private def checkMatchingInfix(ai: Term.ApplyInfix): Boolean = {
    val op = ai.op.value
    InfixApp.isLeftAssoc(op) && matcher.matches(ai.lhs.text, op) &&
    (ai.argClause match {
      case ac @ Term.ArgClause(arg :: Nil, _) if !isWrapped(ac) =>
        !hasPlaceholder(arg, ctx.style.rewrite.isAllowInfixPlaceholderArg)
      case _ => true
    }) && (ai.lhs match {
      case lhs: Term.ApplyInfix if hasPlaceholder(lhs, true) =>
        isWrapped(lhs) || checkMatchingInfix(lhs)
      case _ => true
    })
  }

  @inline
  private def isMatching(head: Token, last: => Token): Boolean =
    head.is[Token.LeftParen] && ctx.isMatching(head, last)

  private def isWrapped(head: Token, last: Token): Boolean =
    isMatching(head, last) ||
      ctx.tokenTraverser.prevNonTrivialToken(head).exists {
        isMatching(_, nextNonTrivial(last).orNull)
      }

  private def ends(t: Tree): (Token, Token) = {
    val tokens = t.tokens
    (tokens.head, tokens.last)
  }

  private def isWrapped(t: Tree): Boolean = {
    val (head, last) = ends(t)
    isWrapped(head, last)
  }

  @inline
  private def nextNonTrivial(token: Token): Option[Token] =
    ctx.tokenTraverser.nextNonTrivialToken(token)

}
