package org.scalafmt.rewrite

import scala.annotation.tailrec
import scala.meta._
import scala.meta.internal.trees.PlaceholderChecks

import org.scalafmt.config.FilterMatcher
import org.scalafmt.config.RewriteSettings
import org.scalafmt.util.InfixApp

object AvoidInfix extends RewriteFactory {

  override def hasChanged(v1: RewriteSettings, v2: RewriteSettings): Boolean =
    v2.neverInfix ne v1.neverInfix

  override def create(implicit ctx: RewriteCtx): RewriteSession =
    new AvoidInfix

  def getMatcher(ctx: RewriteCtx): FilterMatcher =
    ctx.style.rewrite.neverInfix.matcher

  def getMatcherIfEnabled(ctx: RewriteCtx): Option[FilterMatcher] =
    if (ctx.style.rewrite.rules.contains(AvoidInfix)) Some(getMatcher(ctx))
    else None

}

class AvoidInfix(implicit ctx: RewriteCtx) extends RewriteSession {

  private val matcher = AvoidInfix.getMatcher(ctx)

  // In a perfect world, we could just use
  // Tree.transform {
  //   case t: Term.ApplyInfix => Term.Apply(Term.Select(t.lhs, t.op), t.args)
  // }
  // and be done with it. However, until transform becomes token aware (see https://github.com/scalameta/scalameta/pull/457)
  // we will do these dangerous rewritings by hand.

  override def rewrite(tree: Tree): Unit =
    tree match {
      case x @ Term.ApplyInfix(lhs, op, targs, args) if checkMatchingInfix(x) =>
        val builder = Seq.newBuilder[TokenPatch]

        val (opHead, opLast) = ends(op)
        builder += TokenPatch.AddLeft(opHead, ".", keepTok = true)
        val opNextOpt = nextNonTrivial(opLast)

        def moveOpenDelim(prev: Token, open: Token): Unit = {
          // move delimiter (before comment or newline)
          builder += TokenPatch.AddRight(prev, open.syntax, keepTok = true)
          builder += TokenPatch.Remove(open)
        }

        // move the left bracket if targs
        val argTuple =
          if (targs.isEmpty)
            for {
              opNext <- opNextOpt
            } yield (opLast, opNext)
          else
            for {
              lb <- opNextOpt
              rb <- ctx.getMatchingOpt(lb)
              rbNext <- {
                moveOpenDelim(opLast, lb)
                nextNonTrivial(rb)
              }
            } yield (rb, rbNext)
        // move the left paren if enclosed, else enclose
        argTuple.foreach { case (prev, lp) =>
          val isEnclosed = args.lastOption.forall { arg =>
            val last = arg.tokens.last
            val isEnclosed = ctx.getMatchingOpt(lp).exists(last.end <= _.end)
            if (!isEnclosed) {
              builder += TokenPatch.AddRight(prev, "(", keepTok = true)
              builder += TokenPatch.AddRight(last, ")", keepTok = true)
            }
            isEnclosed
          }
          if (isEnclosed) moveOpenDelim(prev, lp)
        }

        val (lhsHead, lhsLast) = ends(lhs)
        val shouldWrapLhs = !isWrapped(lhsHead, lhsLast) && (lhs match {
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
    InfixApp.isLeftAssoc(op) && matcher.matches(op) && (ai.args match {
      case arg :: Nil if !isWrapped(arg) =>
        !ctx.style.rewrite.allowInfixPlaceholderArg &&
        PlaceholderChecks.isPlaceholder(arg) ||
        !PlaceholderChecks.hasPlaceholder(arg)
      case _ => true
    }) && (ai.lhs match {
      case lhs: Term.ApplyInfix if PlaceholderChecks.hasPlaceholder(lhs) =>
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
