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

        val opHead = op.tokens.head
        builder += TokenPatch.AddLeft(opHead, ".", keepTok = true)

        val opLast = op.tokens.last
        val opNextOpt = ctx.tokenTraverser.nextNonTrivialToken(opLast)

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
                ctx.tokenTraverser.nextNonTrivialToken(rb)
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

        val shouldWrapLhs = !isWrapped(lhs) && (lhs match {
          case y: Term.ApplyInfix => !checkMatchingInfix(y)
          // foo _ compose bar => (foo _).compose(bar)
          // new Foo compose bar => (new Foo).compose(bar)
          case _: Term.Eta | _: Term.New => true
          case _ => false
        })
        if (shouldWrapLhs) {
          builder += TokenPatch.AddLeft(lhs.tokens.head, "(", keepTok = true)
          builder += TokenPatch.AddRight(lhs.tokens.last, ")", keepTok = true)
        }

        // remove parens if enclosed
        for {
          parent <- tree.parent
          if !parent.is[Term.ApplyInfix] && !parent.is[Term.Apply]
          if ctx.style.rewrite.rules.contains(RedundantParens)
          infixTokens = tree.tokens
          head <- infixTokens.headOption if head.is[Token.LeftParen]
          last <- infixTokens.lastOption if last.is[Token.RightParen]
          if ctx.isMatching(head, last)
        } yield {
          builder += TokenPatch.Remove(head)
          builder += TokenPatch.Remove(last)
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

  private def isWrapped(t: Tree): Boolean = t.tokens.head match {
    case h: Token.LeftParen => ctx.getMatchingOpt(h).contains(t.tokens.last)
    case _ => false
  }

}
