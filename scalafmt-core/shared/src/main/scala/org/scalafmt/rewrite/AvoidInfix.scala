package org.scalafmt.rewrite

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
      case x @ Term.ApplyInfix(lhs, op, _, args) if checkMatchingInfix(x) =>
        val builder = Seq.newBuilder[TokenPatch]

        val opHead = op.tokens.head
        builder += TokenPatch.AddLeft(opHead, ".", keepTok = true)

        args match {
          case rhs :: Nil =>
            rhs.tokens.headOption.foreach { head =>
              val last = rhs.tokens.last
              val opLast = op.tokens.last
              if (!ctx.isMatching(head, last)) {
                if (PlaceholderChecks.hasPlaceholder(rhs)) return
                builder += TokenPatch.AddRight(opLast, "(", keepTok = true)
                builder += TokenPatch.AddRight(last, ")", keepTok = true)
              } else {
                // move delimiter (before comment or newline)
                builder +=
                  TokenPatch.AddRight(opLast, head.syntax, keepTok = true)
                builder += TokenPatch.Remove(head)
              }
            }
          case _ => // otherwise, definitely enclosed
        }

        val shouldWrapLhs = lhs match {
          case y: Term.ApplyInfix if !isWrapped(y) && !checkMatchingInfix(y) =>
            if (PlaceholderChecks.hasPlaceholder(lhs)) return
            true
          // foo _ compose bar => (foo _).compose(bar)
          // new Foo compose bar => (new Foo).compose(bar)
          case _: Term.Eta | _: Term.New => true
          case _ => false
        }
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

  private def checkMatchingInfix(ai: Term.ApplyInfix): Boolean = {
    val op = ai.op.value
    InfixApp.isLeftAssoc(op) && matcher.matches(op)
  }

  private def isWrapped(t: Tree): Boolean = t.tokens.head match {
    case h: Token.LeftParen => ctx.getMatchingOpt(h).contains(t.tokens.last)
    case _ => false
  }

}
