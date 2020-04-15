package org.scalafmt.rewrite

import scala.annotation.tailrec
import scala.collection.mutable
import scala.meta.tokens.Token.{LeftParen, RightParen}
import scala.meta._

object AvoidInfix extends Rewrite {
  override def create(implicit ctx: RewriteCtx): RewriteSession =
    new AvoidInfix

  private def hasPlaceholder(tree: Tree): Boolean = {
    val queue = new mutable.Queue[Tree]
    queue += tree
    @tailrec
    def iter: Boolean = queue.nonEmpty && {
      queue.dequeue() match {
        case _: Term.Placeholder => true
        case t: Term.ApplyInfix =>
          queue ++= (t.lhs +: t.args)
          iter
        case t: Term.Apply =>
          queue += t.fun
          iter
        case t: Term.Select =>
          queue += t.qual
          iter
        case _ =>
          iter
      }
    }
    iter
  }

}

class AvoidInfix(implicit ctx: RewriteCtx) extends RewriteSession {

  private val matcher = ctx.style.rewrite.neverInfix.toMatcher

  // In a perfect world, we could just use
  // Tree.transform {
  //   case t: Term.ApplyInfix => Term.Apply(Term.Select(t.lhs, t.op), t.args)
  // }
  // and be done with it. However, until transform becomes token aware (see https://github.com/scalameta/scalameta/pull/457)
  // we will do these dangerous rewritings by hand.

  override def rewrite(tree: Tree): Unit = tree match {
    case Term.ApplyInfix(lhs, op, _, args) if matcher.matches(op.value) =>
      val builder = Seq.newBuilder[TokenPatch]

      val opHead = op.tokens.head
      builder += TokenPatch.AddLeft(opHead, ".", keepTok = true)

      if (args.lengthCompare(1) == 0) { // otherwise, definitely enclosed
        val rhs = args.head
        rhs.tokens.headOption.foreach { head =>
          val last = args.head.tokens.last
          val opLast = op.tokens.last
          if (!ctx.isMatching(head, last)) {
            if (AvoidInfix.hasPlaceholder(args.head)) return
            builder += TokenPatch.AddRight(opLast, "(", keepTok = true)
            builder += TokenPatch.AddRight(last, ")", keepTok = true)
          } else if (ctx.tokenTraverser.nextToken(opLast) ne head) {
            // has comments: move delimiter
            builder += TokenPatch.AddRight(opLast, head.syntax, keepTok = true)
            builder += TokenPatch.Remove(head)
          }
        }
      }

      val shouldWrapLhs = lhs match {
        case Term.ApplyInfix(_, o, _, _)
            if !matcher.matches(o.value) && !lhs.tokens.head.is[LeftParen] =>
          if (AvoidInfix.hasPlaceholder(lhs)) return
          true
        case _: Term.Eta => true // foo _ compose bar => (foo _).compose(bar)
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
        head <- infixTokens.headOption if head.is[LeftParen]
        last <- infixTokens.lastOption if last.is[RightParen]
        if ctx.isMatching(head, last)
      } yield {
        builder += TokenPatch.Remove(head)
        builder += TokenPatch.Remove(last)
      }

      ctx.addPatchSet(builder.result(): _*)

    case _ =>
  }

}
