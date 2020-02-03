package org.scalafmt.rewrite

import scala.meta._
import scala.meta.tokens.Token.{LeftParen, RightParen}

object RedundantParens extends Rewrite {
  override def create(implicit ctx: RewriteCtx): RewriteSession =
    new RedundantParens
}

class RedundantParens(implicit ctx: RewriteCtx) extends RewriteSession {

  import ctx.dialect

  def isWrappedInParens(tokens: Tokens): Boolean =
    tokens.nonEmpty && tokens.head.is[LeftParen] && tokens.last.is[RightParen]

  override def rewrite(tree: Tree): Unit = tree match {
    case g: Enumerator.Guard =>
      val tokens: Tokens = g.cond.tokens
      if (isWrappedInParens(tokens)) {
        val openParensCount = tokens.segmentLength(_.is[LeftParen], 0)
        val closeParensCount =
          tokens.reverse.segmentLength(_.is[RightParen], 0)
        val parensToRemoveCount = Math.min(openParensCount, closeParensCount)
        val leftSegment: Tokens = tokens.slice(0, parensToRemoveCount)
        val rightSegment: Tokens =
          tokens.slice(tokens.length - parensToRemoveCount, tokens.length)
        leftSegment.zip(rightSegment.reverse).foreach {
          case (left, right) if ctx.isMatching(left, right) =>
            ctx.addPatchSet(TokenPatch.Remove(left), TokenPatch.Remove(right))
          case _ =>
        }
      }

    case t @ Term.Apply(_, List(b: Term.Block))
        if ctx.style.activeForEdition_2020_01 &&
          b.tokens.headOption.exists(_.is[Token.LeftBrace]) =>
      t.tokens.lastOption.filter(_.is[RightParen]).foreach { rparen =>
        ctx.getMatchingOpt(rparen).foreach { lparen =>
          implicit val builder = Seq.newBuilder[TokenPatch]
          builder += TokenPatch.Remove(lparen)
          builder += TokenPatch.Remove(rparen)
          ctx.removeLFToAvoidEmptyLine(lparen)
          ctx.removeLFToAvoidEmptyLine(rparen)
          ctx.addPatchSet(builder.result(): _*)
        }
      }

    case _ =>
  }

}
