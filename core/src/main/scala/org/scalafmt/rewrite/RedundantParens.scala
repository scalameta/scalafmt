package org.scalafmt.rewrite

import scala.meta.Tree
import scala.meta._
import scala.meta.tokens.Token.{LeftParen, RightParen}

object RedundantParens extends Rewrite {

  def isWrappedInParens(tokens: Tokens): Boolean =
    tokens.nonEmpty && tokens.head.is[LeftParen] && tokens.last.is[RightParen]

  override def rewrite(code: Tree, ctx: RewriteCtx): Seq[Patch] = {
    val builder = Seq.newBuilder[Patch]

    code.collect {
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
          builder += Patch(leftSegment.head, leftSegment.last, "")
          builder += Patch(rightSegment.head, rightSegment.last, "")
        }
    }

    builder.result()
  }
}
