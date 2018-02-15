package org.scalafmt.rewrite

import scala.meta.Tree
import scala.meta._
import scala.meta.tokens.Token.{LeftParen, RightParen}

case object RedundantParens extends Rewrite {

  def isWrappedInParens(tokens: Tokens): Boolean =
    tokens.nonEmpty && tokens.head.is[LeftParen] && tokens.last.is[RightParen]

  def hasBalancedParens(tokens: Tokens): Boolean =
    tokens
      .foldLeft((0, true)) {
        case ((acc, false), _) => (acc, false)
        case ((acc, true), token) if token.is[LeftParen] => (acc + 1, true)
        case ((acc, true), token) if token.is[RightParen] =>
          if (acc - 1 >= 0) { (acc - 1, true) } else { (acc, false) }
        case ((acc, true), _) => (acc, true)
      }
      ._2

  override def rewrite(code: Tree, ctx: RewriteCtx): Seq[Patch] = {
    val builder = Seq.newBuilder[Patch]
    import ctx.dialect
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
          val innerSegment: Tokens = tokens.slice(
            parensToRemoveCount,
            tokens.length - parensToRemoveCount)
          if (hasBalancedParens(innerSegment)) {
            leftSegment.foreach(tok => builder += TokenPatch.Remove(tok))
            rightSegment.foreach(tok => builder += TokenPatch.Remove(tok))
          }
        }
    }
    builder.result()
  }
}
