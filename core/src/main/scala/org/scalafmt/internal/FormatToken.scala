package org.scalafmt.internal

import org.scalafmt.util.TokenOps._
import scala.collection.mutable
import scala.meta.tokens.Token
import scala.meta.tokens.Token.Whitespace
import scala.meta.tokens.Tokens

/**
  * Two adjacent non-whitespace tokens.
  *
  * Consider a FormatToken as a node in a search graph and [[Split]]
  * are the edges. The format tokens remain unchanged after formatting,
  * while the splits are changed.
  *
  * @param left The left non-whitespace token.
  * @param right The left non-whitespace token.
  * @param between The whitespace tokens between left and right.
  */
case class FormatToken(left: Token,
                       right: Token,
                       between: Vector[Whitespace]) {

  override def toString = s"${left.code}∙${right.code}"

  def inside(range: Set[Range]): Boolean = {
    if (range.isEmpty) true
    else range.exists(_.contains(right.position.end.line))
  }

  /**
    * A format token is uniquely identified by its left token.
    */
  override def hashCode(): Int = hash(left).##
}

object FormatToken {

  /**
    * Convert scala.meta Tokens to FormatTokens.
    *
    * Since tokens might be very large, we try to allocate as
    * little memory as possible.
    */
  def formatTokens(tokens: Tokens): Array[FormatToken] = {
    var left = tokens.head
    val result = Array.newBuilder[FormatToken]
    val whitespace = Vector.newBuilder[Whitespace]
    tokens.toArray.foreach {
      case t: Whitespace => whitespace += t
      case right =>
        val tok = FormatToken(left, right, whitespace.result)
        result += tok
        left = right
        whitespace.clear()
    }
    result.result
  }
}
