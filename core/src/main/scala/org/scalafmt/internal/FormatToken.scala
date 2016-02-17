package org.scalafmt.internal

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

  def inside(range: Int => Boolean): Boolean = {
    range(right.position.end.line)
  }
}

object FormatToken {

  /**
    * Convert scala.meta Tokens to FormatTokens.
    *
    * Since tokens might be very large, we try to allocate as
    * little memory as possible.
    */
  def formatTokens(tokens: Tokens): Array[FormatToken] = {
    val N = tokens.length
    require(N > 1)
    var i = 1
    var left = tokens.head
    val ts = tokens.toArray
    val result = mutable.ArrayBuilder.make[FormatToken]
    result.sizeHint(N)
    val whitespace = mutable.ArrayBuilder.make[Whitespace]()
    while (i < N) {
      ts(i) match {
        case t: Whitespace =>
          whitespace += t
        case right =>
          // TODO(olafur) avoid result.toVector
          val tok = FormatToken(left, right, whitespace.result.toVector)
          result += tok
          left = right
          whitespace.clear()
      }
      i += 1
    }
    result.result
  }

}


