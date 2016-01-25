package org.scalafmt

import scala.collection.mutable
import scala.meta.tokens.Token
import scala.meta.tokens.Token.Whitespace
import scala.meta.tokens.Tokens

case class FormatToken(left: Token, right: Token, between: Vector[Whitespace]) {
  override def toString = s"${left.code}∙${right.code}"
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
    val whitespace = mutable.ArrayBuilder.make[Whitespace]()
    while (i < N) {
      ts(i) match {
        case t: Whitespace =>
          whitespace += t
        case right =>
          // TODO(olafur) avoid result.toVector
          result += FormatToken(left, right, whitespace.result.toVector)
          left = right
          whitespace.clear()
      }
      i += 1
    }
    result.result
  }

}


