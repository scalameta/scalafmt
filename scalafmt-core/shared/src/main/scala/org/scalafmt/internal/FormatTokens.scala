package org.scalafmt.internal

import scala.meta.tokens.Token
import scala.meta.tokens.Tokens

import org.scalafmt.util.Whitespace

class FormatTokens(val arr: Array[FormatToken])
    extends IndexedSeq[FormatToken] {

  private val leftTok2tok: Map[Token, FormatToken] = {
    val result = Map.newBuilder[Token, FormatToken]
    result.sizeHint(arr.length)
    arr.foreach(t => result += t.left -> t)
    result += (arr.last.right -> arr.last)
    result.result()
  }

  override def length: Int = arr.length
  override def apply(idx: Int): FormatToken = arr(idx)

  def at(off: Int): FormatToken =
    if (off < 0) arr.head else if (off < arr.length) arr(off) else arr.last

  def get(tok: Token): Option[FormatToken] = leftTok2tok.get(tok)
  def apply(tok: Token): FormatToken = leftTok2tok(tok)

  def apply(tok: Token, off: Int): FormatToken = apply(apply(tok), off)
  def apply(ft: FormatToken, off: Int): FormatToken = at(ft.meta.idx + off)

}

object FormatTokens {

  /**
    * Convert scala.meta Tokens to FormatTokens.
    *
    * Since tokens might be very large, we try to allocate as
    * little memory as possible.
    */
  def apply(tokens: Tokens): FormatTokens = {
    var left = tokens.head
    val result = Array.newBuilder[FormatToken]
    var ftIdx = 0
    var wsIdx = 0
    var tokIdx = 0
    val arr = tokens.toArray
    arr.foreach {
      case Whitespace() => tokIdx += 1
      case right =>
        val meta = FormatToken.Meta(arr.slice(wsIdx, tokIdx), ftIdx)
        result += FormatToken(left, right, meta)
        left = right
        ftIdx += 1
        tokIdx += 1
        wsIdx = tokIdx
    }
    new FormatTokens(result.result)
  }

}
