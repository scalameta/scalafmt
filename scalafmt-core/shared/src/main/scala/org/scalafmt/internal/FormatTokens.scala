package org.scalafmt.internal

import scala.meta.Tree
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

  @inline def hasNext(ft: FormatToken): Boolean = ft.meta.idx < (arr.length - 1)
  @inline def hasPrev(ft: FormatToken): Boolean = ft.meta.idx > 0

}

object FormatTokens {

  /** Convert scala.meta Tokens to FormatTokens.
    *
    * Since tokens might be very large, we try to allocate as
    * little memory as possible.
    */
  def apply(tokens: Tokens, owner: Token => Tree): FormatTokens = {
    var left: Token = null
    var lmeta: FormatToken.TokenMeta = null
    val result = Array.newBuilder[FormatToken]
    var ftIdx = 0
    var wsIdx = 0
    var tokIdx = 0
    val arr = tokens.toArray
    def process(right: Token): Unit = {
      val rmeta = FormatToken.TokenMeta(owner(right), right.syntax)
      if (left ne null) {
        val meta =
          FormatToken.Meta(arr.slice(wsIdx, tokIdx), ftIdx, lmeta, rmeta)
        result += FormatToken(left, right, meta)
        ftIdx += 1
      }
      left = right
      lmeta = rmeta
    }
    val tokCnt = arr.length
    while (tokIdx < tokCnt)
      arr(tokIdx) match {
        case Whitespace() => tokIdx += 1
        case right =>
          process(right)
          tokIdx += 1
          wsIdx = tokIdx
      }
    new FormatTokens(result.result)
  }

}
