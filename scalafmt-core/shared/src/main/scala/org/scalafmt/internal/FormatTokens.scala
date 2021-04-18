package org.scalafmt.internal

import scala.annotation.tailrec
import scala.meta.Tree
import scala.meta.tokens.Token
import scala.meta.tokens.Tokens

import org.scalafmt.util.TokenOps
import org.scalafmt.util.Whitespace

class FormatTokens(val arr: Array[FormatToken])
    extends IndexedSeq[FormatToken] {

  private val leftTok2tok: Map[TokenOps.TokenHash, Int] = {
    val result = Map.newBuilder[TokenOps.TokenHash, Int]
    result.sizeHint(arr.length)
    arr.foreach(t => result += FormatTokens.thash(t.left) -> t.meta.idx)
    result += FormatTokens.thash(arr.last.right) -> arr.last.meta.idx
    result.result()
  }

  override def length: Int = arr.length
  override def apply(idx: Int): FormatToken = arr(idx)

  private def get(tok: Token, isBefore: Boolean): FormatToken = tok match {
    case _: Token.BOF => arr.head
    case _: Token.EOF => arr.last
    case _ =>
      val idx = leftTok2tok(FormatTokens.thash(tok))
      val ft = arr(idx)
      if (isBefore) {
        if (ft.left.start <= tok.start) ft else at(idx - 1)
      } else {
        if (ft.left.start >= tok.start) ft else at(idx + 1)
      }
  }

  def at(off: Int): FormatToken =
    if (off < 0) arr.head else if (off < arr.length) arr(off) else arr.last

  @inline def before(tok: Token): FormatToken = get(tok, true)
  @inline def after(tok: Token): FormatToken = get(tok, false)
  @inline def apply(tok: Token): FormatToken = before(tok)

  /** If the token is missing:
    * - to go backward, start from the next token
    * - to go forward, start from the previous token
    * This ensures that the next token in the direction of search is counted.
    */
  def apply(tok: Token, off: Int): FormatToken =
    apply(if (off < 0) after(tok) else before(tok), off)
  def apply(ft: FormatToken, off: Int): FormatToken = at(ft.meta.idx + off)

  @inline def hasNext(ft: FormatToken): Boolean = ft.meta.idx < (arr.length - 1)
  @inline def hasPrev(ft: FormatToken): Boolean = ft.meta.idx > 0

  @inline def prev(ft: FormatToken): FormatToken = apply(ft, -1)
  @inline def next(ft: FormatToken): FormatToken = apply(ft, 1)

  @tailrec
  final def findTokenWith[A](
      ft: FormatToken,
      iter: FormatToken => FormatToken
  )(f: FormatToken => Option[A]): Either[FormatToken, A] =
    f(ft) match {
      case Some(a) => Right(a)
      case _ =>
        val nextFt = iter(ft)
        if (nextFt eq ft) Left(ft)
        else findTokenWith(nextFt, iter)(f)
    }

  final def findToken(
      ft: FormatToken,
      iter: FormatToken => FormatToken
  )(f: FormatToken => Boolean): Either[FormatToken, FormatToken] =
    findTokenWith(ft, iter)(Some(_).filter(f))

  final def nextNonCommentSameLine(curr: FormatToken): FormatToken =
    findToken(curr, next)(ft => ft.hasBreak || !ft.right.is[Token.Comment])
      .fold(identity, identity)

  final def nextNonComment(curr: FormatToken): FormatToken =
    findToken(curr, next)(!_.right.is[Token.Comment]).fold(identity, identity)

  final def nextNonComment(curr: FormatToken.Meta): FormatToken =
    nextNonComment(arr(curr.idx))

  final def prevNonCommentSameLine(curr: FormatToken): FormatToken =
    findToken(curr, prev)(ft => ft.hasBreak || !ft.left.is[Token.Comment])
      .fold(identity, identity)

  final def prevNonComment(curr: FormatToken): FormatToken =
    findToken(curr, prev)(!_.left.is[Token.Comment]).fold(identity, identity)

  def getLast(tree: Tree): FormatToken =
    apply(TokenOps.findLastVisibleToken(tree.tokens))

  def getLastOpt(tree: Tree): Option[FormatToken] =
    TokenOps.findLastVisibleTokenOpt(tree.tokens).map(apply)

  def getLastNonTrivial(tree: Tree): FormatToken =
    apply(TokenOps.findLastNonTrivialToken(tree.tokens))

  def getLastNonTrivialOpt(tree: Tree): Option[FormatToken] =
    TokenOps.findLastNonTrivialTokenOpt(tree.tokens).map(apply)

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
    var fmtWasOff = false
    val arr = tokens.toArray
    def process(right: Token): Unit = {
      val rmeta = FormatToken.TokenMeta(owner(right), right.syntax)
      if (left eq null) {
        fmtWasOff = TokenOps.isFormatOff(right)
      } else {
        val between = arr.slice(wsIdx, tokIdx)
        val fmtIsOff = fmtWasOff || TokenOps.isFormatOff(right)
        fmtWasOff = if (fmtWasOff) !TokenOps.isFormatOn(right) else fmtIsOff
        val meta = FormatToken.Meta(between, ftIdx, fmtIsOff, lmeta, rmeta)
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

  @inline
  def thash(token: Token): TokenOps.TokenHash = TokenOps.hash(token)

}
