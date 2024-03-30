package org.scalafmt.internal

import scala.annotation.tailrec
import scala.meta.Tree
import scala.meta.tokens.Token
import scala.meta.tokens.Tokens

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.rewrite.FormatTokensRewrite
import org.scalafmt.util._

import TokenOps._

class FormatTokens(leftTok2tok: Map[TokenHash, Int])(val arr: Array[FormatToken])
    extends IndexedSeq[FormatToken] {

  private def this(arr: Array[FormatToken]) = this {
    val result = Map.newBuilder[TokenHash, Int]
    result.sizeHint(arr.length)
    arr.foreach(t => result += FormatTokens.thash(t.left) -> t.meta.idx)
    result += FormatTokens.thash(arr.last.right) -> arr.last.meta.idx
    result.result()
  }(arr)

  private lazy val matchingParentheses: Map[TokenHash, Token] = TreeOps
    .getMatchingParentheses(arr.view.map(_.right))

  override def length: Int = arr.length
  override def apply(idx: Int): FormatToken = arr(idx)

  private def get(tok: Token, isBefore: Boolean): FormatToken = {
    val idx = leftTok2tok.getOrElse(
      FormatTokens.thash(tok),
      throw new NoSuchElementException(
        s"Missing token index [${tok.start}:${tok.end}]: `$tok`"
      )
    )
    if (idx >= arr.length) arr.last
    else {
      val ft = arr(idx)
      if (ft.left eq tok) ft
      else if (isBefore) { if (ft.left.start <= tok.start) ft else at(idx - 1) }
      else { if (ft.left.start >= tok.start) ft else at(idx + 1) }
    }
  }

  def at(off: Int): FormatToken =
    if (off < 0) arr.head else if (off < arr.length) arr(off) else arr.last

  // get token; if rewritten, the one before
  @inline
  def before(tok: Token): FormatToken = get(tok, true)
  // get token; if rewritten, the one after
  @inline
  def after(tok: Token): FormatToken = get(tok, false)
  @inline
  def apply(tok: Token): FormatToken = before(tok)

  /** If the token is missing:
    *   - to go backward, start from the next token
    *   - to go forward, start from the previous token This ensures that the
    *     next token in the direction of search is counted.
    */
  def apply(tok: Token, off: Int): FormatToken =
    apply(if (off < 0) after(tok) else before(tok), off)
  def apply(ft: FormatToken, off: Int): FormatToken = at(ft.meta.idx + off)

  @inline
  def hasNext(ft: FormatToken): Boolean = ft.meta.idx < (arr.length - 1)
  @inline
  def hasPrev(ft: FormatToken): Boolean = ft.meta.idx > 0

  @inline
  def prev(ft: FormatToken): FormatToken = apply(ft, -1)
  @inline
  def next(ft: FormatToken): FormatToken = apply(ft, 1)

  @inline
  def matching(token: Token): Token = matchingParentheses.getOrElse(
    FormatTokens.thash(token),
    throw new NoSuchElementException(
      s"Missing matching token index [${token.start}:${token.end}]: `$token`"
    )
  )
  @inline
  def matchingOpt(token: Token): Option[Token] = matchingParentheses
    .get(FormatTokens.thash(token))
  @inline
  def hasMatching(token: Token): Boolean = matchingParentheses
    .contains(FormatTokens.thash(token))
  @inline
  def areMatching(t1: Token)(t2: => Token): Boolean = matchingOpt(t1) match {
    case Some(x) => x eq t2
    case _ => false
  }

  def getDelimsIfEnclosed(
      tokens: Tokens,
      tree: Tree
  ): Option[(FormatToken, FormatToken)] = getHeadOpt(tokens, tree)
    .flatMap { head =>
      matchingOpt(head.left).flatMap { other =>
        val last = getLastNonTrivial(tokens, tree)
        if (last.left eq other) Some((head, last)) else None
      }
    }
  def getDelimsIfEnclosed(tree: Tree): Option[(FormatToken, FormatToken)] =
    getDelimsIfEnclosed(tree.tokens, tree)

  def getHeadIfEnclosed(tokens: Tokens, tree: Tree): Option[FormatToken] =
    getDelimsIfEnclosed(tokens, tree).map(_._1)
  def getHeadIfEnclosed(tree: Tree): Option[FormatToken] =
    getDelimsIfEnclosed(tree).map(_._1)

  def getLastIfEnclosed(tokens: Tokens, tree: Tree): Option[FormatToken] =
    getDelimsIfEnclosed(tokens, tree).map(_._2)
  def getLastIfEnclosed(tree: Tree): Option[FormatToken] =
    getDelimsIfEnclosed(tree).map(_._2)

  def isEnclosedInMatching(tokens: Tokens, tree: Tree): Boolean =
    getHeadIfEnclosed(tokens, tree).isDefined
  def isEnclosedInMatching(tree: Tree): Boolean =
    isEnclosedInMatching(tree.tokens, tree)

  @inline
  private def areMatchingParens(close: Token)(open: => Token): Boolean = close
    .is[Token.RightParen] && areMatching(close)(open)

  def isEnclosedInParens(tree: Tree): Boolean = getClosingIfInParens(tree)
    .isDefined

  def getClosingIfInParens(
      last: FormatToken
  )(head: FormatToken): Option[FormatToken] =
    if (areMatchingParens(last.left)(head.left)) Some(prev(last))
    else {
      val afterLast = nextNonComment(last)
      if (areMatchingParens(afterLast.right)(prevNonCommentBefore(head).left))
        Some(afterLast)
      else None
    }
  def getClosingIfInParens(tree: Tree): Option[FormatToken] = {
    val tokens = tree.tokens
    getHeadOpt(tokens, tree)
      .flatMap(getClosingIfInParens(getLastNonTrivial(tokens, tree)))
  }

  def getLastExceptParen(tree: Tree): FormatToken = {
    val tokens = tree.tokens
    val last = getLastNonTrivial(tokens, tree)
    getClosingIfInParens(last)(getHead(tokens, tree)).getOrElse(last)
  }

  @tailrec
  final def findTokenWith[A](ft: FormatToken, iter: FormatToken => FormatToken)(
      f: FormatToken => Option[A]
  ): Either[FormatToken, A] = f(ft) match {
    case Some(a) => Right(a)
    case _ =>
      val nextFt = iter(ft)
      if (nextFt eq ft) Left(ft) else findTokenWith(nextFt, iter)(f)
  }

  final def findToken(ft: FormatToken, iter: FormatToken => FormatToken)(
      f: FormatToken => Boolean
  ): FormatToken = findTokenWith(ft, iter)(Some(_).filter(f)).merge

  final def nextNonCommentSameLine(curr: FormatToken): FormatToken =
    findToken(curr, next)(ft => ft.hasBreak || !ft.right.is[Token.Comment])

  final def nextNonComment(curr: FormatToken): FormatToken =
    findToken(curr, next)(!_.right.is[Token.Comment])

  final def nextNonComment(curr: FormatToken.Meta): FormatToken =
    nextNonComment(arr(curr.idx))

  @inline
  final def nextNonCommentAfter(curr: FormatToken): FormatToken =
    nextNonComment(next(curr))

  @inline
  final def nextAfterNonComment(curr: FormatToken): FormatToken =
    next(nextNonComment(curr))

  final def prevNonCommentSameLine(curr: FormatToken): FormatToken =
    findToken(curr, prev)(ft => ft.hasBreak || !ft.left.is[Token.Comment])

  final def prevNonComment(curr: FormatToken): FormatToken =
    findToken(curr, prev)(!_.left.is[Token.Comment])

  @inline
  final def prevNonCommentBefore(curr: FormatToken): FormatToken =
    prevNonComment(prev(curr))

  @inline
  final def prevNonCommentSameLineBefore(curr: FormatToken): FormatToken =
    prevNonCommentSameLine(prev(curr))

  @tailrec
  final def getOnOrBeforeOwned(ft: FormatToken, tree: Tree): FormatToken = {
    val prevFt = prevNonCommentBefore(ft)
    if (prevFt == ft || prevFt.meta.leftOwner != tree) ft
    else getOnOrBeforeOwned(prevFt, tree)
  }

  @tailrec
  final def getOnOrAfterOwned(ft: FormatToken, tree: Tree): FormatToken = {
    val nextFt = nextAfterNonComment(ft)
    if (nextFt == ft || nextFt.meta.leftOwner != tree) ft
    else getOnOrAfterOwned(nextFt, tree)
  }

  def getHead(tokens: Tokens, tree: Tree): FormatToken =
    getOnOrBeforeOwned(after(tokens.head), tree)
  @inline
  def getHead(tree: Tree): FormatToken = getHead(tree.tokens, tree)

  def getHeadOpt(tokens: Tokens, tree: Tree): Option[FormatToken] = tokens
    .headOption.map(x => getOnOrBeforeOwned(after(x), tree))
  @inline
  def getHeadOpt(tree: Tree): Option[FormatToken] = getHeadOpt(tree.tokens, tree)

  def getLast(tokens: Tokens, tree: Tree): FormatToken =
    getOnOrAfterOwned(apply(findLastVisibleToken(tokens)), tree)
  @inline
  def getLast(tree: Tree): FormatToken = getLast(tree.tokens, tree)

  def getLastOpt(tokens: Tokens, tree: Tree): Option[FormatToken] =
    findLastVisibleTokenOpt(tokens).map(x => getOnOrAfterOwned(apply(x), tree))
  @inline
  def getLastOpt(tree: Tree): Option[FormatToken] = getLastOpt(tree.tokens, tree)

  def getLastNonTrivial(tokens: Tokens, tree: Tree): FormatToken =
    prevNonComment(getLast(tokens, tree))
  def getLastNonTrivial(tree: Tree): FormatToken =
    getLastNonTrivial(tree.tokens, tree)

  def getLastNonTrivialOpt(tokens: Tokens, tree: Tree): Option[FormatToken] =
    getLastOpt(tokens, tree).map(prevNonComment)
  def getLastNonTrivialOpt(tree: Tree): Option[FormatToken] =
    getLastNonTrivialOpt(tree.tokens, tree)

  /* the following methods return the first format token such that
   * its `right` is after the parameter and is not a comment */
  @inline
  def tokenAfter(token: Token): FormatToken = nextNonComment(before(token))
  @inline
  def tokenAfter(tree: Tree): FormatToken = nextNonComment(getLast(tree))
  @inline
  def tokenAfter(trees: Seq[Tree]): FormatToken = tokenAfter(trees.last)

  def tokenAfterOpt(tree: Tree): Option[FormatToken] = getLastOpt(tree)
    .map(nextNonComment)
  def tokenAfterOpt(trees: Seq[Tree]): Option[FormatToken] = trees.lastOption
    .flatMap(tokenAfterOpt)

  /* the following methods return the last format token such that
   * its `left` is before the parameter */
  @inline
  def justBefore(token: Token): FormatToken = apply(token, -1)
  @inline
  def tokenJustBefore(tree: Tree): FormatToken = prev(getHead(tree))

  def tokenJustBeforeOpt(tree: Tree): Option[FormatToken] = getHeadOpt(tree)
    .map(prev)
  def tokenJustBeforeOpt(trees: Seq[Tree]): Option[FormatToken] = trees
    .headOption.flatMap(tokenJustBeforeOpt)

  /* the following methods return the last format token such that
   * its `left` is before the parameter and is not a comment */
  @inline
  def tokenOnOrBefore(token: Token): FormatToken = prevNonComment(before(token))
  @inline
  def tokenBefore(token: Token): FormatToken = prevNonComment(justBefore(token))
  @inline
  def tokenBefore(tree: Tree): FormatToken =
    prevNonComment(tokenJustBefore(tree))
  @inline
  def tokenBefore(trees: Seq[Tree]): FormatToken = tokenBefore(trees.head)

  def tokenBeforeOpt(tree: Tree): Option[FormatToken] = tokenJustBeforeOpt(tree)
    .map(prevNonComment)
  def tokenBeforeOpt(trees: Seq[Tree]): Option[FormatToken] = trees.headOption
    .flatMap(tokenBeforeOpt)

  @inline
  def isBreakAfterRight(ft: FormatToken): Boolean = next(ft).hasBreakOrEOF

  @inline
  def isRightCommentThenBreak(ft: FormatToken): Boolean = ft.right
    .is[Token.Comment] && isBreakAfterRight(ft)

  @inline
  def isRightLikeSingleLineComment(ft: FormatToken): Boolean =
    isRightCommentThenBreak(ft) && !ft.rightHasNewline

  @inline
  def isAttachedCommentThenBreak(ft: FormatToken): Boolean = ft.noBreak &&
    isRightCommentThenBreak(ft)

}

object FormatTokens {

  /** Convert scala.meta Tokens to FormatTokens.
    *
    * Since tokens might be very large, we try to allocate as little memory as
    * possible.
    */
  def apply(tokens: Tokens, owner: Token => Tree)(implicit
      style: ScalafmtConfig
  ): (FormatTokens, StyleMap) = {
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
      if (left eq null) { fmtWasOff = isFormatOff(right) }
      else {
        val between = arr.slice(wsIdx, tokIdx)
        val fmtIsOff = fmtWasOff || isFormatOff(right)
        fmtWasOff = if (fmtWasOff) !isFormatOn(right) else fmtIsOff
        val meta = FormatToken.Meta(between, ftIdx, fmtIsOff, lmeta, rmeta)
        result += FormatToken(left, right, meta)
        ftIdx += 1
      }
      left = right
      lmeta = rmeta
    }
    val tokCnt = arr.length
    while (tokIdx < tokCnt) arr(tokIdx) match {
      case _: Token.Whitespace => tokIdx += 1
      case right =>
        process(right)
        tokIdx += 1
        wsIdx = tokIdx
    }

    val ftoks = new FormatTokens(result.result)
    val styleMap = new StyleMap(ftoks, style)

    FormatTokensRewrite(ftoks, styleMap) -> styleMap
  }

  @inline
  def thash(token: Token): TokenHash = hash(token)

}
