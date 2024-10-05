package org.scalafmt.internal

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.rewrite.FormatTokensRewrite
import org.scalafmt.util._

import scala.meta.Tree
import scala.meta.tokens.Token
import scala.meta.tokens.Tokens

import scala.annotation.tailrec

import TokenOps._

class FormatTokens(leftTok2tok: Map[TokenHash, Int])(val arr: Array[FormatToken])
    extends IndexedSeq[FormatToken] {

  private def this(arr: Array[FormatToken]) = this {
    val result = new FormatTokens.TokenToIndexMapBuilder
    result.sizeHint(arr.length)
    arr.foreach(t => result.add(t.meta.idx)(t.left))
    result.add(arr.last.meta.idx)(arr.last.right)
    result.result()
  }(arr)

  private lazy val matchingParentheses: Map[TokenHash, FormatToken] = TreeOps
    .getMatchingParentheses(arr.view)(_.left)

  override def length: Int = arr.length
  override def apply(idx: Int): FormatToken = arr(idx)

  private def getAt(tok: Token, isBefore: Boolean)(idx: Int): FormatToken =
    if (idx >= arr.length) arr.last
    else {
      val ft = arr(idx)
      if (ft.left eq tok) ft
      else if (isBefore) if (ft.left.start <= tok.start) ft else at(idx - 1)
      else if (ft.left.start >= tok.start) ft
      else at(idx + 1)
    }

  private def get(tok: Token, isBefore: Boolean): FormatToken =
    getAt(tok, isBefore)(leftTok2tok.getOrElse(
      FormatTokens.thash(tok),
      FormatTokens.throwNoToken(tok, "Missing token index"),
    ))

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
  def matching(token: Token): FormatToken = matchingParentheses.getOrElse(
    FormatTokens.thash(token),
    FormatTokens.throwNoToken(token, "Missing matching token index"),
  )
  @inline
  def matchingOpt(token: Token): Option[FormatToken] = matchingParentheses
    .get(FormatTokens.thash(token))
  @inline
  def hasMatching(token: Token): Boolean = matchingParentheses
    .contains(FormatTokens.thash(token))
  @inline
  def areMatching(t1: Token)(t2: => Token): Boolean = matchingOpt(t1) match {
    case Some(x) => x.left eq t2
    case _ => false
  }

  def getHeadAndLastIfEnclosed(
      tokens: Tokens,
      tree: Tree,
  ): Option[(FormatToken, Option[FormatToken])] = getHeadOpt(tokens, tree)
    .map { head =>
      head -> matchingOpt(head.left).flatMap { other =>
        val last = getLastNonTrivial(tokens, tree)
        if (last eq other) Some(last) else None
      }
    }
  def getHeadAndLastIfEnclosed(
      tree: Tree,
  ): Option[(FormatToken, Option[FormatToken])] =
    getHeadAndLastIfEnclosed(tree.tokens, tree)

  def getDelimsIfEnclosed(
      tokens: Tokens,
      tree: Tree,
  ): Option[(FormatToken, FormatToken)] = getHeadAndLastIfEnclosed(tokens, tree)
    .flatMap { case (head, lastOpt) => lastOpt.map(last => (head, last)) }
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
  def getBracesIfEnclosed(tree: Tree): Option[(FormatToken, FormatToken)] =
    getDelimsIfEnclosed(tree).filter(_._1.left.is[Token.LeftBrace])

  @inline
  def isEnclosedInBraces(tree: Tree): Boolean = getDelimsIfEnclosed(tree)
    .exists(_._1.left.is[Token.LeftBrace])

  @inline
  private def areMatchingParens(close: Token)(open: => Token): Boolean = close
    .is[Token.RightParen] && areMatching(close)(open)

  def isEnclosedInParens(tree: Tree): Boolean = getClosingIfInParens(tree)
    .isDefined

  def getClosingIfInParens(
      last: FormatToken,
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

  final def findTokenWith[A](ft: FormatToken, iter: FormatToken => FormatToken)(
      f: FormatToken => Option[A],
  ): Either[FormatToken, A] = findTokenEx(ft)(xft => f(xft).toRight(iter(xft)))

  @tailrec
  final def findTokenEx[A](ft: FormatToken)(
      f: FormatToken => Either[FormatToken, A],
  ): Either[FormatToken, A] = f(ft) match {
    case null => Left(ft)
    case Left(nextFt) if nextFt ne ft => findTokenEx(nextFt)(f)
    case x => x
  }

  final def findToken(ft: FormatToken, iter: FormatToken => FormatToken)(
      f: FormatToken => Boolean,
  ): FormatToken = findTokenWith(ft, iter)(Some(_).filter(f)).merge

  final def nextNonCommentSameLine(curr: FormatToken): FormatToken =
    findToken(curr, next)(ft => ft.hasBreak || !ft.right.is[Token.Comment])

  final def nextNonCommentSameLineAfter(curr: FormatToken): FormatToken =
    nextNonCommentSameLine(next(curr))

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
  final def prevBeforeNonComment(curr: FormatToken): FormatToken =
    prev(prevNonComment(curr))

  @inline
  final def prevNonCommentSameLineBefore(curr: FormatToken): FormatToken =
    prevNonCommentSameLine(prev(curr))

  def prevNotTrailingComment(
      curr: FormatToken,
  ): Either[FormatToken, FormatToken] = curr.left match {
    case _: Token.Comment =>
      val prev = prevNonCommentSameLineBefore(curr)
      if (prev.hasBreak) Left(curr) else Right(prev)
    case _ => Right(curr)
  }

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

  @inline
  private def getHeadImpl(tokens: Tokens): FormatToken = after(tokens.head)
  def getHead(tokens: Tokens, tree: Tree): FormatToken =
    getOnOrBeforeOwned(getHeadImpl(tokens), tree)
  @inline
  def getHead(tree: Tree): FormatToken = getHead(tree.tokens, tree)

  def getHeadOpt(tokens: Tokens, tree: Tree): Option[FormatToken] = tokens
    .headOption.map(x => getOnOrBeforeOwned(after(x), tree))
  @inline
  def getHeadOpt(tree: Tree): Option[FormatToken] = getHeadOpt(tree.tokens, tree)

  @inline
  private def getLastImpl(tokens: Tokens): FormatToken =
    apply(findLastVisibleToken(tokens))
  def getLast(tokens: Tokens, tree: Tree): FormatToken =
    getOnOrAfterOwned(getLastImpl(tokens), tree)
  @inline
  def getLast(tree: Tree): FormatToken = getLast(tree.tokens, tree)

  def getOnOrAfterLast(tokens: Tokens, tree: Tree): FormatToken = {
    val last = tokens.last
    val beforeLast = before(last)
    val res = getOnOrAfterOwned(beforeLast, tree)
    val ok = (res eq beforeLast) && res.right.is[Token.Comment] &&
      (res.noBreak || res.right.pos.startLine == last.pos.startLine)
    if (ok) next(res) else res
  }

  @inline
  def getOnOrAfterLast(tree: Tree): FormatToken =
    getOnOrAfterLast(tree.tokens, tree)

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

  def getLastNotTrailingComment(tree: Tree): Either[FormatToken, FormatToken] =
    prevNotTrailingComment(getLast(tree))
  def getLastNotTrailingCommentOpt(
      tree: Tree,
  ): Option[Either[FormatToken, FormatToken]] = getLastOpt(tree)
    .map(prevNotTrailingComment)

  /* the following methods return the first format token such that
   * its `right` is after the parameter and is not a comment */
  @inline
  def tokenAfter(ft: FormatToken): FormatToken = nextNonComment(ft)
  @inline
  def tokenAfter(token: Token): FormatToken = tokenAfter(before(token))
  @inline
  def tokenAfter(tree: Tree): FormatToken = tokenAfter(getLast(tree))
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
  def tokenJustBefore(ft: FormatToken): FormatToken = prev(ft)
  @inline
  def tokenJustBefore(tree: Tree): FormatToken = prev(getHead(tree))

  def tokenJustBeforeOpt(tree: Tree): Option[FormatToken] = getHeadOpt(tree)
    .map(prev)
  def tokenJustBeforeOpt(trees: Seq[Tree]): Option[FormatToken] = trees
    .headOption.flatMap(tokenJustBeforeOpt)

  def isTokenHeadOf(tok: => Token, tree: Tree): Boolean =
    getHeadOpt(tree) match {
      case None => false
      case Some(x) => x.left eq tok
    }

  def isJustBeforeTree(ft: FormatToken)(tree: Tree): Boolean =
    isTokenHeadOf(ft.right, tree)

  /* the following methods return the last format token such that
   * its `left` is before the parameter and is not a comment */
  @inline
  def tokenOnOrBefore(token: Token): FormatToken = prevNonComment(before(token))
  @inline
  def tokenBefore(ft: FormatToken): FormatToken = prevNonCommentBefore(ft)
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
  def hasBreakAfterRightBeforeNonComment(ft: FormatToken): Boolean =
    nextNonCommentSameLineAfter(ft).hasBreakOrEOF

  @inline
  def hasBreakBeforeNonComment(ft: FormatToken): Boolean = ft.hasBreak ||
    hasBreakAfterRightBeforeNonComment(ft)

  @inline
  def isRightCommentThenBreak(ft: FormatToken): Boolean = ft.right
    .is[Token.Comment] && hasBreakAfterRightBeforeNonComment(ft)

  @inline
  def isRightCommentWithBreak(ft: FormatToken): Boolean = ft.right
    .is[Token.Comment] && hasBreakBeforeNonComment(ft)

  @inline
  def isAttachedCommentThenBreak(ft: FormatToken): Boolean = ft.noBreak &&
    isRightCommentThenBreak(ft)

  // Maps token to number of non-whitespace bytes before the token's position.
  private final lazy val nonWhitespaceOffset: Array[Int] = {
    val result = new Array[Int](arr.length)
    var curr = 0
    arr.foreach { t =>
      result(t.idx) = curr
      curr += t.left.len
    }
    result
  }

  def distance(left: FormatToken, right: FormatToken): Int =
    nonWhitespaceOffset(right.idx) - nonWhitespaceOffset(left.idx)
  def distance(tokens: Tokens): Int =
    if (tokens.isEmpty) 0
    else distance(getHeadImpl(tokens), getLastImpl(tokens))
  @inline
  def distance(tree: Tree): Int = distance(tree.tokens)

}

object FormatTokens {

  /** Convert scala.meta Tokens to FormatTokens.
    *
    * Since tokens might be very large, we try to allocate as little memory as
    * possible.
    */
  def apply(tokens: Tokens, owner: Token => Tree)(implicit
      style: ScalafmtConfig,
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
      if (left eq null) fmtWasOff = isFormatOff(right)
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

  private def throwNoToken(t: Token, msg: String): Nothing =
    throw new NoSuchElementException(
      s"$msg ${t.structure} @${t.pos.startLine}:${t.pos.startColumn}: `$t`",
    )

  @inline
  def thash(token: Token): TokenHash = hash(token)

  class TokenToIndexMapBuilder {
    private val builder = Map.newBuilder[TokenHash, Int]
    def sizeHint(size: Int): Unit = builder.sizeHint(size)
    def add(idx: Int)(token: Token): Unit = builder += thash(token) -> idx
    def result(): Map[TokenHash, Int] = builder.result()
  }

}
