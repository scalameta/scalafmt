package org.scalafmt.internal

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.rewrite.FormatTokensRewrite
import org.scalafmt.util._

import scala.meta._
import scala.meta.tokens.{Token => T, Tokens}

import scala.annotation.tailrec

import TokenOps._

class FormatTokens(leftTok2tok: Map[TokenHash, Int])(val arr: Array[FT])
    extends IndexedSeq[FT] {

  private def this(arr: Array[FT]) = this {
    val result = new FormatTokens.TokenToIndexMapBuilder
    result.sizeHint(arr.length)
    arr.foreach(t => result.add(t.meta.idx)(t.left))
    result.add(arr.last.meta.idx)(arr.last.right)
    result.result()
  }(arr)

  private lazy val matchingDelims: Map[Int, FT] = TreeOps
    .getMatchingDelims(arr)(_.idx)(_.left)

  override def length: Int = arr.length
  override def apply(idx: Int): FT = arr(idx)

  private def getAt(tok: T, isBefore: Boolean)(idx: Int): FT =
    if (idx >= arr.length) arr.last
    else {
      val ft = arr(idx)
      if (ft.left eq tok) ft
      else if (isBefore) if (ft.left.start <= tok.start) ft else at(idx - 1)
      else if (ft.left.start >= tok.start) ft
      else at(idx + 1)
    }

  private def get(tok: T, isBefore: Boolean): FT = getAt(tok, isBefore)(
    leftTok2tok
      .getOrElse(hash(tok), FormatTokens.throwNoToken(tok, "Missing token index")),
  )

  def at(off: Int): FT =
    if (off < 0) arr.head else if (off < arr.length) arr(off) else arr.last

  // get token; if rewritten, the one before
  @inline
  def before(tok: T): FT = get(tok, true)
  // get token; if rewritten, the one after
  @inline
  def after(tok: T): FT = get(tok, false)
  @inline
  def apply(tok: T): FT = before(tok)

  /** If the token is missing:
    *   - to go backward, start from the next token
    *   - to go forward, start from the previous token This ensures that the
    *     next token in the direction of search is counted.
    */
  def apply(tok: T, off: Int): FT =
    apply(if (off < 0) after(tok) else before(tok), off)
  def apply(ft: FT, off: Int): FT = at(ft.meta.idx + off)

  @inline
  def hasNext(ft: FT): Boolean = ft.meta.idx < arr.length - 1
  @inline
  def hasPrev(ft: FT): Boolean = ft.meta.idx > 0

  @inline
  def prev(ft: FT): FT = apply(ft, -1)
  @inline
  def next(ft: FT): FT = apply(ft, 1)

  @inline
  private def matching(idx: Int, tok: => T): FT = matchingDelims.getOrElse(
    idx,
    FormatTokens.throwNoToken(tok, "Missing matching token index"),
  )
  @inline
  def matchingLeft(ft: FT): FT = matching(ft.idx, ft.left)
  @inline
  def matchingRight(ft: FT): FT = matching(ft.idx + 1, ft.right)
  @inline
  def matchingOpt(idx: Int): Option[FT] = matchingDelims.get(idx)
  @inline
  def matchingOptLeft(ft: FT): Option[FT] = matchingOpt(ft.idx)
  @inline
  def matchingOptRight(ft: FT): Option[FT] = matchingOpt(ft.idx + 1)

  def getHeadAndLastIfEnclosed(
      tokens: Tokens,
      tree: Tree,
  ): Option[(FT, Option[FT])] = getHeadOpt(tokens, tree).map(head =>
    head -> matchingOptLeft(head).flatMap { other =>
      val last = getLastNonTrivial(tokens, tree)
      if (last eq other) Some(last) else None
    },
  )
  def getHeadAndLastIfEnclosed(tree: Tree): Option[(FT, Option[FT])] =
    getHeadAndLastIfEnclosed(tree.tokens, tree)

  def getDelimsIfEnclosed(tokens: Tokens, tree: Tree): Option[(FT, FT)] =
    getHeadAndLastIfEnclosed(tokens, tree).flatMap { case (head, lastOpt) =>
      lastOpt.map(last => (head, last))
    }
  def getDelimsIfEnclosed(tree: Tree): Option[(FT, FT)] =
    getDelimsIfEnclosed(tree.tokens, tree)

  def getHeadIfEnclosed(tokens: Tokens, tree: Tree): Option[FT] =
    getDelimsIfEnclosed(tokens, tree).map(_._1)
  def getHeadIfEnclosed(tree: Tree): Option[FT] = getDelimsIfEnclosed(tree)
    .map(_._1)

  def getLastIfEnclosed(tokens: Tokens, tree: Tree): Option[FT] =
    getDelimsIfEnclosed(tokens, tree).map(_._2)
  def getLastIfEnclosed(tree: Tree): Option[FT] = getDelimsIfEnclosed(tree)
    .map(_._2)

  def isEnclosedInMatching(tokens: Tokens, tree: Tree): Boolean =
    getDelimsIfEnclosed(tokens, tree).isDefined
  def isEnclosedInMatching(tree: Tree): Boolean =
    isEnclosedInMatching(tree.tokens, tree)

  @inline
  def getBracesIfEnclosed(tree: Tree): Option[(FT, FT)] =
    getDelimsIfEnclosed(tree).filter(_._1.left.is[T.LeftBrace])

  @inline
  def isEnclosedInBraces(tree: Tree): Boolean = getDelimsIfEnclosed(tree)
    .exists(_._1.left.is[T.LeftBrace])

  def isEnclosedWithinParens(tree: Tree): Boolean =
    getClosingIfWithinParens(tree).isDefined

  def getClosingIfWithinParens(tree: Tree): Option[FT] =
    getClosingIfWithinParensOrBraces(tree).flatMap(_.toOption)

  def getClosingIfWithinParens(last: FT)(head: FT): Option[FT] =
    getClosingIfWithinParensOrBraces(last)(head).flatMap(_.toOption)

  def isEnclosedWithinParensOrBraces(tree: Tree): Boolean =
    getClosingIfWithinParensOrBraces(tree).isDefined

  def getClosingIfWithinParensOrBraces(
      last: FT,
  )(head: FT): Option[Either[FT, FT]] = {
    val innerMatched = matchingOptLeft(last).contains(head)
    if (innerMatched && last.left.is[T.RightParen]) Some(Right(prev(last)))
    else {
      val afterLast = nextNonComment(last)
      if (!matchingOptRight(afterLast).exists(_ eq prevNonCommentBefore(head)))
        if (innerMatched) Some(Left(prev(last))) else None
      else
        Some(Either.cond(afterLast.right.is[T.RightParen], afterLast, afterLast))
    }
  }

  def getClosingIfWithinParensOrBraces(tree: Tree): Option[Either[FT, FT]] = {
    val tokens = tree.tokens
    getHeadOpt(tokens, tree) match {
      case Some(head) =>
        getClosingIfWithinParensOrBraces(getLastNonTrivial(tokens, tree))(head)
      case None => None
    }
  }

  def getLastExceptParen(tree: Tree): FT = {
    val tokens = tree.tokens
    val last = getLast(tokens, tree)
    getClosingIfWithinParens(prevNonComment(last))(getHead(tokens, tree))
      .getOrElse(last)
  }

  final def findTokenWith[A](ft: FT, iter: FT => FT)(
      f: FT => Option[A],
  ): Either[FT, A] = findTokenEx(ft)(xft => f(xft).toRight(iter(xft)))

  @tailrec
  final def findTokenEx[A](ft: FT)(f: FT => Either[FT, A]): Either[FT, A] =
    f(ft) match {
      case null => Left(ft)
      case Left(nextFt) if nextFt ne ft => findTokenEx(nextFt)(f)
      case x => x
    }

  final def findToken(ft: FT, iter: FT => FT)(f: FT => Boolean): FT =
    findTokenWith(ft, iter)(Some(_).filter(f)).merge

  final def nextNonCommentSameLine(curr: FT): FT =
    findToken(curr, next)(ft => ft.hasBreak || !ft.right.is[T.Comment])

  final def nextNonCommentSameLineAfter(curr: FT): FT =
    nextNonCommentSameLine(next(curr))

  final def nextAfterNonCommentSameLine(curr: FT): FT =
    next(nextNonCommentSameLine(curr))

  final def nextNonComment(curr: FT): FT =
    findToken(curr, next)(!_.right.is[T.Comment])

  final def nextNonComment(curr: FT.Meta): FT = nextNonComment(arr(curr.idx))

  @inline
  final def nextNonCommentAfter(curr: FT): FT = nextNonComment(next(curr))

  @inline
  final def nextAfterNonComment(curr: FT): FT = next(nextNonComment(curr))

  final def prevNonCommentSameLine(curr: FT): FT =
    findToken(curr, prev)(ft => ft.hasBreak || !ft.left.is[T.Comment])

  final def prevNonComment(curr: FT): FT =
    findToken(curr, prev)(!_.left.is[T.Comment])

  @inline
  final def prevNonCommentBefore(curr: FT): FT = prevNonComment(prev(curr))

  @inline
  final def prevBeforeNonComment(curr: FT): FT = prev(prevNonComment(curr))

  @inline
  final def prevNonCommentSameLineBefore(curr: FT): FT =
    prevNonCommentSameLine(prev(curr))

  def prevNotTrailingComment(curr: FT): Either[FT, FT] = curr.left match {
    case _: T.Comment =>
      val prev = prevNonCommentSameLineBefore(curr)
      if (prev.hasBreak) Left(curr) else Right(prev)
    case _ => Right(curr)
  }

  @tailrec
  final def getOnOrBeforeOwned(ft: FT, tree: Tree): FT = {
    val prevFt = prevNonCommentBefore(ft)
    if (prevFt == ft || prevFt.meta.leftOwner != tree) ft
    else getOnOrBeforeOwned(prevFt, tree)
  }

  @tailrec
  final def getOnOrAfterOwned(ft: FT, tree: Tree): FT = {
    val nextFt = nextAfterNonComment(ft)
    if (nextFt == ft || nextFt.meta.leftOwner != tree) ft
    else getOnOrAfterOwned(nextFt, tree)
  }

  @inline
  private def getHeadImpl(tokens: Tokens): FT = after(tokens.head)
  def getHead(tokens: Tokens, tree: Tree): FT =
    getOnOrBeforeOwned(getHeadImpl(tokens), tree)
  @inline
  def getHead(tree: Tree): FT = getHead(tree.tokens, tree)

  def getHeadOpt(tokens: Tokens, tree: Tree): Option[FT] = tokens.headOption
    .map(x => getOnOrBeforeOwned(after(x), tree))
  @inline
  def getHeadOpt(tree: Tree): Option[FT] = getHeadOpt(tree.tokens, tree)

  @inline
  private def getLastImpl(tokens: Tokens): FT =
    apply(findLastVisibleToken(tokens))
  def getLast(tokens: Tokens, tree: Tree): FT =
    getOnOrAfterOwned(getLastImpl(tokens), tree)
  @inline
  def getLast(tree: Tree): FT = getLast(tree.tokens, tree)

  def getOnOrAfterLast(tokens: Tokens, tree: Tree): FT = {
    val last = tokens.last
    val beforeLast = before(last)
    val res = getOnOrAfterOwned(beforeLast, tree)
    val ok = (res eq beforeLast) && res.right.is[T.Comment] &&
      (res.noBreak || res.right.pos.startLine == last.pos.startLine)
    if (ok) next(res) else res
  }

  @inline
  def getOnOrAfterLast(tree: Tree): FT = getOnOrAfterLast(tree.tokens, tree)

  def getLastOpt(tokens: Tokens, tree: Tree): Option[FT] =
    findLastVisibleTokenOpt(tokens).map(x => getOnOrAfterOwned(apply(x), tree))
  @inline
  def getLastOpt(tree: Tree): Option[FT] = getLastOpt(tree.tokens, tree)

  def getLastNonTrivial(tokens: Tokens, tree: Tree): FT =
    prevNonComment(getLast(tokens, tree))
  def getLastNonTrivial(tree: Tree): FT = getLastNonTrivial(tree.tokens, tree)

  def getLastNonTrivialOpt(tokens: Tokens, tree: Tree): Option[FT] =
    getLastOpt(tokens, tree).map(prevNonComment)
  def getLastNonTrivialOpt(tree: Tree): Option[FT] =
    getLastNonTrivialOpt(tree.tokens, tree)

  def getLastNotTrailingComment(tree: Tree): Either[FT, FT] =
    prevNotTrailingComment(getLast(tree))
  def getLastNotTrailingCommentOpt(tree: Tree): Option[Either[FT, FT]] =
    getLastOpt(tree).map(prevNotTrailingComment)

  /* the following methods return the first format token such that
   * its `right` is after the parameter and is not a comment */
  @inline
  def tokenAfter(ft: FT): FT = nextNonComment(ft)
  @inline
  def tokenAfter(token: T): FT = tokenAfter(before(token))
  @inline
  def tokenAfter(tree: Tree): FT = tokenAfter(getLast(tree))
  @inline
  def tokenAfter(trees: Seq[Tree]): FT = tokenAfter(trees.last)

  def tokenAfterOpt(tree: Tree): Option[FT] = getLastOpt(tree)
    .map(nextNonComment)
  def tokenAfterOpt(trees: Seq[Tree]): Option[FT] = trees.lastOption
    .flatMap(tokenAfterOpt)

  /* the following methods return the last format token such that
   * its `left` is before the parameter */
  @inline
  def justBefore(token: T): FT = apply(token, -1)
  @inline
  def tokenJustBefore(ft: FT): FT = prev(ft)
  @inline
  def tokenJustBefore(tree: Tree): FT = prev(getHead(tree))

  def tokenJustBeforeOpt(tree: Tree): Option[FT] = getHeadOpt(tree).map(prev)
  def tokenJustBeforeOpt(trees: Seq[Tree]): Option[FT] = trees.headOption
    .flatMap(tokenJustBeforeOpt)

  def isTokenHeadOf(tok: => T, tree: Tree): Boolean = getHeadOpt(tree) match {
    case None => false
    case Some(x) => x.left eq tok
  }

  def isJustBeforeTree(ft: FT)(tree: Tree): Boolean =
    isTokenHeadOf(ft.right, tree)

  /* the following methods return the last format token such that
   * its `left` is before the parameter and is not a comment */
  @inline
  def tokenOnOrBefore(token: T): FT = prevNonComment(before(token))
  @inline
  def tokenBefore(ft: FT): FT = prevNonCommentBefore(ft)
  @inline
  def tokenBefore(token: T): FT = prevNonComment(justBefore(token))
  @inline
  def tokenBefore(tree: Tree): FT = prevNonComment(tokenJustBefore(tree))
  @inline
  def tokenBefore(trees: Seq[Tree]): FT = tokenBefore(trees.head)

  def tokenBeforeOpt(tree: Tree): Option[FT] = tokenJustBeforeOpt(tree)
    .map(prevNonComment)
  def tokenBeforeOpt(trees: Seq[Tree]): Option[FT] = trees.headOption
    .flatMap(tokenBeforeOpt)

  @inline
  def isBreakAfterRight(ft: FT): Boolean = next(ft).hasBreakOrEOF

  @inline
  def hasBreakAfterRightBeforeNonComment(ft: FT): Boolean =
    nextNonCommentSameLineAfter(ft).hasBreakOrEOF

  @inline
  def hasBreakBeforeNonComment(ft: FT): Boolean = ft.hasBreak ||
    hasBreakAfterRightBeforeNonComment(ft)

  @inline
  def isRightCommentThenBreak(ft: FT): Boolean = ft.right.is[T.Comment] &&
    hasBreakAfterRightBeforeNonComment(ft)

  @inline
  def isRightCommentWithBreak(ft: FT): Boolean = ft.right.is[T.Comment] &&
    hasBreakBeforeNonComment(ft)

  @inline
  def isAttachedCommentThenBreak(ft: FT): Boolean = ft.noBreak &&
    isRightCommentThenBreak(ft)

  def isEmpty(tree: Tree): Boolean = {
    val toks = tree.tokens
    toks.headOption.forall(after(_).left.end > toks.last.end)
  }

  // Maps token to number of non-whitespace bytes before the token's position.
  private final lazy val offsets: Array[FormatTokens.Offsets] = {
    val result = new Array[FormatTokens.Offsets](arr.length)
    var offsets = new FormatTokens.Offsets(0, 0, 0)
    arr.foreach { t =>
      result(t.idx) = offsets
      offsets += t
    }
    result
  }

  private def offsetDiff(left: Int, right: Int)(
      f: FormatTokens.Offsets => Int,
  ): Int = {
    val lastIdx = offsets.length - 1
    val rightIdx = if (right >= lastIdx) lastIdx else right + 1
    f(offsets(rightIdx)) - f(offsets(left))
  }
  def offsetDiff(left: FT, right: FT)(f: FormatTokens.Offsets => Int): Int =
    offsetDiff(left.idx, right.idx)(f)
  def offsetDiff(tree: Tree)(f: FormatTokens.Offsets => Int): Int = {
    val tokens = tree.tokens
    offsetDiff(getHead(tokens, tree), getLast(tokens, tree))(f)
  }
  def width(left: Int, right: Int): Int = offsetDiff(left, right)(_.width)
  def width(left: FT, right: FT): Int = width(left.idx, right.idx)
  def span(left: Int, right: Int): Int = offsetDiff(left, right)(_.nonWs)
  def span(left: FT, right: FT): Int = span(left.idx, right.idx)
  def span(tokens: Tokens): Int =
    if (tokens.isEmpty) 0 else span(getHeadImpl(tokens), getLastImpl(tokens))
  @inline
  def span(tree: Tree): Int = span(tree.tokens)

  @tailrec
  final def getNonMultilineEnd(ft: FT, inInterp: Int = 0): Option[FT] = {
    def nft = next(ft)
    ft.right match {
      case _: T.Comment =>
        if (ft.hasBreak) Some(ft)
        else if (ft.rightHasNewline) None
        else {
          val xft = nft
          if (xft.hasBreak) Some(xft)
          else getNonMultilineEnd(xft, inInterp = inInterp)
        }
      case _: T.Interpolation.Id =>
        getNonMultilineEnd(nft, inInterp = inInterp + 1)
      case _: T.Constant.String if ft.rightHasNewline => None
      case _ if inInterp == 0 => Some(nft)
      case _: T.Interpolation.End =>
        if (inInterp == 1) Some(nft)
        else getNonMultilineEnd(nft, inInterp = inInterp - 1)
      case _: T.Interpolation.Part if ft.rightHasNewline => None
      case _ => getNonMultilineEnd(nft, inInterp = inInterp)
    }
  }

}

object FormatTokens {

  /** Convert scala.meta Tokens to FormatTokens.
    *
    * Since tokens might be very large, we try to allocate as little memory as
    * possible.
    */
  def apply(tokens: Tokens, owners: collection.Map[TokenHash, Tree])(implicit
      style: ScalafmtConfig,
  ): (FormatTokens, StyleMap) = {
    var left: T = null
    var lmeta: FT.TokenMeta = null
    val result = Array.newBuilder[FT]
    var ftIdx = 0
    var prevNonWsIdx = -1
    var fmtWasOff = false
    def process(right: T, tokIdx: Int): Unit = if (!right.is[T.Whitespace]) {
      val rmeta = FT.TokenMeta(owners(hash(right)), right.text)
      if (left eq null) fmtWasOff = style.isFormatOff(right)
      else {
        val between = tokens.arraySlice(prevNonWsIdx + 1, tokIdx)
        val fmtIsOff = fmtWasOff || style.isFormatOff(right)
        fmtWasOff = if (fmtWasOff) !style.isFormatOn(right) else fmtIsOff
        val meta = FT.Meta(between, ftIdx, fmtIsOff, lmeta, rmeta)
        result += FT(left, right, meta)
        ftIdx += 1
      }
      left = right
      lmeta = rmeta
      prevNonWsIdx = tokIdx
    }
    tokens.foreachWithIndex(process)

    val ftoks = new FormatTokens(result.result)
    val styleMap = new StyleMap(ftoks, style)

    FormatTokensRewrite(ftoks, styleMap) -> styleMap
  }

  private def throwNoToken(t: T, msg: String): Nothing =
    throw new NoSuchElementException(
      s"$msg ${t.structure} @${t.pos.startLine}:${t.pos.startColumn}: `$t`",
    )

  class TokenToIndexMapBuilder {
    private val builder = Map.newBuilder[TokenHash, Int]
    def sizeHint(size: Int): Unit = builder.sizeHint(size)
    def add(idx: Int)(token: T): Unit = builder += hash(token) -> idx
    def result(): Map[TokenHash, Int] = builder.result()
  }

  class Offsets(val nonWs: Int, val width: Int, val nonWsNonPunct: Int) {
    def +(ft: FT): Offsets = {
      val tok = ft.left
      val (head, last) = State
        .getColumns(tok, ft.meta.left, 0)(identity)(identity)
      def tokLenUnless(flag: Boolean) = if (flag) 0 else tok.len
      val owner = ft.leftOwner
      val nonPunct = tok match {
        case _: T.Punct | _: T.KwThen | _: T.Comment => 0
        case _: T.KwDo => tokLenUnless(owner.is[Term.While])
        case _: T.At => tokLenUnless(owner match {
            case t: Pat.Bind => t.rhs.is[Pat.SeqWildcard]
            case _ => false
          })
        case _: T.Colon => tokLenUnless(owner match {
            case _: Template | _: Term.Apply | _: Term.Repeated => true
            case t: Pat.Bind => t.rhs.is[Pat.SeqWildcard]
            case _ => false
          })
        case _: T.Underscore =>
          tokLenUnless(owner.isAny[Term.Repeated, Pat.SeqWildcard])
        case _: T.Ident => tokLenUnless(
            owner.is[Term.EndMarker] || owner.parent.is[Term.EndMarker],
          )
        case _: T.Keyword => tokLenUnless(owner.parent.is[Term.EndMarker])
        case _ => tok.len
      }
      new Offsets(
        this.nonWs + tok.len,
        this.width + head.max(last),
        this.nonWsNonPunct + nonPunct,
      )
    }
  }

  def apply(
      topSourceTree: Tree,
  )(baseStyle: ScalafmtConfig): (FormatTokens, StyleMap) = {
    val (initStyle, owners) = TreeOps.getStyleAndOwners(topSourceTree, baseStyle)
    apply(topSourceTree.tokens, owners)(initStyle)
  }

}
