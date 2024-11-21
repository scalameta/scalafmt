package org.scalafmt.internal

import org.scalafmt.util.LoggerOps
import org.scalafmt.util.TokenOps._

import scala.meta.Tree
import scala.meta.tokens.{Token => T}

import scala.annotation.tailrec

/** Two adjacent non-whitespace tokens.
  *
  * Consider a FormatToken as a node in a search graph and [[Split]] are the
  * edges. The format tokens remain unchanged after formatting, while the splits
  * are changed.
  *
  * @param left
  *   The left non-whitespace token.
  * @param right
  *   The right non-whitespace token.
  * @param meta
  *   Extra information about the token
  */
case class FormatToken(left: T, right: T, meta: FT.Meta) {

  override def toString = {
    val ws = newlinesBetween match {
      case 0 => between.mkString
      case 1 => "LF"
      case _ => "LFLF"
    }
    val lt = meta.left.text
    val rt = meta.right.text
    val ls = left.structure
    val rs = right.structure
    val lo = LoggerOps.treeInfo(leftOwner)
    val ro = LoggerOps.treeInfo(rightOwner)
    s"[$idx] $lt $rt >>> $ls | $rs >>> $lo | $ro <<<[$ws]>>>"
  }

  def inside(range: Set[Range]): Boolean =
    if (range.isEmpty) true else range.exists(_.contains(right.pos.endLine))

  def between = meta.between
  lazy val newlinesBetween: Int = {
    val nl = meta.newlinesBetween
    // make sure to break before/after docstring
    if (nl != 0) nl
    else if (left.is[T.Comment] && isDocstring(meta.left.text)) 1
    else if (right.is[T.Comment] && isDocstring(meta.right.text)) 1
    else 0
  }
  @inline
  def noBreak: Boolean = FT.noBreak(newlinesBetween)
  @inline
  def hasBreak: Boolean = !noBreak
  @inline
  def hasBlankLine: Boolean = FT.hasBlankLine(newlinesBetween)

  @inline
  def leftHasNewline = meta.left.hasNL
  @inline
  def rightHasNewline = meta.right.hasNL
  @inline
  def hasBreakOrEOF: Boolean = hasBreak || right.is[T.EOF]

  def hasCRLF: Boolean = between.exists {
    case _: T.CRLF => true
    case t: T.MultiNL => t.tokens.exists(_.is[T.CRLF])
    case _ => false
  }

  /** A format token is uniquely identified by its left token.
    */
  override def hashCode(): Int = hash(left).##

  private[scalafmt] def withIdx(idx: Int): FT = copy(meta = meta.copy(idx = idx))

  @inline
  def idx: Int = meta.idx
  @inline
  def leftOwner: Tree = meta.leftOwner
  @inline
  def rightOwner: Tree = meta.rightOwner
}

object FormatToken {

  @inline
  def noBreak(newlines: Int): Boolean = newlines == 0
  @inline
  def hasBlankLine(newlines: Int): Boolean = newlines > 1

  @inline
  def isNL(token: T): Boolean = token.is[T.AtEOL]
  @inline
  def newlines(token: T): Int = token match {
    case t: T.AtEOL => t.newlines
    case _ => 0
  }

  /** @param between
    *   The whitespace tokens between left and right.
    * @param idx
    *   The token's index in the FormatTokens array
    * @param formatOff
    *   if true, between and right should not be formatted
    */
  case class Meta(
      between: Array[T],
      idx: Int,
      formatOff: Boolean,
      left: TokenMeta,
      right: TokenMeta,
  ) {
    @inline
    def leftOwner: Tree = left.owner
    @inline
    def rightOwner: Tree = right.owner

    /** returns a value between 0 and 2 (2 for a blank line) */
    lazy val newlinesBetween: Int = {
      @tailrec
      def count(idx: Int, maxCount: Int): Int =
        if (idx == between.length) maxCount
        else {
          val newMaxCount = maxCount + newlines(between(idx))
          if (newMaxCount < 2) count(idx + 1, newMaxCount) else 2
        }
      count(0, 0)
    }
  }

  case class TokenMeta(owner: Tree, text: String) {
    lazy val firstNL = text.indexOf('\n')
    @inline
    def hasNL: Boolean = firstNL >= 0
    def countNL: Int = {
      var cnt = 0
      var idx = firstNL
      while (idx >= 0) {
        cnt += 1
        idx = text.indexOf('\n', idx + 1)
      }
      cnt
    }
  }

  class ExtractFromMeta[A](f: FT.Meta => Option[A]) {
    def unapply(meta: FT.Meta): Option[A] = f(meta)
  }

  val LeftOwner = new ExtractFromMeta(x => Some(x.leftOwner))
  val RightOwner = new ExtractFromMeta(x => Some(x.rightOwner))

  val LeftOwnerParent =
    new ExtractFromMeta(x => Some((x.leftOwner, x.leftOwner.parent)))
  val RightOwnerParent =
    new ExtractFromMeta(x => Some((x.rightOwner, x.rightOwner.parent)))

}
