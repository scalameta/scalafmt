package org.scalafmt.internal

import scala.annotation.tailrec
import scala.meta.Tree
import scala.meta.tokens.Token

import org.scalafmt.util.TokenOps._

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
case class FormatToken(left: Token, right: Token, meta: FormatToken.Meta) {

  override def toString =
    s"${meta.left.text}∙${meta.right.text}[${left.end}:${right.end}]"

  def inside(range: Set[Range]): Boolean = {
    if (range.isEmpty) true
    else range.exists(_.contains(right.pos.endLine))
  }

  def between = meta.between
  lazy val newlinesBetween: Int = {
    val nl = meta.newlinesBetween
    // make sure to break before/after docstring
    if (nl != 0) nl
    else if (left.is[Token.Comment] && isDocstring(meta.left.text)) 1
    else if (right.is[Token.Comment] && isDocstring(meta.right.text)) 1
    else 0
  }
  @inline def noBreak: Boolean = FormatToken.noBreak(newlinesBetween)
  @inline def hasBreak: Boolean = !noBreak
  @inline def hasBlankLine: Boolean = FormatToken.hasBlankLine(newlinesBetween)

  @inline def leftHasNewline = meta.left.hasNL
  @inline def rightHasNewline = meta.right.hasNL
  @inline def hasBreakOrEOF: Boolean = hasBreak || right.is[Token.EOF]

  /** A format token is uniquely identified by its left token.
    */
  override def hashCode(): Int = hash(left).##
}

object FormatToken {

  @inline def noBreak(newlines: Int): Boolean = newlines == 0
  @inline def hasBlankLine(newlines: Int): Boolean = newlines > 1

  @inline def isNL(token: Token): Boolean = token.is[Token.LF]

  /** @param between
    *   The whitespace tokens between left and right.
    * @param idx
    *   The token's index in the FormatTokens array
    * @param formatOff
    *   if true, between and right should not be formatted
    */
  case class Meta(
      between: Array[Token],
      idx: Int,
      formatOff: Boolean,
      left: TokenMeta,
      right: TokenMeta
  ) {
    @inline def leftOwner: Tree = left.owner
    @inline def rightOwner: Tree = right.owner

    /** returns a value between 0 and 2 (2 for a blank line) */
    lazy val newlinesBetween: Int = {
      @tailrec
      def count(idx: Int, maxCount: Int): Int = {
        if (idx == between.length) maxCount
        else {
          val token = between(idx)
          if (isNL(token))
            if (maxCount == 0) count(idx + 1, 1) else 2
          else
            count(idx + 1, maxCount)
        }
      }
      count(0, 0)
    }
  }

  case class TokenMeta(
      owner: Tree,
      text: String
  ) {
    lazy val firstNL = text.indexOf('\n')
    @inline def hasNL: Boolean = firstNL >= 0
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

  class ExtractFromMeta[A](f: FormatToken.Meta => Option[A]) {
    def unapply(meta: FormatToken.Meta): Option[A] = f(meta)
  }

  val LeftOwner = new ExtractFromMeta(x => Some(x.leftOwner))
  val RightOwner = new ExtractFromMeta(x => Some(x.rightOwner))

}
