package org.scalafmt.internal

import scala.meta.Tree
import scala.meta.tokens.Token

import org.scalafmt.util.TokenOps._

/**
  * Two adjacent non-whitespace tokens.
  *
  * Consider a FormatToken as a node in a search graph and [[Split]]
  * are the edges. The format tokens remain unchanged after formatting,
  * while the splits are changed.
  *
  * @param left The left non-whitespace token.
  * @param right The right non-whitespace token.
  * @param meta Extra information about the token
  */
case class FormatToken(left: Token, right: Token, meta: FormatToken.Meta) {

  override def toString =
    s"${meta.left.text}∙${meta.right.text}[${left.end}:${right.end}]"

  def inside(range: Set[Range]): Boolean = {
    if (range.isEmpty) true
    else range.exists(_.contains(right.pos.endLine))
  }

  def between = meta.between
  lazy val betweenText: String = between.map(_.syntax).mkString
  lazy val newlinesBetween: Int = {
    val nl = meta.between.count(_.is[Token.LF])
    // make sure to break before/after docstring
    if (nl != 0) nl
    else if (left.is[Token.Comment] && isDocstring(meta.left.text)) 1
    else if (right.is[Token.Comment] && isDocstring(meta.right.text)) 1
    else 0
  }
  @inline def noBreak: Boolean = FormatToken.noBreak(newlinesBetween)
  @inline def hasBreak: Boolean = !noBreak
  @inline def hasBlankLine: Boolean = FormatToken.hasBlankLine(newlinesBetween)

  @inline def leftHasNewline = meta.left.firstNL >= 0

  /**
    * A format token is uniquely identified by its left token.
    */
  override def hashCode(): Int = hash(left).##
}

object FormatToken {

  @inline def noBreak(newlines: Int): Boolean = newlines == 0
  @inline def hasBlankLine(newlines: Int): Boolean = newlines > 1

  /**
    * @param between The whitespace tokens between left and right.
    * @param idx The token's index in the FormatTokens array
    */
  case class Meta(
      between: Array[Token],
      idx: Int,
      left: TokenMeta,
      right: TokenMeta
  ) {
    @inline def leftOwner: Tree = left.owner
    @inline def rightOwner: Tree = right.owner
  }

  case class TokenMeta(
      owner: Tree,
      text: String
  ) {
    lazy val firstNL = text.indexOf('\n')
  }

}
