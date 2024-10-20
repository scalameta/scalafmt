package org.scalafmt.internal

import scala.meta.tokens.Token

class TokenRange private (val lt: FormatToken, val rt: FormatToken) {

  def validateAfter(other: TokenRange): Unit = require(lt.idx > other.rt.idx)

}

class TokenRanges private (val ranges: Seq[TokenRange]) extends AnyVal {

  @inline
  def isEmpty: Boolean = ranges.isEmpty
  @inline
  def getIf(flag: Boolean): TokenRanges = if (flag) this else TokenRanges.empty

  def append(range: TokenRange): TokenRanges = {
    ranges.headOption.foreach(range.validateAfter)
    new TokenRanges(range +: ranges)
  }

  def startOfFirstRange(): Option[Token] = ranges.lastOption.map(_.lt.left)
}

object TokenRange {

  def apply(lt: FormatToken, rt: FormatToken): TokenRange =
    if (lt.idx < rt.idx) new TokenRange(lt, rt) else new TokenRange(rt, lt)

}

object TokenRanges {

  val empty = new TokenRanges(Seq.empty)

  def apply(range: TokenRange): TokenRanges = new TokenRanges(Seq(range))

}
