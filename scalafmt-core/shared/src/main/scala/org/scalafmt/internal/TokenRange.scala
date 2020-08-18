package org.scalafmt.internal

import scala.meta.Token

class TokenRange private (val lt: Token, val rt: Token) {

  def validateAfter(other: TokenRange): Unit = require(lt.start >= other.rt.end)

}

class TokenRanges private (val ranges: Seq[TokenRange]) extends AnyVal {

  def append(range: TokenRange): TokenRanges = {
    ranges.lastOption.foreach(range.validateAfter)
    new TokenRanges(ranges :+ range)
  }

}

object TokenRange {

  def apply(lt: Token, rt: Token): TokenRange =
    if (lt.start < rt.start) new TokenRange(lt, rt) else new TokenRange(rt, lt)

}

object TokenRanges {

  val empty = new TokenRanges(Seq.empty)

  def apply(range: TokenRange): TokenRanges =
    new TokenRanges(Seq(range))

  def apply(ranges: Seq[TokenRange]): TokenRanges = {
    if (ranges.isEmpty) empty
    else {
      ranges.reduceLeft[TokenRange] { case (l, r) => r.validateAfter(l); r }
      new TokenRanges(ranges)
    }
  }

}
