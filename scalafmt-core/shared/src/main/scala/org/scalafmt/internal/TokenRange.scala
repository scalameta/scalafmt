package org.scalafmt.internal

import scala.meta.tokens.{Token => T}

class TokenRange private (val lt: FT, val rt: FT) {

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

  def filter(f: TokenRange => Boolean): TokenRanges =
    new TokenRanges(ranges.filter(f))

  def map(f: TokenRange => TokenRange): TokenRanges =
    new TokenRanges(ranges.map(f))

  def excludeCloseDelim(implicit ftoks: FormatTokens): TokenRanges =
    new TokenRanges(ranges.flatMap(x =>
      if (!x.lt.left.is[T.OpenDelim]) Some(x)
      else TokenRange.opt(x.lt, ftoks.prev(x.rt)),
    ))

  def startOfFirstRange(): Option[T] = ranges.lastOption.map(_.lt.left)
}

object TokenRange {

  def apply(lt: FT, rt: FT): TokenRange =
    if (lt.idx < rt.idx) new TokenRange(lt, rt) else new TokenRange(rt, lt)

  def opt(lt: FT, rt: FT): Option[TokenRange] =
    if (lt.idx < rt.idx) Some(new TokenRange(lt, rt))
    else if (lt.idx > rt.idx) Some(new TokenRange(rt, lt))
    else None

}

object TokenRanges {

  val empty = new TokenRanges(Seq.empty)

  def apply(range: TokenRange): TokenRanges = new TokenRanges(Seq(range))

}
