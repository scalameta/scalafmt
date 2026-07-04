package org.scalafmt
package util

import org.scalafmt.config.ScalafmtConfig

import scala.meta._
import scala.meta.tokens.{Token => T}

import scala.annotation.tailrec

class TokenTraverser(tokens: Tokens, input: Input)(implicit
    style: ScalafmtConfig,
) {
  private[this] val (tok2idx, excludedTokens) = {
    val map = Map.newBuilder[T, Int]
    val excluded = Set.newBuilder[TokenOps.TokenHash]
    var formatOff = false
    var i = 0
    tokens.foreach { tok =>
      if (!formatOff) { if (style.isFormatOff(tok)) formatOff = true }
      else if (style.isFormatOn(tok)) formatOff = false
      else excluded += TokenOps.hash(tok)
      map += tok -> i
      i += 1
    }
    if (input.isInstanceOf[Input.Ammonite]) {
      val realTokens = tokens.dropWhile(_.is[T.BOF])
      realTokens.headOption.foreach {
        // shebang in .sc files
        case t: T.Ident if t.value.startsWith("#!") =>
          realTokens.takeWhile(!_.is[T.AtEOL])
            .foreach(excluded += TokenOps.hash(_))
        case _ =>
      }
    }

    (map.result(), excluded.result())
  }

  final def isExcluded(token: T): Boolean =
    (token ne null) && excludedTokens.contains(TokenOps.hash(token))

  @inline
  def getIndex(token: T): Int = tok2idx(token)

  def nextToken(token: T): T = tok2idx.get(token) match {
    case Some(i) if i < tokens.length - 1 => tokens(i + 1)
    case _ => token
  }

  def prevToken(token: T): T = tok2idx.get(token) match {
    case Some(i) if i > 0 => tokens(i - 1)
    case _ => token
  }

  def nextNonTrivialToken(token: T): T =
    findAfter(token)(TokenTraverser.isTrivialPred)

  def prevNonTrivialToken(token: T): T =
    findBefore(token)(TokenTraverser.isTrivialPred)

  /** Find a token after the given one. The search stops when the predicate
    * returns Some value (or the end is reached).
    * @return
    *   Some(token) if the predicate returned Some(true), else None.
    */
  def findAfter(token: T)(predicate: T => MaybeBool): T =
    tok2idx.get(token) match {
      case Some(x) => findAtOrAfter(x + 1)(predicate)
      case None => null
    }

  /** Find a token before the given one. The search stops when the predicate
    * returns Some value (or the end is reached).
    * @return
    *   Some(token) if the predicate returned Some(true), else None.
    */
  def findBefore(token: T)(predicate: T => MaybeBool): T =
    tok2idx.get(token) match {
      case Some(x) => findAtOrBefore(x - 1)(predicate)
      case None => null
    }

  @tailrec
  final def findAtOrAfter(off: Int)(pred: T => MaybeBool): T =
    if (off >= tokens.length) null
    else {
      val token = tokens(off)
      pred(token) match {
        case MaybeBool.True => token
        case MaybeBool.False => null
        case _ => findAtOrAfter(off + 1)(pred)
      }
    }

  @tailrec
  final def findAtOrBefore(off: Int)(pred: T => MaybeBool): T =
    if (off < 0) null
    else {
      val token = tokens(off)
      pred(token) match {
        case MaybeBool.True => token
        case MaybeBool.False => null
        case _ => findAtOrBefore(off - 1)(pred)
      }
    }

  final def filter(start: T, end: T)(predicate: T => Boolean): Seq[T] =
    if (start == end || nextToken(start) == start) Nil
    else {
      val tail = filter(nextToken(start), end)(predicate)
      if (predicate(start)) start +: tail else tail
    }

}

object TokenTraverser {

  private def isTrivialPred(token: T): MaybeBool =
    if (token.is[T.Trivia]) MaybeBool.Maybe else MaybeBool.True

}
