package org.scalafmt.util

import org.scalafmt.config.ScalafmtConfig

import scala.meta.Input
import scala.meta.tokens.Tokens
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

  final def isExcluded(token: T): Boolean = excludedTokens
    .contains(TokenOps.hash(token))

  @inline
  def getIndex(token: T): Int = tok2idx(token)
  @inline
  def getIndexOpt(token: T): Option[Int] = tok2idx.get(token)

  def nextToken(token: T): T = tok2idx.get(token) match {
    case Some(i) if i < tokens.length - 1 => tokens(i + 1)
    case _ => token
  }

  def prevToken(token: T): T = tok2idx.get(token) match {
    case Some(i) if i > 0 => tokens(i - 1)
    case _ => token
  }

  def nextNonTrivialToken(token: T): Option[T] =
    findAfter(token)(TokenTraverser.isTrivialPred)

  def prevNonTrivialToken(token: T): Option[T] =
    findBefore(token)(TokenTraverser.isTrivialPred)

  /** Find a token after the given one. The search stops when the predicate
    * returns Some value (or the end is reached).
    * @return
    *   Some(token) if the predicate returned Some(true), else None.
    */
  def findAfter(token: T)(predicate: T => Option[Boolean]): Option[T] = tok2idx
    .get(token).flatMap(x => findAtOrAfter(x + 1)(predicate))

  /** Find a token before the given one. The search stops when the predicate
    * returns Some value (or the end is reached).
    * @return
    *   Some(token) if the predicate returned Some(true), else None.
    */
  def findBefore(token: T)(predicate: T => Option[Boolean]): Option[T] = tok2idx
    .get(token).flatMap(x => findAtOrBefore(x - 1)(predicate))

  @tailrec
  final def findAtOrAfter(off: Int)(pred: T => Option[Boolean]): Option[T] =
    if (off >= tokens.length) None
    else {
      val token = tokens(off)
      pred(token) match {
        case Some(true) => Some(token)
        case Some(false) => None
        case _ => findAtOrAfter(off + 1)(pred)
      }
    }

  @tailrec
  final def findAtOrBefore(off: Int)(pred: T => Option[Boolean]): Option[T] =
    if (off < 0) None
    else {
      val token = tokens(off)
      pred(token) match {
        case Some(true) => Some(token)
        case Some(false) => None
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

  private def isTrivialPred(token: T): Option[Boolean] =
    if (token.is[T.Trivia]) None else Some(true)

}
