package org.scalafmt.util

import scala.annotation.tailrec
import scala.meta.Input
import scala.meta.tokens.Token
import scala.meta.tokens.Tokens

class TokenTraverser(tokens: Tokens, input: Input) {
  private[this] val (tok2idx, excludedTokens) = {
    val map = Map.newBuilder[Token, Int]
    val excluded = Set.newBuilder[TokenOps.TokenHash]
    var formatOff = false
    var i = 0
    tokens.foreach { tok =>
      if (!formatOff) {
        if (TokenOps.isFormatOff(tok)) formatOff = true
      } else {
        if (TokenOps.isFormatOn(tok)) formatOff = false
        else excluded += TokenOps.hash(tok)
      }
      map += (tok -> i)
      i += 1
    }
    if (input.isInstanceOf[Input.Ammonite]) {
      val realTokens = tokens.dropWhile(_.is[Token.BOF])
      realTokens.headOption.foreach {
        // shebang in .sc files
        case t: Token.Ident if t.value.startsWith("#!") =>
          realTokens.takeWhile(!_.is[Token.LF]).foreach {
            excluded += TokenOps.hash(_)
          }
        case _ =>
      }
    }

    (map.result(), excluded.result())
  }

  final def isExcluded(token: Token): Boolean =
    excludedTokens.contains(TokenOps.hash(token))

  @inline def getIndex(token: Token): Int = tok2idx(token)
  @inline def getIndexOpt(token: Token): Option[Int] = tok2idx.get(token)

  def nextToken(token: Token): Token = {
    tok2idx.get(token) match {
      case Some(i) if i < tokens.length - 1 => tokens(i + 1)
      case _ => token
    }
  }

  def prevToken(token: Token): Token = {
    tok2idx.get(token) match {
      case Some(i) if i > 0 => tokens(i - 1)
      case _ => token
    }
  }

  def nextNonTrivialToken(token: Token): Option[Token] =
    findAfter(token)(TokenTraverser.isTrivialPred)

  def prevNonTrivialToken(token: Token): Option[Token] =
    findBefore(token)(TokenTraverser.isTrivialPred)

  /** Find a token after the given one. The search stops when the predicate
    * returns Some value (or the end is reached).
    * @return
    *   Some(token) if the predicate returned Some(true), else None.
    */
  def findAfter(
      token: Token
  )(predicate: Token => Option[Boolean]): Option[Token] =
    tok2idx.get(token).flatMap(x => findAtOrAfter(x + 1)(predicate))

  /** Find a token before the given one. The search stops when the predicate
    * returns Some value (or the end is reached).
    * @return
    *   Some(token) if the predicate returned Some(true), else None.
    */
  def findBefore(
      token: Token
  )(predicate: Token => Option[Boolean]): Option[Token] =
    tok2idx.get(token).flatMap(x => findAtOrBefore(x - 1)(predicate))

  @tailrec
  final def findAtOrAfter(off: Int)(
      pred: Token => Option[Boolean]
  ): Option[Token] =
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
  final def findAtOrBefore(off: Int)(
      pred: Token => Option[Boolean]
  ): Option[Token] =
    if (off < 0) None
    else {
      val token = tokens(off)
      pred(token) match {
        case Some(true) => Some(token)
        case Some(false) => None
        case _ => findAtOrBefore(off - 1)(pred)
      }
    }

  final def filter(start: Token, end: Token)(
      predicate: Token => Boolean
  ): Seq[Token] = {
    if (start == end || nextToken(start) == start) Nil
    else {
      val tail = filter(nextToken(start), end)(predicate)
      if (predicate(start)) start +: tail
      else tail
    }
  }

}

object TokenTraverser {

  private def isTrivialPred(token: Token): Option[Boolean] =
    if (token.is[Token.Trivia]) None else Some(true)

}
