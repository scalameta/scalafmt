package org.scalafmt.util

import scala.annotation.tailrec
import scala.meta.tokens.Token
import scala.meta.tokens.Tokens

class TokenTraverser(tokens: Tokens) {
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
    (map.result(), excluded.result())
  }

  final def isExcluded(token: Token): Boolean =
    excludedTokens.contains(TokenOps.hash(token))

  def nextToken(token: Token): Token = {
    tok2idx.get(token) match {
      case Some(i) if tokens.length > i + 1 =>
        tokens(i + 1)
      case _ => token
    }
  }

  def prevToken(token: Token): Token = {
    tok2idx.get(token) match {
      case Some(i) if tokens.length > i - 1 =>
        tokens(i - 1)
      case _ => token
    }
  }

  @tailrec
  final def find(token: Token)(predicate: Token => Boolean): Option[Token] = {
    nextToken(token) match {
      case t if t == token => None
      case t if predicate(t) => Option(t)
      case t => find(t)(predicate)
    }
  }

  @tailrec
  final def reverseFind(
      token: Token
  )(predicate: Token => Boolean): Option[Token] = {
    prevToken(token) match {
      case t if t == token => None
      case t if predicate(t) => Option(t)
      case t => reverseFind(t)(predicate)
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
