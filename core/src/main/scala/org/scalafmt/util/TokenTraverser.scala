package org.scalafmt.util

import scala.annotation.tailrec
import scala.meta.tokens.Token
import scala.meta.tokens.Tokens

class TokenTraverser(tokens: Tokens) {
  private[this] val tok2idx = {
    val map = Map.newBuilder[Token, Int]
    var i = 0
    tokens.foreach { tok =>
      map += (tok -> i)
      i += 1
    }
    map.result()
  }

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
  final def find(token: Token)(predicate: Token => Boolean):Option[Token] = {
    nextToken(token) match {
      case t if t == token => None
      case t if predicate(t) => Option(t)
      case t => find(t)(predicate)
    }
  }

  @tailrec
  final def reverseFind(token: Token)(predicate: Token => Boolean):Option[Token] = {
    prevToken(token) match {
      case t if t == token => None
      case t if predicate(t) => Option(t)
      case t => reverseFind(t)(predicate)
    }
  }

}
