package org.scalafmt.util

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

}
