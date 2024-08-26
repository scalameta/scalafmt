package org.scalafmt.util

import scala.meta.Dialect
import scala.meta.internal.parsers.SoftKeywords
import scala.meta.tokens.Token
import scala.meta.tokens.Token._

sealed trait TokenClassifier extends Function[Token, Boolean] {
  def matches(token: Token): Boolean
  def apply(token: Token): Boolean = matches(token)
  def unapply(token: Token): Boolean = matches(token)
}

object Reserved extends TokenClassifier {
  def matches(token: Token): Boolean = token match {
    case _: Keyword | _: KwFalse | _: KwNull | _: KwTrue => true
    case _ => false
  }
}

object LeftParenOrBracket extends TokenClassifier {
  def matches(tok: Token): Boolean = tok.isAny[LeftParen, LeftBracket]
}

object RightParenOrBracket extends TokenClassifier {
  def matches(tok: Token): Boolean = tok.isAny[RightParen, RightBracket]
}

object LeftParenOrBrace extends TokenClassifier {
  def matches(tok: Token): Boolean = tok.isAny[LeftParen, LeftBrace]
}

class SoftKeywordClasses(dialect: Dialect) extends SoftKeywords(dialect) {
  object ImplicitOrUsing extends TokenClassifier {
    def matches(tok: Token): Boolean = tok.is[KwImplicit] || KwUsing.matches(tok)
  }

  object ExtendsOrDerives extends TokenClassifier {
    def matches(tok: Token): Boolean = tok.is[KwExtends] ||
      KwDerives.matches(tok)
  }
}
