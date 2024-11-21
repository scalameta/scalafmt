package org.scalafmt.util

import scala.meta.Dialect
import scala.meta.internal.parsers.SoftKeywords
import scala.meta.tokens.Token._
import scala.meta.tokens.{Token => T}

sealed trait TokenClassifier extends Function[T, Boolean] {
  def matches(token: T): Boolean
  def apply(token: T): Boolean = matches(token)
  def unapply(token: T): Boolean = matches(token)
}

object Reserved extends TokenClassifier {
  def matches(token: T): Boolean = token match {
    case _: Keyword | _: KwNull | _: BooleanConstant => true
    case _ => false
  }
}

object LeftParenOrBracket extends TokenClassifier {
  def matches(tok: T): Boolean = tok.isAny[LeftParen, LeftBracket]
}

object RightParenOrBracket extends TokenClassifier {
  def matches(tok: T): Boolean = tok.isAny[RightParen, RightBracket]
}

object LeftParenOrBrace extends TokenClassifier {
  def matches(tok: T): Boolean = tok.isAny[LeftParen, LeftBrace]
}

class SoftKeywordClasses(dialect: Dialect) extends SoftKeywords(dialect) {
  object ImplicitOrUsing extends TokenClassifier {
    def matches(tok: T): Boolean = tok.is[KwImplicit] || KwUsing.matches(tok)
  }

  object ExtendsOrDerives extends TokenClassifier {
    def matches(tok: T): Boolean = tok.is[KwExtends] || KwDerives.matches(tok)
  }
}
