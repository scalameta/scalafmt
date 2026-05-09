package org.scalafmt.util

import scala.meta.Dialect
import scala.meta.classifiers._
import scala.meta.internal.parsers.SoftKeywords
import scala.meta.tokens.{Token => T}

sealed trait TokenClassifier extends Function[T, Boolean] {
  def matches(token: T): Boolean
  def apply(token: T): Boolean = matches(token)
  def unapply(token: T): Boolean = matches(token)
}

object Reserved extends TokenClassifier {
  def matches(token: T): Boolean = token match {
    case _: T.Keyword | _: T.KwNull | _: T.BooleanConstant => true
    case _ => false
  }
}

object LeftParenOrBracket extends TokenClassifier {
  def matches(tok: T): Boolean = tok.isAny[T.LeftParen, T.LeftBracket]
}

object RightParenOrBracket extends TokenClassifier {
  def matches(tok: T): Boolean = tok.isAny[T.RightParen, T.RightBracket]
}

object LeftParenOrBrace extends TokenClassifier {
  def matches(tok: T): Boolean = tok.isAny[T.LeftParen, T.LeftBrace]
}

class SoftKeywordClasses(dialect: Dialect) extends SoftKeywords(dialect) {
  object ImplicitOrUsing extends TokenClassifier {
    def matches(tok: T): Boolean = tok.is[T.KwImplicit] || KwUsing.matches(tok)
  }

  object ExtendsOrDerives extends TokenClassifier {
    def matches(tok: T): Boolean = tok.is[T.KwExtends] || KwDerives.matches(tok)
  }
}
