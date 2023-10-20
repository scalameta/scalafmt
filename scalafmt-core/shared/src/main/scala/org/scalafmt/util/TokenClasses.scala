package org.scalafmt.util

import scala.meta.Dialect
import scala.meta.internal.parsers.SoftKeywords
import scala.meta.tokens.Token
import scala.meta.tokens.Token._

object Reserved {
  def unapply(token: Token): Boolean = token match {
    case _: Keyword | _: KwFalse | _: KwNull | _: KwTrue => true
    case _ => false
  }
}

object LeftParenOrBracket {
  def unapply(tok: Token): Boolean =
    tok.is[LeftParen] || tok.is[LeftBracket]
}

object RightParenOrBracket {
  def unapply(tok: Token): Boolean =
    tok.is[RightParen] || tok.is[RightBracket]
}

object LeftParenOrBrace {
  def unapply(tok: Token): Boolean = tok.is[LeftParen] || tok.is[LeftBrace]
}

class SoftKeywordClasses(dialect: Dialect) extends SoftKeywords(dialect) {
  object ImplicitOrUsing {
    def unapply(tok: Token): Boolean = {
      tok.is[KwImplicit] || KwUsing.unapply(tok)
    }
  }

  object ExtendsOrDerives {
    def unapply(tok: Token): Boolean = {
      tok.is[KwExtends] || KwDerives.unapply(tok)
    }
  }
}
