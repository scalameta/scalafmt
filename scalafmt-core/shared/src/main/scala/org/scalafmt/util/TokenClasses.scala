package org.scalafmt.util

import scala.meta.Dialect
import scala.meta.internal.classifiers.classifier
import scala.meta.internal.parsers.SoftKeywords
import scala.meta.tokens.Token
import scala.meta.tokens.Token._

@classifier
trait Keyword
object Keyword {
  def unapply(token: Token): Boolean = {
    token.is[KwAbstract] || token.is[KwCase] || token.is[KwCatch] ||
    token.is[KwClass] || token.is[KwDef] || token.is[KwDo] ||
    token.is[KwElse] || token.is[KwEnum] || token.is[KwExtends] ||
    token.is[KwFalse] || token.is[KwFinal] || token.is[KwFinally] ||
    token.is[KwFor] || token.is[KwForsome] || token.is[KwIf] ||
    token.is[KwImplicit] || token.is[KwImport] || token.is[KwLazy] ||
    token.is[KwMatch] || token.is[KwMacro] || token.is[KwNew] ||
    token.is[KwNull] || token.is[KwObject] || token.is[KwOverride] ||
    token.is[KwPackage] || token.is[KwPrivate] || token.is[KwProtected] ||
    token.is[KwReturn] || token.is[KwSealed] || token.is[KwSuper] ||
    token.is[KwThis] || token.is[KwThrow] || token.is[KwTrait] ||
    token.is[KwTrue] || token.is[KwTry] || token.is[KwType] ||
    token.is[KwVal] || token.is[KwVar] || token.is[KwWhile] ||
    token.is[KwWith] || token.is[KwYield]
  }
}

@classifier
trait Delim
object Delim {
  def unapply(token: Token): Boolean = {
    token.is[Hash] || token.is[Colon] || token.is[Viewbound] ||
    token.is[LeftArrow] || token.is[Subtype] || token.is[Equals] ||
    token.is[RightArrow] || token.is[Supertype] || token.is[At] ||
    token.is[Underscore] || token.is[LeftParen] || token.is[RightParen] ||
    token.is[Comma] || token.is[Dot] || token.is[Semicolon] ||
    token.is[LeftBracket] || token.is[RightBracket] || token.is[LeftBrace] ||
    token.is[RightBrace]
  }
}

@classifier
trait Modifier
object Modifier {
  def unapply(token: Token): Boolean = {
    token.is[KwAbstract] || token.is[KwFinal] || token.is[KwSealed] ||
    token.is[KwImplicit] || token.is[KwLazy] || token.is[KwPrivate] ||
    token.is[KwProtected] || token.is[KwOverride]
  }
}

@classifier
trait Literal
object Literal {
  def unapply(token: Token): Boolean = {
    token.is[Constant.Int] || token.is[Constant.Long] ||
    token.is[Constant.Float] || token.is[Constant.Double] ||
    token.is[Constant.Char] || token.is[Constant.Symbol] ||
    token.is[Constant.String] || token.is[KwNull] || token.is[KwTrue] ||
    token.is[KwFalse]
  }
}

@classifier
trait Whitespace
object Whitespace {
  def unapply(token: Token): Boolean = {
    token.is[Space] || token.is[Tab] || token.is[CR] || token.is[LF] ||
    token.is[FF]
  }
}

@classifier
trait Trivia
object Trivia {
  def unapply(token: Token): Boolean = {
    token.is[Whitespace] || token.is[Comment]
  }
}

@classifier
trait LeftParenOrBracket
object LeftParenOrBracket {
  def unapply(tok: Token): Boolean =
    tok.is[LeftParen] || tok.is[LeftBracket]
}

@classifier
trait RightParenOrBracket
object RightParenOrBracket {
  def unapply(tok: Token): Boolean =
    tok.is[RightParen] || tok.is[RightBracket]
}

@classifier
trait LeftParenOrBrace
object LeftParenOrBrace {
  def unapply(tok: Token): Boolean = tok.is[LeftParen] || tok.is[LeftBrace]
}

class SoftKeywordClasses(dialect: Dialect) extends SoftKeywords(dialect) {
  @classifier
  trait ImplicitOrUsing
  object ImplicitOrUsing {
    def unapply(tok: Token): Boolean = {
      tok.is[KwImplicit] || tok.is[KwUsing]
    }
  }
}
