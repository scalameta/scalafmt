package org.scalafmt

import scala.meta.tokens.Token
import scala.meta.tokens.Token.Whitespace

case class FormatToken(left: Token, right: Token, between: Vector[Whitespace])


