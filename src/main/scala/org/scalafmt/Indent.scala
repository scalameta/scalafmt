package org.scalafmt

import scala.meta.tokens.Token

sealed trait ExpiresOn

case object Left extends ExpiresOn

case object Right extends ExpiresOn

sealed trait Length

case class Num(n: Int) extends Length

case object StateColumn extends Length

case class Push(num: Int, expire: Token, expiresAt: ExpiresOn)

case class Indent(num: Length, expire: Token, expiresAt: ExpiresOn) {
  require(num match {
    case Num(n) =>
      n > 0
    case _ => true
  })
}
