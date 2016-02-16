package org.scalafmt.internal

import scala.meta.tokens.Token

sealed trait ExpiresOn

case object Left extends ExpiresOn

case object Right extends ExpiresOn

sealed trait Length

case class Num(n: Int) extends Length

case object StateColumn extends Length

case class Indent[T <: Length](length: T, expire: Token, expiresAt: ExpiresOn) {
  require(length match {
    case Num(n) => n > 0
    case _ => true
  })

  def withNum(column: Int, indentation: Int): Indent[Num] =
    length match {
      case n: Num =>
        Indent(n, expire, expiresAt)
      // Can't match against StateColumn :/
      case _ => Indent(Num(column - indentation), expire, expiresAt)
    }
}