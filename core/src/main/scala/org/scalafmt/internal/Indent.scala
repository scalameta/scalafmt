package org.scalafmt.internal

import scala.meta.tokens.Token

sealed trait ExpiresOn

case object Left extends ExpiresOn

case object Right extends ExpiresOn

sealed trait Length

case class Num(n: Int) extends Length

case object StateColumn extends Length

/**
  * One layer of indentation, created by an opening (, {, etc.
  *
  * @param length lengt of indentation, can be negative (in rare cases, with
  *               deeply nested terms with no newlines).
  * @param expire Until which token does this indentation stay?
  * @param expiresAt If Right, then expires when [[expire]] is curr.right,
  *                  otherwise curr.left in [[BestFirstSearch]].
  * @tparam T Can be a known number [[Num]] or unknown integer [[StateColumn]].
  */
case class Indent[T <: Length](length: T, expire: Token, expiresAt: ExpiresOn) {

  def withNum(column: Int, indentation: Int): Indent[Num] =
    length match {
      case n: Num => Indent(n, expire, expiresAt)
      // Can't match against StateColumn :/
      case _ => Indent(Num(column - indentation), expire, expiresAt)
    }
}
