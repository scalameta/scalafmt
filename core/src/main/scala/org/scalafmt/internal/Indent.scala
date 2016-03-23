package org.scalafmt.internal

import scala.meta.tokens.Token

sealed trait ExpiresOn

case object Left extends ExpiresOn

case object Right extends ExpiresOn

sealed trait Length

case class Num(n: Int) extends Length

/**
  * Indent up to the column of the left token.
  *
  * Example: the opening parenthesis below indents by [[StateColumn]].
  *
  * foobar(arg1,
  *        arg2)
  */
case object StateColumn extends Length

/**
  * One layer of indentation, created by an opening (, {, etc.
  *
  * Indent is parameterized by some [[Length]] to allow splits from
  * [[Router]] to be memoized. If the length field was int, we would have to
  * eagerly evaluate the indentation for state columns, which may be different
  * depending on the formatting we choose.
  *
  * @param length lengt of indentation, can be negative (in rare cases, with
  *               deeply nested terms with no newlines).
  * @param expire Until which token does this indentation stay?
  * @param expiresAt If Right, then expires when [[expire]] is curr.right,
  *                  otherwise curr.left in [[BestFirstSearch]].
  * @tparam T Can be a known number [[Num]] (used in [[State]]) or unknown
  *           integer [[StateColumn]] (used in [[Split]]).
  */
case class Indent[T <: Length](length: T, expire: Token, expiresAt: ExpiresOn) {

  def withNum(column: Int, indentation: Int): Indent[Num] = length match {
    case n: Num => Indent(n, expire, expiresAt)
    case _: StateColumn.type =>
      Indent(Num(column - indentation), expire, expiresAt)
  }
}
