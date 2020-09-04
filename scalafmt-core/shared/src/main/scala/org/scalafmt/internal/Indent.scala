package org.scalafmt.internal

import scala.meta.tokens.Token

sealed abstract class ExpiresOn

object ExpiresOn {
  case object After extends ExpiresOn

  case object Before extends ExpiresOn

  @inline
  def beforeIf(flag: Boolean) = if (flag) Before else After
}

sealed abstract class Length {
  def withStateOffset(offset: Int): Int
  val reset: Boolean
}

object Length {

  case class Num(n: Int, reset: Boolean = false) extends Length {
    override def withStateOffset(offset: Int): Int = n
    override def toString: String = n.toString
  }

  /**
    * Indent up to the column of the left token.
    *
    * Example: the opening parenthesis below indents by [[StateColumn]].
    *
    * foobar(arg1,
    *        arg2)
    */
  case object StateColumn extends Length {
    override def withStateOffset(offset: Int): Int = offset
    override val reset: Boolean = false
  }
}

case class ActualIndent(
    length: Int,
    expire: Token,
    expiresAt: ExpiresOn,
    reset: Boolean
) {
  def notExpiredBy(ft: FormatToken): Boolean = {
    val expireToken: Token =
      if (expiresAt == ExpiresOn.After) ft.left else ft.right
    expire.end > expireToken.end
  }
}

abstract class Indent {
  def switch(trigger: Token): Indent
  def withStateOffset(offset: Int): Option[ActualIndent]
  def hasStateColumn: Boolean
}

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
  */
private class IndentImpl(length: Length, expire: Token, expiresAt: ExpiresOn)
    extends Indent {
  override def hasStateColumn: Boolean = length eq Length.StateColumn
  override def switch(trigger: Token): Indent = this
  override def withStateOffset(offset: Int): Option[ActualIndent] =
    Some(
      ActualIndent(
        length.withStateOffset(offset),
        expire,
        expiresAt,
        length.reset
      )
    )
  override def toString: String = {
    val when = if (expiresAt == ExpiresOn.Before) '<' else '>'
    s"$length$when$expire:${expire.end}"
  }
}

object Indent {

  def apply(length: Length, expire: => Token, expiresAt: => ExpiresOn): Indent =
    length match {
      case Length.Num(0, _) => Empty
      case x => new IndentImpl(x, expire, expiresAt)
    }

  case object Empty extends Indent {
    override def withStateOffset(offset: Int): Option[ActualIndent] = None
    override def switch(trigger: Token): Indent = this
    override def hasStateColumn: Boolean = false
  }

  class Switch private (before: Indent, trigger: Token, after: Indent)
      extends Indent {
    override def switch(trigger: Token): Indent =
      if (trigger ne this.trigger) this else after.switch(trigger)
    override def withStateOffset(offset: Int): Option[ActualIndent] =
      before.withStateOffset(offset)
    override def hasStateColumn: Boolean = before.hasStateColumn
    override def toString: String = s"$before>($trigger:${trigger.end})?$after"
  }

  object Switch {
    def apply(before: Indent, trigger: Token, after: Indent): Indent =
      if (before.eq(Indent.Empty) && after.eq(Indent.Empty)) Indent.Empty
      else new Switch(before, trigger, after)
  }

  def before(indent: Indent, trigger: Token): Indent =
    Switch(indent, trigger, Indent.Empty)

  def after(trigger: Token, indent: Indent): Indent =
    Switch(Indent.Empty, trigger, indent)

  def getIndent(indents: Iterable[ActualIndent]): Int =
    indents.foldLeft(0) {
      case (res, elem) =>
        if (elem.reset) elem.length
        else res + elem.length
    }

}
