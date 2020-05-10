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
}

object Length {

  case class Num(n: Int) extends Length {
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
  }
}

case class ActualIndent(length: Int, expire: Token, expiresAt: ExpiresOn) {
  def notExpiredBy(ft: FormatToken): Boolean = {
    val expireToken: Token =
      if (expiresAt == ExpiresOn.After) ft.left else ft.right
    expire.end > expireToken.end
  }
}

abstract class Indent {
  def switch(switchObject: AnyRef): Indent
  def withStateOffset(offset: Int): Option[ActualIndent]
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
  override def switch(switchObject: AnyRef): Indent = this
  override def withStateOffset(offset: Int): Option[ActualIndent] =
    Some(ActualIndent(length.withStateOffset(offset), expire, expiresAt))
  override def toString: String = {
    val when = if (expiresAt == ExpiresOn.Before) '<' else '>'
    s"$length$when$expire:${expire.end}"
  }
}

object Indent {

  def apply(length: Length, expire: Token, expiresAt: ExpiresOn): Indent =
    length match {
      case Length.Num(0) => Empty
      case x => new IndentImpl(x, expire, expiresAt)
    }

  case object Empty extends Indent {
    override def withStateOffset(offset: Int): Option[ActualIndent] = None
    override def switch(switchObject: AnyRef): Indent = this
  }

  class Before(indent: Indent, before: AnyRef) extends Indent {
    override def switch(switchObject: AnyRef): Indent =
      if (before ne switchObject) this else Indent.Empty
    override def withStateOffset(offset: Int): Option[ActualIndent] =
      indent.withStateOffset(offset)
    override def toString: String = s"$indent>?"
  }

  class After(indent: Indent, after: AnyRef) extends Indent {
    override def switch(switchObject: AnyRef): Indent =
      if (after ne switchObject) this else indent
    override def withStateOffset(offset: Int): Option[ActualIndent] = None
    override def toString: String = s"?<$indent"
  }

}
