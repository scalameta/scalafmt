package org.scalafmt

import scala.meta.tokens.Token

case class Decision(formatToken: FormatToken, split: List[Split])

sealed trait Modification {
  def isNewline = this match {
    case _: NewlineT => true
    case _ => false
  }
}

case object NoSplit extends Modification

trait NewlineT extends Modification {
  def isDouble: Boolean = false
  def noIndent: Boolean = false
}

object NewlineT {
  def apply(isDouble: Boolean, noIndent: Boolean): NewlineT =
    (isDouble, noIndent) match {
      case (true, true) => NoIndent2xNewline
      case (true, false) => Newline2x
      case (false, true) => NoIndentNewline
      case _ => Newline
    }
}


case object Newline extends NewlineT {
}

case object Newline2x extends NewlineT {
  override def isDouble: Boolean = true
}

case object NoIndentNewline extends NewlineT {
  override def noIndent: Boolean = true
}

case object NoIndent2xNewline extends NewlineT {
  override def noIndent: Boolean = true
  override def isDouble: Boolean = true
}

case object Space extends Modification


/**
  * A Split is the whitespace between two non-whitespace tokens.
  *
  * Consider a split to be an edge in a search graph and [[FormatToken]]
  * are the nodes.
  *
  * @param modification Is this a space, no space, newline or 2 newlines?
  * @param cost How good is this output? Lower is better.
  * @param indents Does this add indentation?
  * @param policy How does this split affect other later splits?
  * @param penalty Does this split overflow the column limit?
  * @param origin For debugging only, to retrace how a particular output
  *               manifested.
  */
class Split(val modification: Modification,
            val cost: Int,
            val indents: List[Indent[Length]],
            val policy: Policy,
            val penalty: Boolean,
            val optimalAt: Option[Token],
            val origin: String) {

  def length: Int = modification match {
    case m if m.isNewline => 0
    case NoSplit => 0
    case Space => 1
  }

  def withOptimal(token: Token): Split =
    new Split(modification, cost, indents, policy, true, Some(token), origin)

  def withPolicy(newPolicy: Policy): Split = {
    val update = if (policy == NoPolicy) newPolicy else newPolicy orElse policy
    new Split(modification, cost, indents, update, true, optimalAt, origin)
  }

  def withPenalty(penalty: Int): Split =
    new Split(modification, cost + penalty, indents, policy, true, optimalAt, origin)

  def withIndent(length: Length, expire: Token, expiresOn: ExpiresOn): Split =
    new Split(modification, cost, Indent(length, expire, expiresOn) +: indents,
      policy, penalty, optimalAt, origin)

  def withModification(newModification: Modification): Split =
    new Split(newModification, cost, indents, policy, penalty, optimalAt, origin)

  override def toString =
    s"""$modification:$origin(cost=$cost${if (indents.nonEmpty) s", indent=$indents" else ""})"""

  // TODO(olafur) come with better concept of split equality.
  // For example update origin in withOptimal.
  def sameOrigin(other: Split) = this.origin == other.origin

}

object Split {
  def apply(modification: Modification, cost: Int)(implicit line: sourcecode.Line) =
    new Split(modification, cost, List.empty, NoPolicy, false, None, s"${line.value}")

}


