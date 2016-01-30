package org.scalafmt

import scala.meta.tokens.Token

case class Decision(formatToken: FormatToken, split: List[Split])

sealed trait Modification {
  def isNewline = this match {
    case Newline | Newline2x => true
    case _ => false
  }
}

case object NoSplit extends Modification

case object Newline extends Modification

case object Newline2x extends Modification

case object Space extends Modification


class Split(val modification: Modification,
            val cost: Int,
            val indents: List[Indent] = List.empty[Indent],
            val policy: Policy = NoPolicy,
            val penalty: Boolean = false,
            val origin: String) {

  def length: Int = modification match {
    case NoSplit => 0
    case Newline | Newline2x => 0
    case Space => 1
  }

  def withPenalty(penalty: Int): Split =
    new Split(modification, cost + penalty, indents, policy, true, origin)

  def withIndent(length: Length, expire: Token, expiresOn: ExpiresOn): Split =
    new Split(modification, cost, Indent(length, expire, expiresOn) +: indents,
      policy, penalty, origin)

  def withModification(newModification: Modification): Split =
    new Split(newModification, cost, indents, policy, penalty, origin)

  override def toString =
    s"""$modification:$origin(cost=$cost${if (indents.nonEmpty) s", indent=$indents" else ""})"""

}

object Split {
  def NoSplit0(implicit file: sourcecode.File, line: sourcecode.Line) =
    new Split(NoSplit, 0, origin = s"${line.value}")
  def Space0(implicit file: sourcecode.File, line: sourcecode.Line) =
    new Split(Space, 0, origin = s"${line.value}")
  def Newline0(implicit file: sourcecode.File, line: sourcecode.Line) =
    new Split(Newline, 0, origin = s"${line.value}")

  def apply(modification: Modification,
            cost: Int,
            policy: Policy = NoPolicy)(
    implicit line: sourcecode.Line) =
    new Split(modification, cost, List.empty[Indent], policy, false,
      s"${line.value}")

}


