package org.scalafmt

import scala.meta.Tree
import scala.meta.tokens.Token
import scala.meta.tokens.Token._

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
            val indent: List[Indent] = List.empty[Indent],
            val policy: Policy = NoPolicy,
            val penalty: Boolean = false) {

  def length: Int = modification match {
    case NoSplit => 0
    case Newline | Newline2x => 0
    case Space => 1
  }

  def withPenalty(penalty: Int): Split =
    new Split(modification, cost + penalty, indent, policy, true)

  def withIndent(newIndent: Indent): Split =
    new Split(modification, cost, newIndent +: indent, policy, penalty)

  def withModification(newModification: Modification): Split =
    new Split(newModification, cost, indent, policy, penalty)

  override def toString =
    s"""$modification(cost=$cost${if (indent.nonEmpty) s", indent=$indent" else ""})"""

}

object Split {
  object NoSplit0 extends Split(NoSplit, 0)
  object Space0 extends Split(Space, 0)
  object Newline0 extends Split(Newline, 0)

  def apply(modification: Modification,
            cost: Int,
            indent: Indent = NoOp,
            policy: Policy = NoPolicy,
            penalty: Boolean = false) =
    new Split(modification, cost, List(indent), policy, penalty)

}


