package org.scalafmt

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
            val penalty: Boolean = false,
            val origin: String) {

  def length: Int = modification match {
    case NoSplit => 0
    case Newline | Newline2x => 0
    case Space => 1
  }

  def withPenalty(penalty: Int): Split =
    new Split(modification, cost + penalty, indent, policy, true, origin)

  def withIndent(newIndent: Indent): Split =
    new Split(modification, cost, newIndent +: indent, policy, penalty, origin)

  def withModification(newModification: Modification): Split =
    new Split(newModification, cost, indent, policy, penalty, origin)

  override def toString =
    s"""$modification:$origin(cost=$cost${if (indent.nonEmpty) s", indent=$indent" else ""})"""

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
            indent: Indent = NoOp,
            policy: Policy = NoPolicy,
            penalty: Boolean = false)(
    implicit line: sourcecode.Line) =
    new Split(modification, cost, List(indent), policy, penalty,
      s"${line.value}")

}


