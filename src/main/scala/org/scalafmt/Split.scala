package org.scalafmt

import scala.meta.Tree
import scala.meta.tokens.Token
import scala.meta.tokens.Token._

case class Decision(state: State, formatToken: FormatToken, split: List[Split])

sealed trait Modification

case object NoSplit extends Modification

case object Newline extends Modification

case object Space extends Modification


class Split(val modification: Modification,
            val cost: Int,
            val indent: List[Indent] = List.empty[Indent],
            val policy: Policy = NoPolicy,
            val penalty: Boolean = false) {

  def length: Int = modification match {
    case NoSplit => 0
    case Newline => 0
    case Space => 1
  }

  def withPenalty(penalty: Int): Split =
    new Split(modification, cost + penalty, indent, policy, true)

  def withIndent(newIndent: Indent): Split =
    new Split(modification, cost, newIndent +: indent, policy, penalty)

}

object Split {
  val NoSplit0 = Split(NoSplit, 0)
  val Space0 = Split(Space, 0)
  val Newline0 = Split(Newline, 0)

  def apply(modification: Modification,
            cost: Int,
            indent: Indent = NoOp,
            policy: Policy = NoPolicy,
            penalty: Boolean = false) =
    new Split(modification, cost, List(indent), policy, penalty)

}

// objects
object Unindent {
  def apply(open: Delim, owners: Map[Token, Tree]): Policy = {
    case Decision(state, t@FormatToken(close: `)`, _, _), splits)
      if open.isInstanceOf[`(`] && owners.get(open) == owners.get(close) =>
      Decision(state, t, splits.map(_.withIndent(Pop)))
    case Decision(state, t@FormatToken(close: `]`, _, _), splits)
      if open.isInstanceOf[`[`] && owners.get(open) == owners.get(close) =>
      Decision(state, t, splits.map(_.withIndent(Pop)))
  }
}

object OneArgOneLineSplit {
  def apply(open: Delim, owners: Map[Token, Tree]): Policy =
    Unindent(open, owners) orElse {
      // Unindent on close ).
      // Newline on every comma.
      case Decision(state, t@FormatToken(comma: `,`, _, _), splits)
        if owners.get(open) == owners.get(comma) =>
        Decision(state, t, splits.filter(_.modification == Newline))
      // TODO(olafur) Make policy partial function.
    }
}

object MultiLineBlock extends ScalaFmtLogger {
  def apply(open: `{`, owners: Map[Token, Tree]): Policy = {
    case Decision(state, tok@FormatToken(_, close: `}`, _), splits)
      if owners.get(open) == owners.get(close) =>
      Decision(state, tok, splits.withFilter(_.modification == Newline).map {
        case s => s.withIndent(Pop)
      })
  }
}


object SingleLineBlock {
  def apply(open: Delim, owners: Map[Token, Tree]): Policy = {
    val end = owners(open).tokens.last.end
    val policy: Policy = {
      case Decision(state, tok, splits)
        if tok.right.end <= end =>
        Decision(state, tok, splits.filterNot(_.modification == Newline))
    }
    policy
  }
}

object BreakStatement {
  def apply(cost: Int, tok: Token, owners: Map[Token, Tree]): Split = {
    val parent = owners(tok)
    Split(Newline, cost, Push(2), {
      case Decision(state, t@FormatToken(left, right, _), s)
        if childOf(left, parent, owners) && !childOf(right, parent, owners) =>
        Decision(state, t, s.map(_.withIndent(Pop)))
    })
  }
}

