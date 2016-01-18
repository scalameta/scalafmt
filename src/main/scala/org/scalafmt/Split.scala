package org.scalafmt

import scala.meta.Tree
import scala.meta.tokens.Token
import scala.meta.tokens.Token._

case class Decision(formatToken: FormatToken, split: List[Split])

sealed abstract class Split(val cost: Int,
                            val indent: Int,
                            val policy: Decision => Decision = identity) {
  // TODO(olafur) Remove this ugly hack.
  private var penalty = false
  def setPenalty(): Unit = penalty = true
  def getPenalty: Boolean = penalty

  def length: Int = this match {
    case _: NoSplit => 0
    case _: Newline => 0
    case _: Space => 1
  }

}

// Direct subclasses.

class NoSplit(override val cost: Int) extends Split(cost, 0)

class Space(override val cost: Int,
            override val policy: Decision => Decision = identity)

  extends Split(cost: Int, 0, policy)

class Newline(override val cost: Int,
              override val indent: Int,
              override val policy: Decision => Decision = identity)
  extends Split(cost, indent, policy) {
  def reindent(decrement: Int) = new Newline(cost, indent + decrement, policy)
}

// objects

case object NoSplitFree extends NoSplit(0)

case class SpaceFree() extends Space(0)

class NewlineFree(indent: Int) extends Newline(0, indent)

case object Newline0 extends NewlineFree(0)

case object Newline_2 extends NewlineFree(-2)

case object Newline2 extends NewlineFree(2)

case object Newline_4 extends NewlineFree(-4)

case object Newline4 extends NewlineFree(4)


case class MultiLineBlock(override val cost: Int,
                          override val policy: Decision => Decision)
  extends Newline(cost, 2, policy)

case class SingeLineBlock(override val cost: Int,
                          override val policy: Decision => Decision)
  extends Space(cost, policy)

case class BreakStatement(override val cost: Int,
                          override val policy: Decision => Decision)
  extends Newline(cost, 2, policy)

object MultiLineBlock {
  def apply(cost: Int, open: `{`, owners: Map[Token, Tree]): MultiLineBlock = {
    MultiLineBlock(cost, {
      case Decision(tok@FormatToken(_, close: `}`, _), _)
        if owners.get(open) == owners.get(close) =>
        Decision(tok, List(Newline_2))
      case decision => decision
    })
  }
}


object SingeLineBlock {
  def apply(cost: Int, open: `{`, owners: Map[Token, Tree]): SingeLineBlock = {
    val end = owners(open).tokens.last.end
    SingeLineBlock(cost, {
      case Decision(tok, splits)
        // TODO(olafur) expire policy.
        if tok.right.end <= end =>
        Decision(tok, splits.filterNot(_.isInstanceOf[Newline]))
      case decision => decision
    })
  }
}

object BreakStatement {
  def apply(cost: Int, tok: Token, owners: Map[Token, Tree]): BreakStatement = {
    val parent = owners(tok)
    BreakStatement(cost, {
      case Decision(t@FormatToken(left, right, _), s)
        if childOf(left, parent, owners) && !childOf(right, parent, owners) =>
        Decision(t, s.map {
          case nl: Newline => nl.reindent(-2)
          case els => els
        })
      case decision => {
        decision
      }
    })
  }
}
