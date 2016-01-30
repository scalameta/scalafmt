package org.scalafmt

/**
  * A state represents one potential solution to reach token at index,
  *
  * @param cost
  * @param splits
  * @param indentation
  * @param column
  */
case class State(cost: Int,
                 policy: Decision => Decision,
                 splits: Vector[Split],
                 indentation: Int,
                 pushes: Vector[Push],
                 column: Int) extends Ordered[State] with ScalaFmtLogger {

  import scala.math.Ordered.orderingToOrdered

  def compare(that: State): Int =
    (-this.cost, this.splits.length, -this.indentation) compare
      (-that.cost, that.splits.length, -that.indentation)

  /**
    * Returns True is this state will always return better formatting than other.
    */
  def alwaysBetter(other: State): Boolean =
    this.cost <= other.cost && this.indentation <= other.indentation

  override def toString = s"State($cost, ${splits.length}, $splits)"


  /**
    * Calculates next State given split at tok.
    *
    * - Accumulates cost and strategies
    * - Calculates column-width overflow penalty
    */
  def next(style: ScalaStyle,
           split: Split,
           tok: FormatToken): State = {
    val KILL = 10000
    val nonExpiredIndents = pushes.filterNot { push =>
      if (push.expiresAt == Left) push.expire == tok.left
      else push.expire == tok.right
    }
    val newIndents = nonExpiredIndents ++ split.indents.map {
      case Indent(StateColumn, t, left) =>
        Push(column - indentation, t, left)
      case Indent(Num(i), t, left) =>
        Push(i, t, left)
    }
    val newIndent = newIndents.foldLeft(0)(_ + _.num)
    // Always account for the cost of the right token.
    val newColumn = tok.right.code.length + (
      if (split.modification == Newline) newIndent
      else column + split.length)
    val splitWithPenalty =
      if (newColumn < style.maxColumn) split
      else split.withPenalty(KILL)
    val newPolicy =
      if (split.policy == NoPolicy) policy
      else (split.policy orElse IdentityPolicy) andThen policy
    State(cost + splitWithPenalty.cost,
      // TODO(olafur) expire policy, see #18.
      newPolicy,
      splits :+ splitWithPenalty,
      newIndent,
      newIndents,
      newColumn)
  }
}


object State extends ScalaFmtLogger {
  val start = State(0,
    identity,
    Vector.empty[Split],
    0,
    Vector.empty[Push],
    0)
  /**
    * Returns formatted output from FormatTokens and Splits.
    */
  def reconstructPath(toks: Array[FormatToken],
                      splits: Vector[Split],
                      style: ScalaStyle,
                      f: FormatToken => String = _.left.code
                     ): Seq[(FormatToken, String)] = {
    var state = State.start
    toks.zip(splits).map {
      case (tok, split) =>
//        logger.debug(s"${log(tok.left)} $split ${state.indents}")
        state = state.next(style, split, tok)
        val whitespace = split.modification match {
          case Space =>
            " "
          case Newline =>
            "\n" + " " * state.indentation
          case Newline2x =>
            "\n\n" + " " * state.indentation
          case _ =>
            ""
        }
        tok -> whitespace
    }
  }
}
