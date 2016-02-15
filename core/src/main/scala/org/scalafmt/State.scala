package org.scalafmt

/**
  * A state represents one potential solution to reach token at index,
  *
  * @param cost
  * @param splits
  * @param indentation
  * @param column
  */
final case class State(cost: Int, policy: Decision => Decision,
    splits: Vector[Split], indentation: Int, pushes: Vector[Indent[Num]],
    column: Int) extends Ordered[State] with ScalaFmtLogger {
  import scala.math.Ordered.orderingToOrdered

  def compare(that: State): Int =
    (-this.cost, this.splits.length, -this.indentation) compare (-that.cost,
      that.splits.length, -that.indentation)

  /**
    * Returns True is this state will always return better formatting than other.
    */
  def alwaysBetter(other: State): Boolean =
    this.cost <= other.cost && this.indentation <= other.indentation

  override def toString = s"State($cost, ${splits.length})"

  /**
    * Calculates next State given split at tok.
    *
    * - Accumulates cost and strategies
    * - Calculates column-width overflow penalty
    */
  def next(style: ScalaStyle, split: Split,
          tok: FormatToken): State = {
    val KILL = 10000
    val nonExpiredIndents = pushes.filterNot {
      push =>
      if (push.expiresAt == Left) push.expire == tok.left
      else push.expire == tok.right
    }
    val newIndents: Vector[Indent[Num]] =
      nonExpiredIndents ++ split.indents.map(_.withNum(column, indentation))
    val newIndent = newIndents.foldLeft(0)(_ + _.length.n)
    // Always account for the cost of the right token.
    val newColumn =
      tok.right.code.length + (
        if (split.modification.isNewline) newIndent
        else column + split.length)
    val splitWithPenalty =
      if (newColumn < style.maxColumn) split
      else split.withPenalty(KILL + newColumn) // minimize overflow
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
  val start = State(0, identity, Vector.empty[Split], 0, Vector.empty[Indent[Num]], 0)

  /**
    * Returns formatted output from FormatTokens and Splits.
    */
  def reconstructPath(toks: Array[FormatToken], splits: Vector[Split],
          style: ScalaStyle, debug: Boolean =
          false): Seq[( FormatToken, String)] = {
    var state = State.start
    val result = toks.zip(splits).map {
      case (tok, split) =>
        // TIP. Use the following line to debug origin of splits.
        if (debug) {
          val left = small(tok.left)
          logger.debug(f"$left%-10s $split")
        }
        state = state.next(style, split, tok)
        val whitespace = split.modification match {
          case Space => " "
          case nl: NewlineT =>
            val newline =
              if (nl.isDouble) "\n\n"
              else "\n"
            val indentation =
              if (nl.noIndent) ""
              else " " * state.indentation
            newline + indentation
          case Provided(literal) => literal
          case NoSplit => ""
        }
        tok -> whitespace
    }
    if (debug) logger.debug(s"Total cost: ${state.cost}")
    result
  }
}
