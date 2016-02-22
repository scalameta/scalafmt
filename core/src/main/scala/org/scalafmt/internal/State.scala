package org.scalafmt.internal

import org.scalafmt.ScalaStyle

import scala.meta.tokens.Token

/**
  * A partial formatting solution up to splits.length number of tokens.
  */
final case class State(cost: Int,
                       policy: PolicySummary,
                       splits: Vector[Split],
                       indentation: Int,
                       pushes: Vector[Indent[Num]],
                       column: Int
                      ) extends Ordered[State] with ScalaFmtLogger {

  import scala.math.Ordered.orderingToOrdered

  def compare(that: State): Int =
    (-this.cost, this.splits.length, -this.indentation) compare(-that.cost,
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
    val nonExpiredIndents = pushes.filterNot { push =>
      val expireToken: Token =
        if (push.expiresAt == Left) tok.left
        else tok.right
      push.expire.end <= expireToken.end
    }
    val newIndents: Vector[Indent[Num]] =
      nonExpiredIndents ++ split.indents.map(_.withNum(column, indentation))
    val newIndent = newIndents.foldLeft(0)(_ + _.length.n)

    // Always account for the cost of the right token.
    val tokLength = tok.right.code.length

    // Some tokens contain newline, like multiline strings/comments.
    val lengthOnFirstLine = {
      val firstNewline = tok.right.code.indexOf('\n')
      if (firstNewline == -1) tokLength
      else firstNewline
    }
    val columnOnCurrentLine = lengthOnFirstLine + {
      if (split.modification.isNewline) newIndent
      else column + split.length
    }
    val lengthOnLastLine = {
      val lastNewline = tok.right.code.lastIndexOf('\n')
      if (lastNewline == -1) tokLength
      else tokLength - (lastNewline - 1)
    }
    val nextStateColumn = lengthOnLastLine + {
        if (split.modification.isNewline) newIndent
        else column + split.length
    }
    val newPolicy: PolicySummary = policy.combine(split.policy, tok.left.end)
    val splitWithPenalty = {
      if (columnOnCurrentLine < style.maxColumn) {
        split // fits inside column
      }
      else {
        split.withPenalty(KILL + columnOnCurrentLine) // overflow
      }
    }
    State(cost + splitWithPenalty.cost,
      // TODO(olafur) expire policy, see #18.
      newPolicy,
      splits :+ splitWithPenalty,
      newIndent,
      newIndents,
      nextStateColumn)
  }
}

object State extends ScalaFmtLogger {
  val start = State(0,
    PolicySummary.empty,
    Vector.empty[Split],
    0, Vector.empty[Indent[Num]], 0)

  /**
    * Returns formatted output from FormatTokens and Splits.
    */
  def reconstructPath(toks: Array[FormatToken], splits: Vector[Split],
                      style: ScalaStyle, debug: Boolean =
                      false): Seq[(FormatToken, String)] = {
    var state = State.start
    val result = toks.zip(splits).map {
      case (tok, split) =>
        // TIP. Use the following line to debug origin of splits.
        if (debug && toks.length < 1000) {
          val left = small(tok.left)
          logger.debug(f"$left%-10s $split ${state.column}")
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
