package org.scalafmt.internal

import org.scalafmt.ScalaStyle
import org.scalafmt.util.LoggerOps
import LoggerOps._
import org.scalafmt.util.TokenOps

import scala.meta.tokens.Token
import scala.meta.tokens.Token.Comment

/**
  * A partial formatting solution up to splits.length number of tokens.
  */
final case class State(cost: Int,
                       policy: PolicySummary,
                       splits: Vector[Split],
                       indentation: Int,
                       pushes: Vector[Indent[Num]],
                       column: Int,
                       formatOff: Boolean) extends Ordered[State] {
  import TokenOps._

  def compare(that: State): Int = {
    val costCompare = Integer.valueOf(-this.cost).compareTo(-that.cost)
    if (costCompare != 0) costCompare
    else {
      val splitsCompare =
        Integer.valueOf(this.splits.length).compareTo(that.splits.length)
      if (splitsCompare != 0) splitsCompare
      else Integer.valueOf(-this.indentation).compareTo(-that.indentation)
    }
  }

  /**
    * Returns True is this state will always return better formatting than other.
    */
  def alwaysBetter(other: State): Boolean =
    this.cost <= other.cost && this.indentation <= other.indentation

  override def toString = s"State($cost, ${splits.length})"

  /**
    * Calculates next State given split at tok.
    */
  def next(style: ScalaStyle, split: Split, tok: FormatToken): State = {
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
    val lengthOnFirstLine = tokenLength(tok.right)
    val columnOnCurrentLine =
      lengthOnFirstLine + {
        if (split.modification.isNewline) newIndent
        else column + split.length
      }
    val lengthOnLastLine = {
      val lastNewline = tok.right.code.lastIndexOf('\n')
      if (lastNewline == -1) tokLength
      else tokLength - lastNewline - 1
    }
    val nextStateColumn =
      lengthOnLastLine + {
        // Tokens with newlines get no indentation.
        if (tok.right.code.contains('\n')) 0
        else if (split.modification.isNewline) newIndent
        else column + split.length
      }
    val newPolicy: PolicySummary = policy.combine(split.policy, tok.left.end)
    val splitWithPenalty = {
      if (columnOnCurrentLine < style.maxColumn || {
            val commentExceedsLineLength =
              tok.right.isInstanceOf[Comment] &&
              tok.right.code.length >= (style.maxColumn - newIndent)
            commentExceedsLineLength && split.modification.isNewline
          }) {
        split // fits inside column
      } else {
        split.withPenalty(KILL + columnOnCurrentLine) // overflow
      }
    }

    val nextFormatOff =
      if (TokenOps.isFormatOff(tok.right)) true
      else if (TokenOps.isFormatOn(tok.right)) false
      else formatOff

    State(cost + splitWithPenalty.cost,
          // TODO(olafur) expire policy, see #18.
          newPolicy,
          splits :+ splitWithPenalty,
          newIndent,
          newIndents,
          nextStateColumn,
          nextFormatOff)
  }
}

object State {
  val start = State(0,
                    PolicySummary.empty,
                    Vector.empty[Split],
                    0,
                    Vector.empty[Indent[Num]],
                    0,
                    formatOff = false)

  /**
    * Reconstructs path for all tokens and invokes callback for each token/split combination.
    */
  def reconstructPath(toks: Array[FormatToken],
                      splits: Vector[Split],
                      style: ScalaStyle,
                      debug: Boolean = false)(
      callback: (State, FormatToken, String) => Unit): Unit = {
    var state = State.start
    toks
      .zip(splits)
      .foreach {
        case (tok, split) =>
          // TIP. Use the following line to debug origin of splits.
          if (debug && toks.length < 1000) {
            val left = cleanup(tok.left).slice(0, 15)
            logger
              .debug(f"$left%-15s $split ${state.indentation} ${state.column}")
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
          callback.apply(state, tok, whitespace)
      }
    if (debug) logger.debug(s"Total cost: ${state.cost}")
  }
}
