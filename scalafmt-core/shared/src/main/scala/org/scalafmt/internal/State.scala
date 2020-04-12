package org.scalafmt.internal

import scala.annotation.tailrec
import scala.meta.tokens.Token
import scala.meta.tokens.Token.Comment

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.TokenOps

/**
  * A partial formatting solution up to splits.length number of tokens.
  */
final case class State(
    cost: Int,
    policy: PolicySummary,
    split: Split,
    depth: Int,
    prev: State,
    indentation: Int,
    pushes: Vector[ActualIndent],
    column: Int,
    formatOff: Boolean
) {

  override def toString = s"State($cost, $depth)"

  def alwaysBetter(other: State): Boolean =
    this.cost <= other.cost && this.indentation <= other.indentation

  /**
    * Calculates next State given split at tok.
    */
  def next(
      style: ScalafmtConfig,
      split: Split,
      tok: FormatToken
  ): State = {
    val newIndents: Vector[ActualIndent] =
      if (tok.right.is[Token.EOF]) Vector.empty
      else {
        val offset = column - indentation
        pushes.filterNot { push =>
          val expireToken: Token =
            if (push.expiresAt == ExpiresOn.After) tok.left else tok.right
          push.expire.end <= expireToken.end
        } ++ split.indents.flatMap(_.withStateOffset(offset))
      }
    val newIndent = newIndents.foldLeft(0)(_ + _.length)

    val tokRightSyntax = tok.right.syntax
    // Always account for the cost of the right token.
    val tokLength = tokRightSyntax.length

    // Some tokens contain newline, like multiline strings/comments.
    val lengthOnFirstLine = TokenOps.tokenLength(tok.right)
    val columnOnCurrentLine =
      lengthOnFirstLine + {
        if (split.modification.isNewline) newIndent
        else column + split.length
      }
    val lengthOnLastLine = {
      val lastNewline = tokRightSyntax.lastIndexOf('\n')
      if (lastNewline == -1) tokLength
      else tokLength - lastNewline - 1
    }
    val nextStateColumn =
      lengthOnLastLine + {
        // Tokens with newlines get no indentation.
        if (tokRightSyntax.contains('\n')) 0
        else if (split.modification.isNewline) newIndent
        else column + split.length
      }
    val newPolicy: PolicySummary = policy.combine(split.policy, tok.left.end)
    val splitWithPenalty = {
      if (columnOnCurrentLine <= style.maxColumn || {
          val commentExceedsLineLength =
            tok.right.is[Comment] &&
              tokRightSyntax.length >= (style.maxColumn - newIndent)
          commentExceedsLineLength && split.modification.isNewline
        }) {
        split // fits inside column
      } else {
        split.withPenalty(
          Constants.ExceedColumnPenalty + columnOnCurrentLine
        ) // overflow
      }
    }

    val nextFormatOff =
      if (TokenOps.isFormatOff(tok.right)) true
      else if (TokenOps.isFormatOn(tok.right)) false
      else formatOff

    State(
      cost + splitWithPenalty.cost,
      // TODO(olafur) expire policy, see #18.
      newPolicy,
      splitWithPenalty,
      depth + 1,
      this,
      newIndent,
      newIndents,
      nextStateColumn,
      nextFormatOff
    )
  }

}

object State {

  val start = State(
    0,
    PolicySummary.empty,
    null,
    0,
    null,
    0,
    Vector.empty[ActualIndent],
    0,
    formatOff = false
  )

  // this is not best state, it's higher priority for search
  object Ordering extends Ordering[State] {
    override def compare(x: State, y: State): Int = compareAt(x, y, 0)

    // each should compare priorities, i.e. define reverse ordering
    private val comparisons: Seq[(State, State) => Int] = Seq(
      compareCost,
      compareDepth,
      compareSplitOrigin
    )

    @tailrec
    private def compareAt(s1: State, s2: State, i: Int): Int = {
      val r = comparisons(i)(s1, s2)
      if (r != 0 || i == comparisons.length - 1) r
      else compareAt(s1, s2, i + 1)
    }

    // higher priority on lower cost
    private def compareCost(s1: State, s2: State): Int =
      Integer.compare(s2.cost, s1.cost)

    // higher priority on deeper state
    private def compareDepth(s1: State, s2: State): Int =
      Integer.compare(s1.depth, s2.depth)

    // higher priority on later line defining the last split
    @tailrec
    private def compareSplitOrigin(s1: State, s2: State): Int = {
      // We assume the same number of splits, see compareSplitsLength
      // Break ties by the last split's line origin.
      val r = Integer.compare(s1.split.line.value, s2.split.line.value)
      if (r != 0 || s1.prev.depth == 0) r
      else compareSplitOrigin(s1.prev, s2.prev)
    }
  }

}
