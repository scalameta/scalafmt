package org.scalafmt.internal

import scala.meta.tokens.Token
import scala.meta.tokens.Token.Comment

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.ExpiresOn.Left
import org.scalafmt.internal.Length.Num
import org.scalafmt.util.TokenOps

/**
  * A partial formatting solution up to splits.length number of tokens.
  */
final case class State(cost: Int,
                       policy: PolicySummary,
                       splits: Vector[Split],
                       indentation: Int,
                       pushes: Vector[Indent[Num]],
                       column: Int)
    extends Ordered[State] {

  def compare(that: State): Int = {
    val costCompare = Integer.valueOf(-this.cost).compareTo(-that.cost)
    if (costCompare != 0) costCompare
    else {
      val splitsCompare =
        Integer.valueOf(this.splits.length).compareTo(that.splits.length)
      if (splitsCompare != 0) splitsCompare
      else {
        // Break ties by the split line origin.
        var i = this.splits.length - 1
        var r = 0
        while (i > 0 && r == 0) {
          r = Integer
            .valueOf(this.splits(i).line.value)
            .compareTo(that.splits(i).line.value)
          i -= 1
        }
        r
      }
    }
  }

  override def toString = s"State($cost, ${splits.length})"

  def alwaysBetter(other: State): Boolean =
    this.cost <= other.cost && this.indentation <= other.indentation
}

object State {
  val start = State(0,
                    PolicySummary.empty,
                    Vector.empty[Split],
                    0,
                    Vector.empty[Indent[Num]],
                    0)

  /**
    * Calculates next State given split at tok.
    */
  def next(curr: State,
           style: ScalafmtConfig,
           split: Split,
           tok: FormatToken): State = {
    import curr._
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
    val tokLength = tok.right.syntax.length

    // Some tokens contain newline, like multiline strings/comments.
    val lengthOnFirstLine = TokenOps.tokenLength(tok.right)
    val columnOnCurrentLine =
      lengthOnFirstLine + {
        if (split.modification.isNewline) newIndent
        else column + split.length
      }
    val lengthOnLastLine = {
      val lastNewline = tok.right.syntax.lastIndexOf('\n')
      if (lastNewline == -1) tokLength
      else tokLength - lastNewline - 1
    }
    val nextStateColumn =
      lengthOnLastLine + {
        // Tokens with newlines get no indentation.
        if (tok.right.syntax.contains('\n')) 0
        else if (split.modification.isNewline) newIndent
        else column + split.length
      }
    val newPolicy: PolicySummary = policy.combine(split.policy, tok.left.end)
    val splitWithPenalty = {
      if (columnOnCurrentLine < style.maxColumn || {
            val commentExceedsLineLength =
              tok.right.is[Comment] &&
                tok.right.syntax.length >= (style.maxColumn - newIndent)
            commentExceedsLineLength && split.modification.isNewline
          }) {
        split // fits inside column
      } else {
        split.withPenalty(Constants.ExceedColumnPenalty + columnOnCurrentLine) // overflow
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
