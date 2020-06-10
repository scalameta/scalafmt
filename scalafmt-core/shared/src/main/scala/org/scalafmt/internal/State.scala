package org.scalafmt.internal

import java.util.regex.Matcher
import java.util.regex.Pattern

import scala.annotation.tailrec
import scala.meta.tokens.Token

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
    pushes: Seq[ActualIndent],
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
      initialNextSplit: Split,
      tok: FormatToken
  )(implicit style: ScalafmtConfig): State = {
    val right = tok.right
    val tokRightSyntax = tok.meta.right.text

    val (nextSplit, nextIndent, nextIndents) =
      if (tok.right.is[Token.EOF]) (initialNextSplit, 0, Seq.empty)
      else {
        val offset = column - indentation
        def getIndent(indents: Iterator[ActualIndent]): Int =
          indents.foldLeft(0)(_ + _.length)
        def getUnexpired(indents: Seq[ActualIndent]): Seq[ActualIndent] =
          indents.filter(_.notExpiredBy(tok))
        def getPushes(indents: Seq[Indent]): Seq[ActualIndent] =
          getUnexpired(indents.flatMap(_.withStateOffset(offset)))
        val indents = initialNextSplit.modExt.indents
        val nextPushes = getUnexpired(pushes) ++ getPushes(indents)
        val nextIndent = getIndent(nextPushes.iterator)
        initialNextSplit.modExt.mod match {
          case m: NewlineT
              if !tok.left.is[Token.Comment] && m.alt.isDefined &&
                nextIndent >= m.alt.get.mod.length + column =>
            val alt = m.alt.get
            val altPushes = getPushes(alt.indents)
            val altIndent = getIndent(altPushes.iterator)
            val split = initialNextSplit.withMod(alt.withIndents(indents))
            (split, nextIndent + altIndent, nextPushes ++ altPushes)
          case _ =>
            (initialNextSplit, nextIndent, nextPushes)
        }
      }

    // Some tokens contain newline, like multiline strings/comments.
    val (columnOnCurrentLine, nextStateColumn) = State.getColumns(
      tok,
      nextIndent,
      if (nextSplit.isNL) None else Some(column + nextSplit.length)
    )
    val nextPolicy: PolicySummary =
      policy.combine(nextSplit.policy, tok.left.end)
    val splitWithPenalty = {
      if (
        columnOnCurrentLine <= style.maxColumn || {
          val commentExceedsLineLength = right.is[Token.Comment] &&
            tokRightSyntax.length >= (style.maxColumn - nextIndent)
          commentExceedsLineLength && nextSplit.isNL
        }
      ) {
        nextSplit // fits inside column
      } else {
        nextSplit.withPenalty(
          Constants.ExceedColumnPenalty + columnOnCurrentLine
        ) // overflow
      }
    }

    val nextFormatOff =
      if (formatOff) !TokenOps.isFormatOn(right)
      else TokenOps.isFormatOff(right)

    State(
      cost + splitWithPenalty.cost,
      // TODO(olafur) expire policy, see #18.
      nextPolicy,
      splitWithPenalty,
      depth + 1,
      this,
      nextIndent,
      nextIndents,
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
    Seq.empty,
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

  private val stripMarginPattern =
    Pattern.compile("\n(\\h*+\\|)?([^\n]*+)")

  def getColumns(
      ft: FormatToken,
      indent: Int,
      noBreakColumn: Option[Int]
  )(implicit style: ScalafmtConfig): (Int, Int) = {
    val firstLineOffset = noBreakColumn.getOrElse(indent)
    val syntax = ft.meta.right.text
    val firstNewline = ft.meta.right.firstNL
    if (firstNewline == -1) {
      val firstLineLength = firstLineOffset + syntax.length
      (firstLineLength, firstLineLength)
    } else
      ft.right match {
        case _: Token.Constant.String =>
          getColumnsWithStripMargin(syntax, firstNewline, indent, noBreakColumn)
        case _ =>
          val lastNewline = syntax.length - syntax.lastIndexOf('\n') - 1
          (firstLineOffset + firstNewline, lastNewline)
      }
  }

  private def getColumnsWithStripMargin(
      syntax: String,
      firstNewline: Int,
      indent: Int,
      noBreakColumn: Option[Int]
  )(implicit style: ScalafmtConfig): (Int, Int) = {
    val column = noBreakColumn.getOrElse(indent)
    val matcher = stripMarginPattern.matcher(syntax)
    matcher.region(firstNewline, syntax.length)
    val firstLineLength = column + firstNewline
    if (!matcher.find()) (firstLineLength, firstLineLength)
    else {
      val matcherToLength = getMatcherToLength(column, indent, style)
      @tailrec
      def iter(prevMaxLength: Int): (Int, Int) = {
        val length = matcherToLength(matcher)
        val maxLength = math.max(prevMaxLength, length)
        if (matcher.find()) iter(maxLength) else (maxLength, length)
      }
      iter(firstLineLength)
    }
  }

  private def getMatcherToLength(
      column: Int,
      indent: Int,
      style: ScalafmtConfig
  ): Matcher => Int = {
    val adjustMargin: Int => Int =
      if (!style.assumeStandardLibraryStripMargin) identity[Int]
      else {
        // 3 for '|' + 2 spaces
        val adjusted = 3 + (if (style.align.stripMargin) column else indent)
        _ => adjusted
      }
    (matcher: Matcher) => {
      val margin = matcher.end(1) - matcher.start(1)
      val textLength = matcher.end(2) - matcher.start(2)
      // if 0, has newline but no pipe
      if (0 == margin) textLength else textLength + adjustMargin(margin)
    }
  }

}
