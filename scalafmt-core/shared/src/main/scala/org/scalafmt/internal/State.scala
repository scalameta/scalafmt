package org.scalafmt.internal

import java.util.regex.Matcher
import java.util.regex.Pattern

import scala.annotation.tailrec
import scala.meta.tokens.Token

import org.scalafmt.config.Comments
import org.scalafmt.config.Docstrings
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.TokenOps
import org.scalafmt.util.TreeOps

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
    allAltAreNL: Boolean,
    delayedPenalty: Int, // apply if positive, ignore otherwise
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
      nextAllAltAreNL: Boolean
  )(implicit style: ScalafmtConfig, fops: FormatOps): State = {
    val tok = fops.tokens(depth)
    val right = tok.right

    val (nextSplit, nextIndent, nextIndents) =
      if (right.is[Token.EOF]) (initialNextSplit, 0, Seq.empty)
      else {
        val offset = column - indentation
        def getUnexpired(indents: Seq[ActualIndent]): Seq[ActualIndent] =
          indents.filter(_.notExpiredBy(tok))
        def getPushes(modExt: ModExt): Seq[ActualIndent] =
          getUnexpired(modExt.getActualIndents(offset))
        val initialModExt = initialNextSplit.modExt
        val indents = initialModExt.indents
        val nextPushes = getUnexpired(pushes) ++ getPushes(initialModExt)
        val nextIndent = Indent.getIndent(nextPushes)
        initialNextSplit.modExt.mod match {
          case m: NewlineT
              if !tok.left.is[Token.Comment] && m.alt.isDefined &&
                nextIndent >= m.alt.get.mod.length + column =>
            val alt = m.alt.get
            val altPushes = getPushes(alt)
            val altIndent = Indent.getIndent(altPushes)
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

    val overflow = columnOnCurrentLine - style.maxColumn
    val nextPolicy: PolicySummary =
      policy.combine(nextSplit.policy, fops.next(tok))

    val (penalty, nextDelayedPenalty) =
      if (
        overflow <= 0 || right.is[Token.Comment] && {
          val rtext = tok.meta.right.text
          nextSplit.isNL && rtext.length >= (style.maxColumn - nextIndent) ||
          fops.next(tok).hasBreak && {
            if (TokenOps.isDocstring(rtext))
              (style.docstrings.wrap ne Docstrings.Wrap.no) && nextSplit.isNL
            else
              (style.comments.wrap eq Comments.Wrap.trailing) ||
              (style.comments.wrap ne Comments.Wrap.no) && nextSplit.isNL
          }
        }
      ) {
        (math.max(0, delayedPenalty), 0) // fits inside column
      } else {
        val defaultOverflowPenalty =
          Constants.ExceedColumnPenalty + columnOnCurrentLine
        if (style.newlines.avoidForSimpleOverflow.isEmpty)
          (defaultOverflowPenalty, 0)
        else
          getOverflowPenalty(nextSplit, defaultOverflowPenalty)
      }
    val splitWithPenalty = nextSplit.withPenalty(penalty)

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
      nextAllAltAreNL,
      nextDelayedPenalty,
      nextFormatOff
    )
  }

  /** Returns a penalty to be applied to the split and any delayed penalty.
    * - if delayedPenalty is positive, it is considered activated and will be
    *   applied at the end of a line unless deactivated earlier;
    * - if delayedPenalty is negative, it is considered inactive but could be
    *   converted to regular penalty if a disqualifying token/split is found
    *   before the end of a line or document.
    */
  @tailrec
  private def getOverflowPenalty(
      nextSplit: Split,
      defaultOverflowPenalty: Int
  )(implicit style: ScalafmtConfig, fops: FormatOps): (Int, Int) = {
    val prevActive = delayedPenalty > 0
    val fullPenalty = defaultOverflowPenalty +
      (if (prevActive) delayedPenalty else -delayedPenalty)
    def result(customPenalty: Int, nextActive: Boolean): (Int, Int) = {
      val delay = fullPenalty - customPenalty
      val nextDelayedPenalty = // always preserve a little delayed penalty
        if (delay > 0) delay else if (delayedPenalty == 0) 0 else 1
      val penalty = fullPenalty - nextDelayedPenalty
      (penalty, if (nextActive) nextDelayedPenalty else -nextDelayedPenalty)
    }
    val ft = fops.tokens(depth)
    if (nextSplit.isNL || ft.right.is[Token.EOF]) {
      result(if (prevActive) fullPenalty else defaultOverflowPenalty, false)
    } else {
      val tokLength = ft.meta.right.text.length
      def getFullPenalty = result(fullPenalty, true)
      def getCustomPenalty = {
        val isComment = ft.right.is[Token.Comment]
        /* we only delay penalty for overflow tokens which are part of a
         * statement that started at the beginning of the current line */
        val startFtOpt =
          if (!State.allowSplitForLineStart(nextSplit, ft, isComment)) None
          else lineStartsStatement(isComment)
        val delay = startFtOpt.exists {
          case FormatToken(_, t: Token.Interpolation.Start, _) =>
            fops.matching(t) ne ft.right
          case _ => true
        }
        // if delaying, estimate column if the split had been a newline
        if (!delay) getFullPenalty
        else result(10 + indentation * 3 / 2 + tokLength, false)
      }
      if (ft.meta.right.firstNL >= 0) getFullPenalty
      else if (
        style.newlines.avoidForSimpleOverflowTooLong &&
        State.isWithinInterpolation(ft.meta.rightOwner)
      ) {
        ft.right match {
          case _: Token.Interpolation.End => getCustomPenalty
          case _: Token.Interpolation.Id if delayedPenalty != 0 =>
            getFullPenalty // can't delay multiple times
          case _ => // delay for intermediate interpolation tokens
            result(tokLength, true)
        }
      } else if (
        ft.right.isInstanceOf[Product] &&
        tokLength == 1 && !ft.meta.right.text.head.isLetterOrDigit
      ) { // delimiter
        val ok = delayedPenalty != 0 || {
          style.newlines.avoidForSimpleOverflowPunct &&
          column >= style.maxColumn
        }
        if (ok) result(1, prevActive)
        else prev.getOverflowPenalty(split, defaultOverflowPenalty + 1)
      } else if (
        style.newlines.avoidForSimpleOverflowTooLong &&
        delayedPenalty == 0 // can't delay multiple times
      ) {
        getCustomPenalty
      } else {
        getFullPenalty
      }
    }
  }

  /**
    * Traverses back to the beginning of the line and returns the largest tree
    * which starts with that token at the start of the line, if any.
    * @see [[State.allowSplitForLineStart]] which tokens can be traversed.
    */
  @tailrec
  private def getLineStartOwner(isComment: Boolean)(implicit
      style: ScalafmtConfig,
      fops: FormatOps
  ): Option[(FormatToken, meta.Tree)] = {
    val ft = fops.tokens(depth)
    if (ft.meta.left.firstNL >= 0) None
    else if (!split.isNL) {
      val ok = (prev ne State.start) &&
        State.allowSplitForLineStart(split, ft, isComment)
      if (ok) prev.getLineStartOwner(isComment) else None
    } else {
      def startsWithLeft(tree: meta.Tree): Boolean =
        tree.tokens.headOption.contains(ft.left)
      val ro = ft.meta.rightOwner
      val owner =
        if (startsWithLeft(ro)) Some(ro)
        else {
          val lo = ft.meta.leftOwner
          if (startsWithLeft(lo)) Some(lo) else None
        }
      owner.map { x =>
        val y = x.parent.flatMap { p =>
          if (!startsWithLeft(p)) None
          else TreeOps.findTreeWithParentSimple(p, false)(startsWithLeft)
        }
        (ft, y.getOrElse(x))
      }
    }
  }

  /**
    * Check that the current line starts a statement which also contains
    * the current token.
    */
  private def lineStartsStatement(
      isComment: Boolean
  )(implicit style: ScalafmtConfig, fops: FormatOps): Option[FormatToken] = {
    getLineStartOwner(isComment).flatMap {
      case (lineFt, lineOwner) =>
        val ft = fops.tokens(depth)
        val ok = {
          // comment could be preceded by a comma
          isComment && ft.left.is[Token.Comma] &&
          (fops.prev(ft).meta.leftOwner eq lineOwner)
        } ||
          TreeOps
            .findTreeOrParentSimple(ft.meta.leftOwner)(_ eq lineOwner)
            .isDefined
        if (ok) Some(lineFt) else None
    }
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
    false,
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

  /**
    * Checks whether a given token and split can be traversed while looking for
    * the beginning of the line.
    */
  private def allowSplitForLineStart(
      split: Split,
      ft: FormatToken,
      isComment: Boolean
  ): Boolean = {
    {
      split.length == 0 || isComment ||
      ft.meta.leftOwner.is[meta.Term.Assign]
    } && !split.modExt.indents.exists(_.hasStateColumn)
  }

  @inline
  private def isInterpolation(tree: meta.Tree): Boolean =
    tree.is[meta.Term.Interpolate]

  @inline
  private def isWithinInterpolation(tree: meta.Tree): Boolean =
    TreeOps.findTreeOrParentSimple(tree)(isInterpolation).isDefined

}
