package org.scalafmt.internal

import java.util.regex.Pattern

import scala.annotation.tailrec
import scala.meta.tokens.Token
import scala.meta._

import org.scalafmt.config.{Comments, Indents, ScalafmtConfig}
import org.scalafmt.util.TreeOps._

/** A partial formatting solution up to splits.length number of tokens.
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
    appliedPenalty: Int, // penalty applied from overflow
    delayedPenalty: Int // apply if positive, ignore otherwise
) {

  override def toString = s"State($cost, $depth)"

  def alwaysBetter(other: State): Boolean =
    this.cost <= other.cost && this.indentation <= other.indentation

  /** Calculates next State given split at tok.
    */
  def next(
      initialNextSplit: Split,
      nextAllAltAreNL: Boolean
  )(implicit style: ScalafmtConfig, tokens: FormatTokens): State = {
    val tok = tokens(depth)
    val right = tok.right

    val (nextSplit, nextIndent, nextIndents) =
      if (right.is[Token.EOF]) (initialNextSplit, 0, Seq.empty)
      else {
        val offset = column - indentation
        def getUnexpired(modExt: ModExt, indents: Seq[ActualIndent] = Nil) = {
          val extendedEnd = getRelativeToLhsLastLineEnd(modExt.isNL)
          (modExt.getActualIndents(offset) ++ indents).flatMap { x =>
            if (x.notExpiredBy(tok)) Some(x)
            else
              extendedEnd
                .map(y => x.copy(expireEnd = y, expiresAt = ExpiresOn.After))
          }
        }

        val initialModExt = initialNextSplit.modExt
        val indents = initialModExt.indents
        val nextPushes = getUnexpired(initialModExt, pushes)
        val nextIndent = Indent.getIndent(nextPushes)
        initialNextSplit.modExt.mod match {
          case m: NewlineT
              if !tok.left.is[Token.Comment] && m.alt.isDefined &&
                nextIndent >= m.alt.get.mod.length + column =>
            val alt = m.alt.get
            val altPushes = getUnexpired(alt)
            val altIndent = Indent.getIndent(altPushes)
            val split = initialNextSplit.withMod(alt.withIndents(indents))
            (split, nextIndent + altIndent, nextPushes ++ altPushes)
          case _ =>
            (initialNextSplit, nextIndent, nextPushes)
        }
      }

    // Some tokens contain newline, like multiline strings/comments.
    val startColumn = nextSplit.modExt.mod match {
      case m: NewlineT => if (m.noIndent) 0 else nextIndent
      case m => if (m.isNewline) nextIndent else column + m.length
    }
    val (columnOnCurrentLine, nextStateColumn) =
      State.getColumns(tok, nextIndent, startColumn)

    val nextTok = tokens.next(tok)
    val nextPolicy: PolicySummary = policy.combine(nextSplit.policy, nextTok)

    def noOverflowPenalties =
      (math.max(0, delayedPenalty), 0) // fits inside column

    def overflowPenalties(column: Int) = {
      val defaultOverflowPenalty = Constants.ExceedColumnPenalty + column
      if (style.newlines.avoidForSimpleOverflow.isEmpty)
        (defaultOverflowPenalty, 0)
      else
        getOverflowPenalty(nextSplit, defaultOverflowPenalty)
    }

    val (penalty, nextDelayedPenalty) =
      if (columnOnCurrentLine <= style.maxColumn) noOverflowPenalties
      else if (right.is[Token.Comment]) {
        def trailing = nextTok.hasBreak // newline after comment
        if (nextSplit.isNL) { // newline before comment
          val rtext = tok.meta.right.text
          if (rtext.length >= (style.maxColumn - nextIndent) || trailing)
            noOverflowPenalties
          else overflowPenalties(columnOnCurrentLine)
        } else if (style.comments.wrap.eq(Comments.Wrap.trailing) && trailing) {
          val minColumn = startColumn + 2 // 2 is for "/*"
          if (minColumn <= style.maxColumn) noOverflowPenalties
          else overflowPenalties(minColumn)
        } else overflowPenalties(columnOnCurrentLine)
      } else overflowPenalties(columnOnCurrentLine)

    val splitWithPenalty = nextSplit.withPenalty(penalty)

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
      appliedPenalty + penalty,
      nextDelayedPenalty
    )
  }

  /** Returns a penalty to be applied to the split and any delayed penalty.
    *   - if delayedPenalty is positive, it is considered activated and will be
    *     applied at the end of a line unless deactivated earlier;
    *   - if delayedPenalty is negative, it is considered inactive but could be
    *     converted to regular penalty if a disqualifying token/split is found
    *     before the end of a line or document.
    */
  @tailrec
  private def getOverflowPenalty(
      nextSplit: Split,
      defaultOverflowPenalty: Int
  )(implicit style: ScalafmtConfig, tokens: FormatTokens): (Int, Int) = {
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
    val ft = tokens(depth)
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
            tokens.matching(t) ne ft.right
          case _ => true
        }
        // if delaying, estimate column if the split had been a newline
        if (!delay) getFullPenalty
        else result(10 + indentation * 3 / 2 + tokLength, false)
      }
      if (ft.meta.right.hasNL) getFullPenalty
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
        style.newlines.avoidForSimpleOverflowSLC &&
        tokens.isRightCommentThenBreak(ft)
      ) {
        result(0, prevActive)
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

  /** Traverses back to the beginning of the line and returns the largest tree
    * which starts with that token at the start of the line, if any.
    * @see
    *   [[State.allowSplitForLineStart]] which tokens can be traversed.
    */
  @tailrec
  private def getLineStartOwner(isComment: Boolean)(implicit
      style: ScalafmtConfig,
      tokens: FormatTokens
  ): Option[(FormatToken, Tree)] = {
    val ft = tokens(depth)
    if (ft.meta.left.hasNL) None
    else if (!split.isNL) {
      val ok = (prev ne State.start) &&
        State.allowSplitForLineStart(split, ft, isComment)
      if (ok) prev.getLineStartOwner(isComment) else None
    } else {
      def startsWithLeft(tree: Tree): Boolean =
        tokens.getHeadOpt(tree).contains(ft)
      def optionIfStartsWithLeft(tree: Tree): Option[Tree] =
        Some(tree).filter(startsWithLeft)
      val owner = optionIfStartsWithLeft(ft.meta.rightOwner)
        .orElse(optionIfStartsWithLeft(ft.meta.leftOwner))
      owner.map { x =>
        val y = x.parent.flatMap { p =>
          if (!startsWithLeft(p)) None
          else findTreeWithParentSimple(p, false)(startsWithLeft)
        }
        (ft, y.getOrElse(x))
      }
    }
  }

  /** Check that the current line starts a statement which also contains the
    * current token.
    */
  private def lineStartsStatement(isComment: Boolean)(implicit
      style: ScalafmtConfig,
      tokens: FormatTokens
  ): Option[FormatToken] = {
    getLineStartOwner(isComment).flatMap { case (lineFt, lineOwner) =>
      val ft = tokens(depth)
      val ok = {
        // comment could be preceded by a comma
        isComment && ft.left.is[Token.Comma] &&
        (tokens.prev(ft).meta.leftOwner match {
          case `lineOwner` => true
          case t: Member.SyntaxValuesClause => t.parent.contains(lineOwner)
          case _ => false
        })
      } ||
        findTreeOrParentSimple(ft.meta.leftOwner)(_ eq lineOwner).isDefined
      if (ok) Some(lineFt) else None
    }
  }

  private def getRelativeToLhsLastLineEnd(isNL: Boolean)(implicit
      style: ScalafmtConfig,
      tokens: FormatTokens
  ): Option[Int] = {
    val allowed = style.indent.relativeToLhsLastLine

    def treeEnd(x: Tree) = tokens.getLast(x).left.end
    def indentEnd(ft: FormatToken, isNL: Boolean)(onComment: => Option[Int]) = {
      val leftOwner = ft.meta.leftOwner
      ft.left match {
        case _: Token.KwMatch
            if leftOwner.is[Term.Match] &&
              allowed.contains(Indents.RelativeToLhs.`match`) =>
          Some(treeEnd(leftOwner))
        case _: Token.Ident if !isNL =>
          leftOwner.parent match {
            case Some(p: Term.ApplyInfix)
                if p.op.eq(leftOwner) &&
                  allowed.contains(Indents.RelativeToLhs.`infix`) =>
              Some(treeEnd(p))
            case _ => None
          }
        case _: Token.Comment if !isNL => onComment
        case _ => None
      }
    }

    val tok = tokens(depth)
    val right = tok.right
    if (allowed.isEmpty) None
    else if (!isNL && right.is[Token.Comment]) Some(right.end)
    else
      indentEnd(tok, isNL) {
        val earlierState = prev.prevNonCommentSameLine
        indentEnd(tokens(earlierState.depth), earlierState.split.isNL)(None)
      }.orElse {
        val delay = !isNL && (right match {
          case _: Token.KwMatch =>
            tok.meta.rightOwner.is[Term.Match] &&
            allowed.contains(Indents.RelativeToLhs.`match`)
          case _: Token.Ident =>
            tok.meta.rightOwner.parent.exists(_.is[Term.ApplyInfix]) &&
            allowed.contains(Indents.RelativeToLhs.`infix`)
          case _ => false
        })
        if (delay) Some(right.end) else None
      }
  }

  @tailrec
  private def prevNonCommentSameLine(implicit tokens: FormatTokens): State =
    if (split.isNL || !tokens(depth).left.is[Token.Comment]) this
    else prev.prevNonCommentSameLine
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
    0
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
      val r = Integer.compare(
        s1.split.fileLine.line.value,
        s2.split.fileLine.line.value
      )
      if (r != 0 || s1.prev.depth == 0) r
      else compareSplitOrigin(s1.prev, s2.prev)
    }
  }

  @inline
  private def compileStripMarginPattern(pipe: Char) =
    Pattern.compile(s"\n(\\h*+\\$pipe)?([^\n]*+)")

  @inline
  private def getStripMarginPattern(pipe: Char) =
    if (pipe == '|') pipeStripMarginPattern else compileStripMarginPattern(pipe)

  private val pipeStripMarginPattern = compileStripMarginPattern('|')

  private val slcLine = Pattern.compile("^/\\/\\/*+\\h*+(.*?)\\h*+$")

  def getColumns(
      ft: FormatToken,
      indent: Int,
      column: Int
  )(implicit style: ScalafmtConfig): (Int, Int) = {
    val syntax = ft.meta.right.text
    val firstNL = ft.meta.right.firstNL
    if (firstNL < 0) {
      val syntaxLen =
        if (column != 0 && ft.right.is[Token.Comment]) {
          val asSlc = State.slcLine.matcher(syntax)
          if (asSlc.matches()) 3 + asSlc.end(1) - asSlc.start(1)
          else syntax.length
        } else syntax.length
      val firstLineLength = column + syntaxLen
      (firstLineLength, firstLineLength)
    } else {
      val firstLength = column + firstNL
      ft.right match {
        case _: Token.Constant.String =>
          val margin: Int => Int = if (style.assumeStandardLibraryStripMargin) {
            // 3 for '|' + 2 spaces
            val adjusted = 3 + (if (style.align.stripMargin) column else indent)
            _ => adjusted
          } else identity
          val pipe = getStripMarginChar(ft.meta.rightOwner)
          getColumnsWithStripMargin(pipe, syntax, firstNL, margin, firstLength)
        case _: Token.Interpolation.Part =>
          val margin: Int => Int = if (style.assumeStandardLibraryStripMargin) {
            // 1 for '|'
            val adjusted = 1 + indent
            _ => adjusted
          } else identity
          val pipe = getStripMarginCharForInterpolate(ft.meta.rightOwner)
          getColumnsWithStripMargin(pipe, syntax, firstNL, margin, firstLength)
        case _ =>
          val lastNewline = syntax.length - syntax.lastIndexOf('\n') - 1
          (firstLength, lastNewline)
      }
    }
  }

  private def getColumnsFromMultiline(
      syntax: String,
      firstNL: Int,
      firstLength: Int
  ): (Int, Int) = {
    @tailrec
    def iter(prevMaxLength: Int, lineBeg: Int): (Int, Int) = {
      val nextNL = syntax.indexOf('\n', lineBeg)
      val length = (if (nextNL < 0) syntax.length else nextNL) - lineBeg
      val maxLength = math.max(prevMaxLength, length)
      if (nextNL < 0) (maxLength, length) else iter(maxLength, nextNL + 1)
    }
    iter(firstLength, firstNL + 1)
  }

  private def getColumnsWithStripMargin(
      pipeOpt: Option[Char],
      syntax: String,
      firstNL: Int,
      adjustMargin: Int => Int,
      firstLength: Int
  ): (Int, Int) =
    pipeOpt.fold(getColumnsFromMultiline(syntax, firstNL, firstLength))(
      getColumnsWithStripMargin(_, syntax, firstNL, adjustMargin, firstLength)
    )

  private def getColumnsWithStripMargin(
      pipe: Char,
      syntax: String,
      firstNL: Int,
      adjustMargin: Int => Int,
      firstLength: Int
  ): (Int, Int) = {
    val matcher = getStripMarginPattern(pipe).matcher(syntax)
    matcher.region(firstNL, syntax.length)
    if (!matcher.find()) (firstLength, firstLength)
    else {
      def getMatcherLength = {
        val margin = matcher.end(1) - matcher.start(1)
        val textLength = matcher.end(2) - matcher.start(2)
        // if 0, has newline but no pipe
        if (0 == margin) textLength else textLength + adjustMargin(margin)
      }
      @tailrec
      def iter(prevMaxLength: Int): (Int, Int) = {
        val length = getMatcherLength
        val maxLength = math.max(prevMaxLength, length)
        if (matcher.find()) iter(maxLength) else (maxLength, length)
      }
      iter(firstLength)
    }
  }

  /** Checks whether a given token and split can be traversed while looking for
    * the beginning of the line.
    */
  private def allowSplitForLineStart(
      split: Split,
      ft: FormatToken,
      isComment: Boolean
  ): Boolean = {
    {
      split.length == 0 || isComment ||
      isInterpolation(ft.meta.rightOwner) ||
      ft.meta.leftOwner.is[meta.Term.Assign]
    } && !split.modExt.indents.exists(_.hasStateColumn)
  }

  @inline
  private def isInterpolation(tree: Tree): Boolean =
    tree.is[Term.Interpolate]

  @inline
  private def isWithinInterpolation(tree: Tree): Boolean =
    findTreeOrParentSimple(tree)(isInterpolation).isDefined

}
