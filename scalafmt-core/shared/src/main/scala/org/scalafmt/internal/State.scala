package org.scalafmt.internal

import org.scalafmt.config.Comments
import org.scalafmt.config.Indents
import org.scalafmt.config.Newlines
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.PolicyOps
import org.scalafmt.util.TreeOps._

import scala.meta._
import scala.meta.tokens.{Token => T}

import scala.annotation.tailrec

/** A partial formatting solution up to splits.length number of tokens.
  */
final class State(
    val cost: Int,
    val policy: PolicySummary,
    var split: Split,
    val depth: Int,
    val prev: State,
    var indentation: Int,
    pushes: Seq[ActualIndent],
    var column: Int,
    var pendingSpaces: List[Policy.End.WithPos],
    val appliedPenalty: Int, // penalty applied from overflow
    val delayedPenalty: Int, // apply if positive, ignore otherwise
    val lineId: Int,
) {

  override def toString = s"State($cost, $depth)"

  @inline
  def modExt: ModExt = split.modExt
  @inline
  def mod: Modification = modExt.mod

  def updatedWithSubsequentEOL(ft: FT): Unit = {
    column += pendingSpaces.count(_.notExpiredBy(ft))
    val mod = split.mod
    val updatedMod = mod match {
      case SpaceOrNoSplit(p) => if (p.notExpiredBy(ft)) Space else NoSplit
      case m => m
    }
    pendingSpaces = Nil
    split = split.withMod(updatedMod)
  }

  def possiblyBetter(other: State): Boolean = this.cost < other.cost ||
    this.indentation < other.indentation

  def costWithoutOverflowPenalty: Int = cost - appliedPenalty

  def hasSlbUntil(ft: FT): Boolean = policy
    .exists(_.appliesUntil(ft)(_.isInstanceOf[PolicyOps.SingleLineBlock]))

  def terminal(): Boolean = policy.exists(_.exists(_.terminal))

  /** Calculates next State given split at tok.
    */
  def next(
      initialNextSplit: Split,
  )(implicit style: ScalafmtConfig, tokens: FormatTokens): State = {
    val tok = tokens(depth)
    val right = tok.right

    val (nextSplit, nextIndent, nextIndents) =
      if (right.is[T.EOF]) (initialNextSplit, 0, Seq.empty)
      else {
        val offset = column - indentation
        def getUnexpired(modExt: ModExt, indents: Seq[ActualIndent]) = {
          val extendedEnd = getRelativeToLhsLastLineEnd(modExt.isNL)
          (modExt.getActualIndents(offset) ++ indents).flatMap(x =>
            if (x.notExpiredBy(tok)) Some(x)
            else extendedEnd
              .map(y => x.copy(expire = y, expiresAt = ExpiresOn.After)),
          )
        }

        val initialModExt = initialNextSplit.modExt
        val nextPushes = getUnexpired(initialModExt, pushes)
        val nextIndent = Indent.getIndent(nextPushes)
        initialModExt.altOpt.flatMap(alt =>
          if (tok.left.is[T.Comment]) None
          else if (nextIndent < alt.mod.length + column) None
          else if (initialModExt.noAltIndent) Some(alt)
          else Some(alt.withIndents(initialModExt.indents)),
        ).fold((initialNextSplit, nextIndent, nextPushes)) { alt =>
          val altPushes = getUnexpired(alt, pushes)
          val altIndent = Indent.getIndent(altPushes)
          val split = initialNextSplit.withMod(alt)
          (split, altIndent, altPushes)
        }
      }

    val (prevPendingSpaces, confirmedSpaces) =
      if (nextSplit.isNL) Nil -> pendingSpaces.count(_.notExpiredBy(tok))
      else pendingSpaces.filter(_.notExpiredBy(tok)) -> 0
    val (nextPendingSpaces, modLength) = nextSplit.mod match {
      case SpaceOrNoSplit(p) if p.notExpiredBy(tok) =>
        (p :: prevPendingSpaces, 0)
      case m => (prevPendingSpaces, m.length)
    }

    // Some tokens contain newline, like multiline strings/comments.
    val startColumn = nextSplit.mod match {
      case m: NewlineT => if (m.noIndent) 0 else nextIndent
      case m => if (m.isNL) nextIndent else column + modLength
    }
    val (pendingColumnOnCurrentLine, nextPendingColumn) = State
      .getColumns(tok, nextIndent, startColumn)
    val columnOnCurrentLine = pendingColumnOnCurrentLine + confirmedSpaces

    val nextTok = tokens.next(tok)
    val nextPolicy: PolicySummary = policy.combine(nextSplit, nextTok)

    def noOverflowPenalties = (math.max(0, delayedPenalty), 0) // fits inside column

    def overflowPenalties(column: Int) = {
      val defaultOverflowPenalty = Constants.ExceedColumnPenalty + column
      if (style.newlines.avoidForSimpleOverflow.isEmpty)
        (defaultOverflowPenalty, 0)
      else getOverflowPenalty(nextSplit, defaultOverflowPenalty)
    }

    val (penalty, nextDelayedPenalty) =
      if (columnOnCurrentLine <= style.maxColumn) noOverflowPenalties
      else if (right.is[T.Comment]) {
        def trailing = nextTok.hasBreak // newline after comment
        if (nextSplit.isNL) { // newline before comment
          val rtext = tok.meta.right.text
          if (rtext.length >= style.maxColumn - nextIndent || trailing)
            noOverflowPenalties
          else overflowPenalties(columnOnCurrentLine)
        } else if (style.comments.wrap.eq(Comments.Wrap.trailing) && trailing) {
          val minColumn = startColumn + 2 // 2 is for "/*"
          if (minColumn <= style.maxColumn) noOverflowPenalties
          else overflowPenalties(minColumn)
        } else overflowPenalties(columnOnCurrentLine)
      } else overflowPenalties(columnOnCurrentLine)

    val splitWithPenalty = nextSplit.withPenalty(penalty)

    new State(
      cost = cost + splitWithPenalty.costWithPenalty,
      // TODO(olafur) expire policy, see #18.
      policy = nextPolicy,
      split = splitWithPenalty,
      depth = depth + 1,
      prev = this,
      indentation = nextIndent,
      pushes = nextIndents,
      column = nextPendingColumn,
      pendingSpaces = nextPendingSpaces,
      appliedPenalty = appliedPenalty + penalty,
      delayedPenalty = nextDelayedPenalty,
      lineId = lineId + (if (nextSplit.isNL) 1 else 0),
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
      defaultOverflowPenalty: Int,
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
    if (nextSplit.isNL || ft.right.is[T.EOF])
      result(if (prevActive) fullPenalty else defaultOverflowPenalty, false)
    else {
      val tokLength = ft.meta.right.text.length
      def getFullPenalty = result(fullPenalty, true)
      def getCustomPenalty = {
        val isComment = ft.right.is[T.Comment]
        /* we only delay penalty for overflow tokens which are part of a
         * statement that started at the beginning of the current line */
        val startFtOpt =
          if (!State.allowSplitForLineStart(nextSplit, ft, isComment)) None
          else lineStartsStatement(isComment)
        val delay = startFtOpt.exists {
          case xft @ FT(_, _: T.Interpolation.Start, _) => tokens
              .matchingRight(xft).left ne ft.right
          case _ => true
        }
        // if delaying, estimate column if the split had been a newline
        if (!delay) getFullPenalty
        else result(10 + indentation * 3 / 2 + tokLength, false)
      }
      if (ft.meta.right.hasNL) getFullPenalty
      else if (
        (style.newlines.avoidForSimpleOverflowTooLong ||
          (style.newlines.inInterpolation eq Newlines.InInterpolation.avoid)) &&
        State.isWithinInterpolation(ft.meta.rightOwner)
      ) ft.right match {
        case _: T.Interpolation.End => getCustomPenalty
        case _: T.Interpolation.Id if delayedPenalty != 0 => getFullPenalty // can't delay multiple times
        case _ => // delay for intermediate interpolation tokens
          result(tokLength, true)
      }
      else if (ft.right.isInstanceOf[T.Punct] && tokLength == 1) { // delimiter
        val ok = delayedPenalty != 0 ||
          style.newlines.avoidForSimpleOverflowPunct &&
          column >= style.maxColumn
        if (ok) result(0, prevActive)
        else prev.getOverflowPenalty(split, defaultOverflowPenalty + 1)
      } else if (
        style.newlines.avoidForSimpleOverflowSLC && ft.right.is[T.Comment]
      ) result(0, prevActive)
      else if (
        style.newlines.avoidForSimpleOverflowTooLong && delayedPenalty == 0 // can't delay multiple times
      ) getCustomPenalty
      else getFullPenalty
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
      tokens: FormatTokens,
  ): Option[(FT, Tree)] = {
    val ft = tokens(depth)
    if (ft.meta.left.hasNL) None
    else if (!split.isNL) {
      val ok = (prev ne State.start) &&
        State.allowSplitForLineStart(split, ft, isComment)
      if (ok) prev.getLineStartOwner(isComment) else None
    } else {
      def startsWithLeft(tree: Tree): Boolean = tokens.getHeadOpt(tree)
        .contains(ft)
      def optionIfStartsWithLeft(tree: Tree): Option[Tree] = Some(tree)
        .filter(startsWithLeft)
      val owner = optionIfStartsWithLeft(ft.meta.rightOwner)
        .orElse(optionIfStartsWithLeft(ft.meta.leftOwner))
      owner.map { x =>
        val y = x.parent.flatMap(p =>
          if (!startsWithLeft(p)) None
          else findTreeWithParentSimple(p, false)(startsWithLeft),
        )
        (ft, y.getOrElse(x))
      }
    }
  }

  /** Check that the current line starts a statement which also contains the
    * current token.
    */
  private def lineStartsStatement(isComment: Boolean)(implicit
      style: ScalafmtConfig,
      tokens: FormatTokens,
  ): Option[FT] = getLineStartOwner(isComment)
    .flatMap { case (lineFt, lineOwner) =>
      val ft = tokens(depth)
      val ok =
        // comment could be preceded by a comma
        isComment && ft.left.is[T.Comma] &&
          (tokens.prev(ft).meta.leftOwner match {
            case `lineOwner` => true
            case t: Member.SyntaxValuesClause => t.parent.contains(lineOwner)
            case _ => false
          }) || findTreeOrParentSimple(ft.meta.leftOwner)(_ eq lineOwner)
            .isDefined
      if (ok) Some(lineFt) else None
    }

  private def getRelativeToLhsLastLineEnd(
      isNL: Boolean,
  )(implicit style: ScalafmtConfig, tokens: FormatTokens): Option[FT] = {
    val allowed = style.indent.relativeToLhsLastLine

    def treeEnd(x: Tree) = tokens.getLast(x)
    def indentEnd(ft: FT, isNL: Boolean)(onComment: => Option[FT]) = {
      val leftOwner = ft.meta.leftOwner
      ft.left match {
        case _: T.KwMatch
            if leftOwner.is[Term.Match] &&
              allowed.contains(Indents.RelativeToLhs.`match`) =>
          Some(treeEnd(leftOwner))
        case _: T.Ident if !isNL =>
          leftOwner.parent match {
            case Some(p: Term.ApplyInfix)
                if p.op.eq(leftOwner) &&
                  allowed.contains(Indents.RelativeToLhs.`infix`) =>
              Some(treeEnd(p))
            case _ => None
          }
        case _: T.Comment if !isNL => onComment
        case _ => None
      }
    }

    val tok = tokens(depth)
    val right = tok.right
    if (allowed.isEmpty) None
    else if (!isNL && right.is[T.Comment]) Some(tokens.next(tok))
    else indentEnd(tok, isNL) {
      val earlierState = prev.prevNonCommentSameLine
      indentEnd(tokens(earlierState.depth), earlierState.split.isNL)(None)
    }.orElse {
      val delay = !isNL &&
        (right match {
          case _: T.KwMatch => tok.meta.rightOwner.is[Term.Match] &&
            allowed.contains(Indents.RelativeToLhs.`match`)
          case _: T.Ident => tok.meta.rightOwner.parent.is[Term.ApplyInfix] &&
            allowed.contains(Indents.RelativeToLhs.`infix`)
          case _ => false
        })
      if (delay) Some(tokens.next(tok)) else None
    }
  }

  @tailrec
  private def prevNonCommentSameLine(implicit tokens: FormatTokens): State =
    if (split.isNL || !tokens(depth).left.is[T.Comment]) this
    else prev.prevNonCommentSameLine
}

object State {

  val start: State = new State(
    cost = 0,
    policy = PolicySummary.empty,
    split = null,
    depth = 0,
    prev = null,
    indentation = 0,
    pushes = Nil,
    column = 0,
    pendingSpaces = Nil,
    appliedPenalty = 0,
    delayedPenalty = 0,
    lineId = 0,
  )

  // this is not best state, it's higher priority for search
  object Ordering {
    // each comparison should compare priorities, i.e. define reverse ordering

    private val classicOrdering = new WithComparisons(compareCost, compareDepth)
    private val compactOrdering =
      new WithComparisons(compareCost, compareDepth, compareLineId)

    def get(style: ScalafmtConfig): Ordering[State] =
      if (style.newlines.classic) classicOrdering else compactOrdering

    class WithComparisons(comparisons: (State, State) => Int*)
        extends Ordering[State] {
      override def compare(x: State, y: State): Int = compareAt(x, y, 0)
      @tailrec
      private def compareAt(s1: State, s2: State, i: Int): Int = {
        val r = comparisons(i)(s1, s2)
        if (r != 0) r
        else {
          val ipp = i + 1
          if (ipp < comparisons.length) compareAt(s1, s2, ipp)
          else compareSplitOrigin(s1, s2)
        }
      }
    }

    // higher priority on lower cost
    private def compareCost(s1: State, s2: State): Int = Integer
      .compare(s2.cost, s1.cost)

    // higher priority on deeper state
    private def compareDepth(s1: State, s2: State): Int = Integer
      .compare(s1.depth, s2.depth)

    // higher priority on fewer lines
    private def compareLineId(s1: State, s2: State): Int = Integer
      .compare(s2.lineId, s1.lineId)

    // higher priority on later line defining the last split
    @tailrec
    private def compareSplitOrigin(s1: State, s2: State): Int = {
      // We assume the same number of splits, see compareSplitsLength
      // Break ties by the last split's line origin.
      val r = s1.split.fileLineStack.compare(s2.split.fileLineStack)
      if (r != 0 || s1.prev.depth == 0) r
      else compareSplitOrigin(s1.prev, s2.prev)
    }
  }

  @inline
  private def getStripMarginPatternWithLineContent(pipe: Char) =
    if (pipe == '|') RegexCompat.stripMarginPatternWithLineContent
    else RegexCompat.compileStripMarginPatternWithLineContent(pipe)

  def getColumns(tok: T, meta: FT.TokenMeta, column: Int)(
      stringMargin: Int => Int,
  )(interpPartMargin: Int => Int): (Int, Int) = {
    val syntax = meta.text
    val firstNL = meta.firstNL
    if (firstNL < 0) {
      val syntaxLen =
        if (column != 0 && tok.is[T.Comment]) {
          val asSlc = RegexCompat.slcLine.matcher(syntax)
          if (asSlc.matches()) 3 + asSlc.end(1) - asSlc.start(1)
          else syntax.length
        } else syntax.length
      val firstLineLength = column + syntaxLen
      (firstLineLength, firstLineLength)
    } else {
      val firstLength = column + firstNL
      tok match {
        case _: T.Constant.String =>
          val margin: Int => Int = stringMargin
          val pipe = getStripMarginChar(meta.owner)
          getColumnsWithStripMargin(pipe, syntax, firstNL, margin, firstLength)
        case _: T.Interpolation.Part =>
          val margin: Int => Int = interpPartMargin
          val pipe = getStripMarginCharForInterpolate(meta.owner)
          getColumnsWithStripMargin(pipe, syntax, firstNL, margin, firstLength)
        case _ =>
          val lastNewline = syntax.length - syntax.lastIndexOf('\n') - 1
          (firstLength, lastNewline)
      }
    }
  }

  def getColumns(ft: FT, indent: Int, column: Int)(implicit
      style: ScalafmtConfig,
  ): (Int, Int) = getColumns(ft.right, ft.meta.right, column)(
    if (style.assumeStandardLibraryStripMargin) {
      // 3 for '|' + 2 spaces
      val adjusted = 3 + (if (style.align.stripMargin) column else indent)
      _ => adjusted
    } else identity,
  )(if (style.assumeStandardLibraryStripMargin) {
    // 1 for '|'
    val adjusted = 1 + indent
    _ => adjusted
  } else identity)

  private def getColumnsFromMultiline(
      syntax: String,
      firstNL: Int,
      firstLength: Int,
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
      firstLength: Int,
  ): (Int, Int) = pipeOpt.fold(
    getColumnsFromMultiline(syntax, firstNL, firstLength),
  )(getColumnsWithStripMargin(_, syntax, firstNL, adjustMargin, firstLength))

  private def getColumnsWithStripMargin(
      pipe: Char,
      syntax: String,
      firstNL: Int,
      adjustMargin: Int => Int,
      firstLength: Int,
  ): (Int, Int) = {
    val matcher = getStripMarginPatternWithLineContent(pipe).matcher(syntax)
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
      ft: FT,
      isComment: Boolean,
  ): Boolean = {
    split.length == 0 || isComment || isInterpolation(ft.meta.rightOwner) ||
    ft.meta.leftOwner.is[meta.Term.Assign]
  } && !split.modExt.indents.exists(_.hasStateColumn)

  @inline
  private def isInterpolation(tree: Tree): Boolean = tree.is[Term.Interpolate]

  @inline
  private def isWithinInterpolation(tree: Tree): Boolean =
    findTreeOrParentSimple(tree)(isInterpolation).isDefined

}
