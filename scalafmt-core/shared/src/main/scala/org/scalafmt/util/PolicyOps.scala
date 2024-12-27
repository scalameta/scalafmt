package org.scalafmt.util

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal._

import org.scalameta.FileLine
import scala.meta.tokens.{Token => T}

object PolicyOps {

  /** @param noSyntaxNL
    *   do not allow newlines in token syntax
    */
  class PenalizeAllNewlines(
      penalty: Int,
      penalizeLambdas: Boolean = true,
      noSyntaxNL: Boolean = false,
      val rank: Int = 0,
  )(implicit fileLine: FileLine, style: ScalafmtConfig)
      extends Policy.Clause {
    override val noDequeue: Boolean = false
    override def terminal: Boolean = false
    private val checkSyntax = noSyntaxNL || !style.newlines.ignoreInSyntax
    override val f: Policy.Pf = {
      case Decision(ft, s) if penalizeLambdas || !ft.left.is[T.RightArrow] =>
        if (checkSyntax && ft.leftHasNewline) s.penalize(penalty)
        else s.penalizeNL(penalty)
    }
    override def prefix: String = s"PNL+$penalty"
  }

  object PenalizeAllNewlines {
    def apply(
        expire: FT,
        penalty: Int,
        penalizeLambdas: Boolean = true,
        noSyntaxNL: Boolean = false,
        ignore: Boolean = false,
        exclude: TokenRanges = TokenRanges.empty,
        excludeLt: FT => Policy.End.WithPos = Policy.End.OnLeft,
        excludeRt: FT => Policy.End.WithPos = Policy.End.OnLeft,
    )(implicit fileLine: FileLine, style: ScalafmtConfig): Policy = Policy ?
      (ignore || penalty <= 0) ||
      policyWithExclude(exclude, excludeLt, excludeRt)(
        new PenalizeAllNewlines(penalty, penalizeLambdas, noSyntaxNL) < expire,
      )
  }

  def penalizeOneNewline(on: FT, penalty: Int): Policy = Policy
    .onlyFor(on, s"PNL+$penalty")(_.penalizeNL(penalty))

  def penalizeNewlineByNesting(before: FT, after: FT): Policy = Policy.End <
    before ==> Policy.beforeLeft(after, prefix = "PNL()") {
      case Decision(FT(l, _, m), s) =>
        val nonBoolPenalty = if (TokenOps.isBoolOperator(l)) 0 else 5
        val penalty = TreeOps.nestedSelect(m.leftOwner) +
          TreeOps.nestedApplies(m.rightOwner) + nonBoolPenalty
        s.penalizeNL(penalty)
    }

  /** Forces all splits up to including expire to be on a single line.
    * @param okSLC
    *   if true, allow single-line comments
    * @param noSyntaxNL
    *   if false, allow newlines in token syntax
    */
  class SingleLineBlock(
      okSLC: Boolean = false,
      noSyntaxNL: Boolean = false,
      val rank: Int = 0,
  )(implicit fileLine: FileLine, style: ScalafmtConfig)
      extends Policy.Clause {
    import TokenOps.isLeftCommentThenBreak
    override val noDequeue: Boolean = true
    override def terminal: Boolean = true
    override val prefix: String = "SLB"
    private val checkSyntax = noSyntaxNL || !style.newlines.ignoreInSyntax
    override val f: Policy.Pf = {
      case Decision(ft, s)
          if !(ft.right.is[T.EOF] || okSLC && isLeftCommentThenBreak(ft)) =>
        if (checkSyntax && ft.leftHasNewline) Seq.empty else s.filterNot(_.isNL)
    }
  }

  object SingleLineBlock {

    def apply(
        expire: FT,
        exclude: TokenRanges = TokenRanges.empty,
        okSLC: Boolean = false,
        noSyntaxNL: Boolean = false,
        rank: Int = 0,
    )(implicit fileLine: FileLine, style: ScalafmtConfig): Policy =
      policyWithExclude(exclude, Policy.End.OnLeft, Policy.End.OnRight)(
        new SingleLineBlock(
          okSLC = okSLC,
          noSyntaxNL = noSyntaxNL,
          rank = rank,
        ) <= expire,
      )
  }

  final class DecideNewlinesOnlyBeforeToken private (
      val token: FT,
      split: Option[Split],
      val rank: Int,
      ifAny: Boolean,
  )(implicit fileLine: FileLine)
      extends Policy.Clause {
    override val noDequeue: Boolean = false
    override def terminal: Boolean = false
    override val prefix: String = "NB"
    override val f: Policy.Pf = split match {
      case Some(s) => {
        case d: Decision if d.formatToken.right eq token.left =>
          d.onlyNewlinesWithFallback(s)
      }
      case _ if ifAny => {
        case d: Decision if d.formatToken.right eq token.left =>
          d.onlyNewlinesIfAvailable
      }
      case _ => {
        case d: Decision if d.formatToken.right eq token.left =>
          d.onlyNewlinesWithoutFallback
      }
    }
  }

  object DecideNewlinesOnlyBeforeToken {
    def apply(
        token: FT,
        split: Option[Split],
        rank: Int = 0,
        ifAny: Boolean = false,
    )(implicit fileLine: FileLine): Policy = Policy.End < token ==>
      new DecideNewlinesOnlyBeforeToken(token, split, rank, ifAny) <= token
  }

  final class DecideNewlinesOnlyAfterToken private (
      val token: FT,
      split: Option[Split],
      val rank: Int,
      ifAny: Boolean,
  )(implicit fileLine: FileLine)
      extends Policy.Clause {
    override val noDequeue: Boolean = false
    override def terminal: Boolean = false
    override val prefix: String = "NA"
    override val f: Policy.Pf = split match {
      case Some(s) => {
        case d: Decision if d.formatToken eq token =>
          d.onlyNewlinesWithFallback(s)
      }
      case _ if ifAny => {
        case d: Decision if d.formatToken eq token => d.onlyNewlinesIfAvailable
      }
      case _ => {
        case d: Decision if d.formatToken eq token =>
          d.onlyNewlinesWithoutFallback
      }
    }
  }

  object DecideNewlinesOnlyAfterToken {
    def apply(
        token: FT,
        split: Option[Split],
        rank: Int = 0,
        ifAny: Boolean = false,
    )(implicit fileLine: FileLine): Policy = Policy.End <= token ==>
      new DecideNewlinesOnlyAfterToken(token, split, rank, ifAny) >= token
  }

  def policyWithExclude(
      exclude: TokenRanges,
      endLt: FT => Policy.End.WithPos,
      endRt: FT => Policy.End.WithPos,
  )(lastPolicy: Policy): Policy = exclude.ranges
    .foldLeft(lastPolicy) { case (policy, range) =>
      (lastPolicy <== endLt(range.lt)) ==> (endRt(range.rt) ==> policy)
    }

  def delayedBreakPolicy(
      end: => Policy.End.WithPos,
      exclude: TokenRanges = TokenRanges.empty,
  )(onBreakPolicy: Policy): Policy = Policy ? onBreakPolicy.isEmpty ||
    policyWithExclude(exclude, Policy.End.OnLeft, Policy.End.OnRight)(
      new Policy.Map(desc = onBreakPolicy.toString)({ s =>
        if (s.isNL) s.orPolicy(onBreakPolicy) else s
      }) <== end,
    )

  def delayedBreakPolicyBefore(token: FT)(onBreakPolicy: Policy): Policy =
    delayedBreakPolicy(Policy.End < token)(onBreakPolicy)

  def delayedBreakPolicyFor(token: FT)(f: FT => Policy): Policy =
    delayedBreakPolicyBefore(token)(f(token))

  def decideNewlinesOnlyBeforeClose(close: FT)(implicit
      fileLine: FileLine,
  ): Policy = decideNewlinesOnlyBeforeClose(0)(close)

  def decideNewlinesOnlyBeforeClose(rank: Int)(close: FT)(implicit
      fileLine: FileLine,
  ): Policy = decideNewlinesOnlyBeforeClose(Split(Newline, 0), rank)(close)

  def decideNewlinesOnlyBeforeClose(split: Split, rank: Int = 0)(close: FT)(
      implicit fileLine: FileLine,
  ): Policy = DecideNewlinesOnlyBeforeToken(close, Option(split), rank)

  def decideNewlinesOnlyBeforeCloseOnBreak(close: FT)(implicit
      fileLine: FileLine,
  ): Policy = decideNewlinesOnlyBeforeCloseOnBreak(0)(close)

  def decideNewlinesOnlyBeforeCloseOnBreak(rank: Int)(close: FT)(implicit
      fileLine: FileLine,
  ): Policy = delayedBreakPolicyFor(close)(decideNewlinesOnlyBeforeClose(rank))

  def decideNewlinesOnlyBeforeToken(token: FT)(implicit
      fileLine: FileLine,
  ): Policy = decideNewlinesOnlyBeforeToken(0)(token)

  def decideNewlinesOnlyBeforeToken(rank: Int, ifAny: Boolean = false)(
      token: FT,
  )(implicit fileLine: FileLine): Policy =
    DecideNewlinesOnlyBeforeToken(token, None, rank = rank, ifAny = ifAny)

  def decideNewlinesOnlyAfterClose(close: FT)(implicit
      fileLine: FileLine,
  ): Policy = decideNewlinesOnlyAfterClose(0)(close)

  def decideNewlinesOnlyAfterClose(rank: Int)(close: FT)(implicit
      fileLine: FileLine,
  ): Policy = decideNewlinesOnlyAfterClose(Split(Newline, 0), rank)(close)

  def decideNewlinesOnlyAfterClose(split: Split, rank: Int = 0)(close: FT)(
      implicit fileLine: FileLine,
  ): Policy = DecideNewlinesOnlyAfterToken(close, Option(split), rank)

  def decideNewlinesOnlyAfterToken(token: FT)(implicit
      fileLine: FileLine,
  ): Policy = decideNewlinesOnlyAfterToken(0)(token)

  def decideNewlinesOnlyAfterToken(rank: Int, ifAny: Boolean = false)(
      token: FT,
  )(implicit fileLine: FileLine): Policy =
    DecideNewlinesOnlyAfterToken(token, None, rank = rank, ifAny = ifAny)

  def unindentAtExclude(exclude: TokenRanges, indent: Length): Policy = exclude
    .ranges.foldLeft(Policy.noPolicy) { case (policy, range) =>
      val (lt, rt) = (range.lt, range.rt)
      val trigger = rt.left
      val unindent = Indent(indent, rt, ExpiresOn.After)
      val triggeredIndent = Indent.before(unindent, trigger)
      val triggerUnindent = Policy
        .onlyFor(lt, prefix = "UNIND{")(_.map(_.withIndent(triggeredIndent)))
      val cancelUnindent = delayedBreakPolicy(Policy.End <= lt)(
        Policy.onlyFor(lt, rank = 1, prefix = "UNIND}")( // use rank to apply after policy above
          _.map(_.switch(trigger, false)),
        ),
      )
      policy ==> triggerUnindent & cancelUnindent
    }

}
