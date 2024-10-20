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
      val endPolicy: Policy.End.WithPos,
      penalty: Int,
      penalizeLambdas: Boolean = true,
      noSyntaxNL: Boolean = false,
      val rank: Int = 0,
  )(implicit fileLine: FileLine, style: ScalafmtConfig)
      extends Policy.Clause {
    override val noDequeue: Boolean = false
    private val checkSyntax = noSyntaxNL || !style.newlines.ignoreInSyntax
    override val f: Policy.Pf = {
      case Decision(ft, s) if penalizeLambdas || !ft.left.is[T.RightArrow] =>
        if (checkSyntax && ft.leftHasNewline) s.map(_.withPenalty(penalty))
        else s.map(x => if (x.isNL) x.withPenalty(penalty) else x)
    }
    override def prefix: String = s"PNL+$penalty"
  }

  object PenalizeAllNewlines {
    def apply(
        expire: T,
        penalty: Int,
        penalizeLambdas: Boolean = true,
        noSyntaxNL: Boolean = false,
    )(implicit fileLine: FileLine, style: ScalafmtConfig): Policy =
      new PenalizeAllNewlines(
        Policy.End < expire,
        penalty,
        penalizeLambdas,
        noSyntaxNL,
      )
  }

  def penalizeNewlineByNesting(from: T, to: T)(implicit
      fileLine: FileLine,
  ): Policy = Policy.End < from ==> Policy.before(to, prefix = "PNL()") {
    case Decision(FormatToken(l, _, m), s) =>
      val nonBoolPenalty = if (TokenOps.isBoolOperator(l)) 0 else 5
      val penalty = TreeOps.nestedSelect(m.leftOwner) +
        TreeOps.nestedApplies(m.rightOwner) + nonBoolPenalty
      s.map(x => if (x.isNL) x.withPenalty(penalty) else x)
  }

  /** Forces all splits up to including expire to be on a single line.
    * @param okSLC
    *   if true, allow single-line comments
    * @param noSyntaxNL
    *   if false, allow newlines in token syntax
    */
  class SingleLineBlock(
      val endPolicy: Policy.End.WithPos,
      okSLC: Boolean = false,
      noSyntaxNL: Boolean = false,
      val rank: Int = 0,
  )(implicit fileLine: FileLine, style: ScalafmtConfig)
      extends Policy.Clause {
    import TokenOps.isLeftCommentThenBreak
    override val noDequeue: Boolean = true
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
        expire: T,
        exclude: TokenRanges = TokenRanges.empty,
        okSLC: Boolean = false,
        noSyntaxNL: Boolean = false,
        rank: Int = 0,
    )(implicit fileLine: FileLine, style: ScalafmtConfig): Policy =
      policyWithExclude(exclude, Policy.End.On, Policy.End.After)(
        new SingleLineBlock(
          Policy.End == expire,
          okSLC = okSLC,
          noSyntaxNL = noSyntaxNL,
          rank = rank,
        ),
      )
  }

  final class DecideNewlinesOnlyBeforeToken(
      val token: T,
      split: Option[Split],
      val rank: Int = 0,
      ifAny: Boolean = false,
  )(implicit fileLine: FileLine)
      extends Policy.Clause {
    override val endPolicy: Policy.End.WithPos = Policy.End == token
    override val noDequeue: Boolean = false
    override val prefix: String = "NB"
    override val f: Policy.Pf = split match {
      case Some(s) => {
        case d: Decision if d.formatToken.right eq token =>
          d.onlyNewlinesWithFallback(s)
      }
      case _ if ifAny => {
        case d: Decision if d.formatToken.right eq token =>
          d.onlyNewlinesIfAvailable
      }
      case _ => {
        case d: Decision if d.formatToken.right eq token =>
          d.onlyNewlinesWithoutFallback
      }
    }
  }

  final class DecideNewlinesOnlyAfterToken(
      val token: T,
      split: Option[Split],
      val rank: Int = 0,
      ifAny: Boolean = false,
  )(implicit fileLine: FileLine)
      extends Policy.Clause {
    override val endPolicy: Policy.End.WithPos = Policy.End > token
    override val noDequeue: Boolean = false
    override val prefix: String = "NA"
    override val f: Policy.Pf = split match {
      case Some(s) => {
        case d: Decision if d.formatToken.left eq token =>
          d.onlyNewlinesWithFallback(s)
      }
      case _ if ifAny => {
        case d: Decision if d.formatToken.left eq token =>
          d.onlyNewlinesIfAvailable
      }
      case _ => {
        case d: Decision if d.formatToken.left eq token =>
          d.onlyNewlinesWithoutFallback
      }
    }
  }

  def policyWithExclude(
      exclude: TokenRanges,
      endLt: T => Policy.End.WithPos,
      endRt: T => Policy.End.WithPos,
  )(lastPolicy: Policy)(implicit fileLine: FileLine): Policy = exclude.ranges
    .foldLeft(lastPolicy) { case (policy, range) =>
      (lastPolicy <== endLt(range.lt.left)) ==> (endRt(range.rt.left) ==> policy)
    }

  def delayedBreakPolicy(
      end: => Policy.End.WithPos,
      exclude: TokenRanges = TokenRanges.empty,
  )(onBreakPolicy: Policy)(implicit fileLine: FileLine): Policy =
    Policy ? onBreakPolicy.isEmpty ||
      policyWithExclude(exclude, Policy.End.On, Policy.End.After)(
        new Policy.Map(end, desc = onBreakPolicy.toString)({ s =>
          if (s.isNL) s.orPolicy(onBreakPolicy) else s
        }),
      )

  def delayedBreakPolicyBefore(token: T)(onBreakPolicy: Policy): Policy =
    delayedBreakPolicy(Policy.End < token)(onBreakPolicy)

  def delayedBreakPolicyFor(token: T)(f: T => Policy): Policy =
    delayedBreakPolicyBefore(token)(f(token))

  def decideNewlinesOnlyBeforeClose(close: T)(implicit
      fileLine: FileLine,
  ): Policy = decideNewlinesOnlyBeforeClose(0)(close)

  def decideNewlinesOnlyBeforeClose(rank: Int)(close: T)(implicit
      fileLine: FileLine,
  ): Policy = decideNewlinesOnlyBeforeClose(Split(Newline, 0), rank)(close)

  def decideNewlinesOnlyBeforeClose(split: Split, rank: Int = 0)(close: T)(
      implicit fileLine: FileLine,
  ): Policy = new DecideNewlinesOnlyBeforeToken(close, Option(split), rank)

  def decideNewlinesOnlyBeforeCloseOnBreak(close: T)(implicit
      fileLine: FileLine,
  ): Policy = decideNewlinesOnlyBeforeCloseOnBreak(0)(close)

  def decideNewlinesOnlyBeforeCloseOnBreak(rank: Int)(close: T)(implicit
      fileLine: FileLine,
  ): Policy = delayedBreakPolicyFor(close)(decideNewlinesOnlyBeforeClose(rank))

  def decideNewlinesOnlyBeforeToken(token: T)(implicit
      fileLine: FileLine,
  ): Policy = decideNewlinesOnlyBeforeToken(0)(token)

  def decideNewlinesOnlyBeforeToken(rank: Int, ifAny: Boolean = false)(
      token: T,
  )(implicit fileLine: FileLine): Policy =
    new DecideNewlinesOnlyBeforeToken(token, None, rank = rank, ifAny = ifAny)

  def decideNewlinesOnlyAfterClose(close: T)(implicit
      fileLine: FileLine,
  ): Policy = decideNewlinesOnlyAfterClose(0)(close)

  def decideNewlinesOnlyAfterClose(rank: Int)(close: T)(implicit
      fileLine: FileLine,
  ): Policy = decideNewlinesOnlyAfterClose(Split(Newline, 0), rank)(close)

  def decideNewlinesOnlyAfterClose(split: Split, rank: Int = 0)(close: T)(
      implicit fileLine: FileLine,
  ): Policy = new DecideNewlinesOnlyAfterToken(close, Option(split), rank)

  def decideNewlinesOnlyAfterToken(token: T)(implicit
      fileLine: FileLine,
  ): Policy = decideNewlinesOnlyAfterToken(0)(token)

  def decideNewlinesOnlyAfterToken(rank: Int, ifAny: Boolean = false)(
      token: T,
  )(implicit fileLine: FileLine): Policy =
    new DecideNewlinesOnlyAfterToken(token, None, rank = rank, ifAny = ifAny)

  def unindentAtExclude(exclude: TokenRanges, indent: Length): Policy = exclude
    .ranges.foldLeft(Policy.noPolicy) { case (policy, range) =>
      val (lt, rt) = (range.lt, range.rt)
      val ltl = lt.left
      val rtl = rt.left
      val trigger = rtl
      val unindent = Indent(indent, rtl, ExpiresOn.After)
      val triggeredIndent = Indent.before(unindent, trigger)
      val triggerUnindent = Policy
        .on(rtl, prefix = "UNIND{") { case Decision(`lt`, s) =>
          s.map(_.withIndent(triggeredIndent))
        }
      val cancelUnindent = delayedBreakPolicy(Policy.End == ltl) {
        Policy.after(ltl, rank = 1, prefix = "UNIND}") { // use rank to apply after policy above
          case Decision(`lt`, s) => s.map(_.switch(trigger, false))
        }
      }
      policy ==> triggerUnindent & cancelUnindent
    }

}
