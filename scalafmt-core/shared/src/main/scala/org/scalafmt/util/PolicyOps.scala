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
    override def toString: String = s"PNL:${super.toString}+$penalty"
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
  ): Policy = Policy.End < from ==>
    Policy.before(to) { case Decision(FormatToken(l, _, m), s) =>
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
    override def toString: String = "SLB:" + super.toString
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
  )(implicit fileLine: FileLine)
      extends Policy.Clause {
    override val endPolicy: Policy.End.WithPos = Policy.End == token
    override val noDequeue: Boolean = false
    override val f: Policy.Pf = split.fold[Policy.Pf] {
      case d: Decision if d.formatToken.right eq token =>
        d.onlyNewlinesWithoutFallback
    } { s =>
      {
        case d: Decision if d.formatToken.right eq token =>
          d.onlyNewlinesWithFallback(s)
      }
    }
    override def toString: String = "NB:" + super.toString
  }

  final class DecideNewlinesOnlyAfterToken(
      val token: T,
      split: Option[Split],
      val rank: Int = 0,
  )(implicit fileLine: FileLine)
      extends Policy.Clause {
    override val endPolicy: Policy.End.WithPos = Policy.End > token
    override val noDequeue: Boolean = false
    override val f: Policy.Pf = split.fold[Policy.Pf] {
      case d: Decision if d.formatToken.left eq token =>
        d.onlyNewlinesWithoutFallback
    } { s =>
      {
        case d: Decision if d.formatToken.left eq token =>
          d.onlyNewlinesWithFallback(s)
      }
    }
    override def toString: String = "NA:" + super.toString
  }

  def policyWithExclude(
      exclude: TokenRanges,
      endLt: T => Policy.End.WithPos,
      endRt: T => Policy.End.WithPos,
  )(lastPolicy: Policy)(implicit fileLine: FileLine): Policy = exclude.ranges
    .foldLeft(lastPolicy) { case (policy, range) =>
      (lastPolicy <== endLt(range.lt)) ==> (endRt(range.rt) ==> policy)
    }

  def delayedBreakPolicy(end: Policy.End.WithPos)(
      onBreakPolicy: Policy,
  )(implicit fileLine: FileLine): Policy = Policy ? onBreakPolicy.isEmpty ||
    new Policy.Map(endPolicy = end, desc = onBreakPolicy.toString)({ s =>
      if (s.isNL) s.orPolicy(onBreakPolicy) else s
    })

  def delayedBreakPolicyBefore(token: T)(onBreakPolicy: Policy)(implicit
      fileLine: FileLine,
  ): Policy = delayedBreakPolicy(Policy.End < token)(onBreakPolicy)

  def delayedBreakPolicyFor(token: T)(f: T => Policy)(implicit
      fileLine: FileLine,
  ): Policy = delayedBreakPolicyBefore(token)(f(token))

  def decideNewlinesOnlyBeforeClose(close: T)(implicit
      fileLine: FileLine,
  ): Policy = decideNewlinesOnlyBeforeClose(Split(Newline, 0))(close)

  def decideNewlinesOnlyBeforeCloseOnBreak(close: T)(implicit
      fileLine: FileLine,
  ): Policy = delayedBreakPolicyFor(close)(decideNewlinesOnlyBeforeClose)

  def decideNewlinesOnlyBeforeClose(split: Split)(close: T)(implicit
      fileLine: FileLine,
  ): Policy = new DecideNewlinesOnlyBeforeToken(close, Option(split))

  def decideNewlinesOnlyBeforeToken(token: T)(implicit
      fileLine: FileLine,
  ): Policy = new DecideNewlinesOnlyBeforeToken(token, None)

  def decideNewlinesOnlyAfterClose(close: T)(implicit
      fileLine: FileLine,
  ): Policy = decideNewlinesOnlyAfterClose(Split(Newline, 0))(close)

  def decideNewlinesOnlyAfterClose(split: Split)(close: T)(implicit
      fileLine: FileLine,
  ): Policy = new DecideNewlinesOnlyAfterToken(close, Option(split))

  def decideNewlinesOnlyAfterToken(token: T)(implicit
      fileLine: FileLine,
  ): Policy = new DecideNewlinesOnlyAfterToken(token, None)

  def unindentAtExclude(exclude: TokenRanges, indent: Length): Policy = exclude
    .ranges.foldLeft(Policy.noPolicy) { case (policy, range) =>
      val (lt, rt) = (range.lt, range.rt)
      val trigger = rt
      val unindent = Indent(indent, rt, ExpiresOn.After)
      val triggeredIndent = Indent.before(unindent, trigger)
      val triggerUnindent = Policy
        .on(rt) { case Decision(FormatToken(`lt`, _, _), s) =>
          s.map(_.withIndent(triggeredIndent))
        }
      val cancelUnindent = delayedBreakPolicy(Policy.End == lt) {
        Policy.after(lt, rank = 1) { // use rank to apply after policy above
          case Decision(FormatToken(`lt`, _, _), s) => s
              .map(_.switch(trigger, false))
        }
      }
      policy ==> triggerUnindent & cancelUnindent
    }

}
