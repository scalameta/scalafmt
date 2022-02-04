package org.scalafmt.util

import scala.meta.tokens.{Token => T}

import org.scalafmt.internal.Decision
import org.scalafmt.internal.Newline
import org.scalafmt.internal.Policy
import org.scalafmt.internal.Policy.End
import org.scalafmt.internal.Policy.Pf
import org.scalafmt.internal.Split
import org.scalafmt.internal.TokenRanges
import org.scalameta.FileLine

object PolicyOps {

  /** @param noSyntaxNL
    *   do not allow newlines in token syntax
    */
  class PenalizeAllNewlines(
      val endPolicy: End.WithPos,
      penalty: Int,
      penalizeLambdas: Boolean = true,
      noSyntaxNL: Boolean = false
  )(implicit fileLine: FileLine)
      extends Policy.Clause {
    override val noDequeue: Boolean = false
    override val f: Policy.Pf = {
      case Decision(ft, s) if penalizeLambdas || !ft.left.is[T.RightArrow] =>
        if (noSyntaxNL && ft.leftHasNewline) s.map(_.withPenalty(penalty))
        else s.map(x => if (x.isNL) x.withPenalty(penalty) else x)
    }
    override def toString: String = s"PNL:${super.toString}+$penalty"
  }

  object PenalizeAllNewlines {
    def apply(
        expire: T,
        penalty: Int,
        penalizeLambdas: Boolean = true,
        noSyntaxNL: Boolean = false
    )(implicit fileLine: FileLine): Policy = {
      new PenalizeAllNewlines(
        Policy.End.Before(expire),
        penalty,
        penalizeLambdas,
        noSyntaxNL
      )
    }
  }

  /** Forces all splits up to including expire to be on a single line.
    * @param okSLC
    *   if true, allow single-line comments
    * @param noSyntaxNL
    *   if false, allow newlines in token syntax
    */
  class SingleLineBlock(
      val endPolicy: End.WithPos,
      okSLC: Boolean = false,
      noSyntaxNL: Boolean = false
  )(implicit fileLine: FileLine)
      extends Policy.Clause {
    import TokenOps.isLeftCommentThenBreak
    override val noDequeue: Boolean = true
    override def toString: String = "SLB:" + super.toString
    override val f: Policy.Pf = {
      case Decision(ft, s)
          if !(ft.right.is[T.EOF] || okSLC && isLeftCommentThenBreak(ft)) =>
        if (noSyntaxNL && ft.leftHasNewline) Seq.empty else s.filterNot(_.isNL)
    }
  }

  object SingleLineBlock {

    def apply(
        expire: T,
        exclude: TokenRanges = TokenRanges.empty,
        okSLC: Boolean = false,
        noSyntaxNL: Boolean = false
    )(implicit fileLine: FileLine): Policy =
      policyWithExclude(exclude, End.On, End.After)(
        End.On(expire),
        new SingleLineBlock(_, okSLC = okSLC, noSyntaxNL = noSyntaxNL)
      )
  }

  final class DecideNewlinesOnlyBeforeToken(val token: T, split: Option[Split])(
      implicit fileLine: FileLine
  ) extends Policy.Clause {
    override val endPolicy: End.WithPos = End.On(token)
    override val noDequeue: Boolean = false
    override val f: Pf = split.fold[Pf] {
      {
        case d: Decision if d.formatToken.right eq token =>
          d.onlyNewlinesWithoutFallback
      }
    } { s =>
      {
        case d: Decision if d.formatToken.right eq token =>
          d.onlyNewlinesWithFallback(s)
      }
    }
    override def toString: String = "NB:" + super.toString
  }

  final class DecideNewlinesOnlyAfterToken(val token: T, split: Option[Split])(
      implicit fileLine: FileLine
  ) extends Policy.Clause {
    override val endPolicy: End.WithPos = End.After(token)
    override val noDequeue: Boolean = false
    override val f: Pf = split.fold[Pf] {
      {
        case d: Decision if d.formatToken.left eq token =>
          d.onlyNewlinesWithoutFallback
      }
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
      endLt: T => End.WithPos,
      endRt: T => End.WithPos
  )(
      expire: End.WithPos,
      policyFunc: End.WithPos => Policy
  )(implicit fileLine: FileLine): Policy = {
    val lastPolicy = policyFunc(expire)
    exclude.ranges.foldLeft(lastPolicy) { case (policy, range) =>
      new Policy.Relay(
        policyFunc(endLt(range.lt)),
        new Policy.Delay(policy, endRt(range.rt))
      )
    }
  }

  private def delayedBreakPolicyFactory(onBreakPolicy: Policy): Policy.Pf = {
    object OnBreakDecision {
      def unapply(d: Decision): Option[Seq[Split]] = {
        var replaced = false
        def decisionPf(s: Split): Split =
          if (!s.isNL) s
          else {
            replaced = true
            s.orPolicy(onBreakPolicy)
          }
        val splits = d.splits.map(decisionPf)
        if (replaced) Some(splits) else None
      }
    }
    { case OnBreakDecision(d) =>
      d
    }
  }

  def delayedBreakPolicy(
      end: Policy.End.WithPos
  )(onBreakPolicy: Policy)(implicit fileLine: FileLine): Policy =
    Policy.Proxy(onBreakPolicy, end)(delayedBreakPolicyFactory)

  def delayedBreakPolicyBefore(
      token: T
  )(onBreakPolicy: Policy)(implicit fileLine: FileLine): Policy =
    delayedBreakPolicy(Policy.End.Before(token))(onBreakPolicy)

  def delayedBreakPolicyFor(
      token: T
  )(f: T => Policy)(implicit fileLine: FileLine): Policy =
    delayedBreakPolicyBefore(token)(f(token))

  def decideNewlinesOnlyBeforeClose(close: T)(implicit
      fileLine: FileLine
  ): Policy =
    decideNewlinesOnlyBeforeClose(Split(Newline, 0))(close)

  def decideNewlinesOnlyBeforeCloseOnBreak(close: T)(implicit
      fileLine: FileLine
  ): Policy =
    delayedBreakPolicyFor(close)(decideNewlinesOnlyBeforeClose)

  def decideNewlinesOnlyBeforeClose(
      split: Split
  )(close: T)(implicit fileLine: FileLine): Policy =
    new DecideNewlinesOnlyBeforeToken(close, Option(split))

  def decideNewlinesOnlyAfterClose(close: T)(implicit
      fileLine: FileLine
  ): Policy =
    decideNewlinesOnlyAfterClose(Split(Newline, 0))(close)

  def decideNewlinesOnlyAfterClose(
      split: Split
  )(close: T)(implicit fileLine: FileLine): Policy =
    new DecideNewlinesOnlyAfterToken(close, Option(split))

  def decideNewlinesOnlyAfterToken(
      token: T
  )(implicit fileLine: FileLine): Policy =
    new DecideNewlinesOnlyAfterToken(token, None)

}
