package org.scalafmt.internal

import org.scalafmt.util.LoggerOps

import scala.meta.tokens.{Token => T}

class PolicySummary(val policies: Seq[Policy]) extends AnyVal {
  import LoggerOps._

  @inline
  def noDequeue = policies.exists(_.noDequeue)

  def combine(split: Split, nextft: FT): PolicySummary =
    if (nextft.right.is[T.EOF]) PolicySummary.empty
    else new PolicySummary(
      (split.policy +: policies).flatMap(_.unexpiredOpt(split, nextft))
        .sortBy(_.rank),
    )

  def execute(decision: Decision, debug: Boolean = false): Seq[Split] = policies
    .foldLeft(decision) { case (result, policy) =>
      def withSplits(splits: Seq[Split]): Decision = {
        if (debug) logger.debug(s"$policy defined at $result")
        result.withSplits(splits)
      }
      policy.f.andThen(withSplits _).applyOrElse(result, identity[Decision])
    }.splits

  @inline
  def exists(f: Policy => Boolean): Boolean = policies.exists(f)
}

object PolicySummary {
  val empty = new PolicySummary(Seq.empty)
}
