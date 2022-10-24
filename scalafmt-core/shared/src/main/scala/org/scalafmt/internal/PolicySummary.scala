package org.scalafmt.internal

import scala.meta.tokens.Token

import org.scalafmt.util.LoggerOps

class PolicySummary(val policies: Seq[Policy]) extends AnyVal {
  import LoggerOps._

  @inline def noDequeue = policies.exists(_.noDequeue)

  def combine(other: Policy, ft: FormatToken): PolicySummary =
    if (ft.right.is[Token.EOF]) PolicySummary.empty
    else
      new PolicySummary(
        (other +: policies).flatMap(_.unexpiredOpt(ft)).sortBy(_.rank)
      )

  def execute(decision: Decision, debug: Boolean = false): Decision =
    policies.foldLeft(decision) { case (result, policy) =>
      def withSplits(splits: Seq[Split]): Decision = {
        if (debug) logger.debug(s"$policy defined at $result")
        result.withSplits(splits)
      }
      policy.f
        .andThen(withSplits _)
        .applyOrElse(result, identity[Decision])
    }

}

object PolicySummary {
  val empty = new PolicySummary(Seq.empty)
}
