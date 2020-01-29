package org.scalafmt.internal

import org.scalafmt.internal.Policy.NoPolicy
import org.scalafmt.util.LoggerOps

class PolicySummary(val policies: Vector[Policy]) {
  import LoggerOps._

  @inline def noDequeue = policies.exists { x =>
    x.isSingleLine || x.noDequeue
  }

  @inline def isSafe = !noDequeue

  def combine(other: Policy, position: Int): PolicySummary = {
    // TODO(olafur) filter policies by expiration date
    val activePolicies = policies.filter(_.expire > position)
    val newPolicies =
      if (other == NoPolicy) activePolicies
      else other +: activePolicies
    new PolicySummary(newPolicies)
  }

  def execute(decision: Decision, debug: Boolean = false): Decision =
    policies.foldLeft(decision) {
      case (result, policy) =>
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
  val empty = new PolicySummary(Vector.empty[Policy])
}
