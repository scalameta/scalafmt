package org.scalafmt.internal

import org.scalafmt.internal.Policy.NoPolicy
import org.scalafmt.util.LoggerOps

class PolicySummary(val policies: Vector[Policy]) {
  import LoggerOps._

  val noDequeue = policies.exists(_.noDequeue)

  def combine(other: Policy, position: Int): PolicySummary = {
    // TODO(olafur) filter policies by expiration date
    val activePolicies = policies.filter(_.expire > position)
    val newPolicies =
      if (other == NoPolicy) activePolicies
      else other +: activePolicies
    new PolicySummary(newPolicies)
  }

  def execute(decision: Decision, debug: Boolean = false): Decision = {
    var last = decision
    var result = decision
    policies.foreach { policy =>
      if (policy.f.isDefinedAt(result)) {
        last = result
        result = policy.f(result)
        // TODO(olafur Would be nice to enforce this at compile time.
        assert(result.formatToken == last.formatToken)
        if (debug) {
          logger.debug(s"$policy defined at $result")
        }
      }
    }
    result
  }
}

object PolicySummary {
  val empty = new PolicySummary(Vector.empty[Policy])
}
