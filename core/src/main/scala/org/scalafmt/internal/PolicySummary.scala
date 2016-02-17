package org.scalafmt.internal

class PolicySummary(val policies: Seq[Policy]) {

  val noDequeue = policies.exists(_.noDequeue)

  def combine(other: Policy, position: Int): PolicySummary = {
    // TODO(olafur) filter policies by expiration date
//    val activePolicies = policies.filter(_.expire >= position)
      new PolicySummary(policies :+ other)
    }

  def execute(decision: Decision): Decision = {
    var result = decision
    policies.foreach { policy =>
      if (policy.f.isDefinedAt(result)) {
        result = policy.f(result)
      }
    }
    result
  }
}


object PolicySummary {
  val empty = new PolicySummary(Seq.empty[Policy])
}
