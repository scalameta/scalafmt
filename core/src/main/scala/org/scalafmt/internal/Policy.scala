package org.scalafmt.internal

import org.scalafmt.Error.NoopDefaultPolicyApplied


/**
  * The decision made by [[Router]].
  *
  * Used by [[Policy]] to enforce non-local formatting.
  *
  * @param f is applied to every decision until expire
  * @param expire The latest token offset.
  */
case class Policy(f: PartialFunction[Decision, Decision],
                  expire: Int, noDequeue: Boolean = false)(implicit val line: sourcecode.Line) {

  override def toString = s"P:${line.value}(D=$noDequeue)"

}

object Policy {
  val IdentityPolicy: PartialFunction[Decision, Decision] = {
    case d =>
      throw NoopDefaultPolicyApplied(d)
  }
  val empty = Policy(IdentityPolicy, Integer.MAX_VALUE)
}

