package org.scalafmt.internal


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

  def apply(decision: Decision): Decision = f(decision)

  def orElse(other: Policy): Policy =
    Policy(f orElse other.f, Math.max(expire, other.expire),
      // TODO(olafur) wrong in some corner cases
      noDequeue || other.noDequeue )

  override def toString = s"P:${line.value}(D=$noDequeue)"

}

object Policy {
  val IdentityPolicy: PartialFunction[Decision, Decision] = {
    case d => d
  }
  val empty = Policy(IdentityPolicy, Integer.MAX_VALUE)
}

