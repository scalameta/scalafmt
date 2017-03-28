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
case class Policy(
    f: PartialFunction[Decision, Decision],
    expire: Int,
    noDequeue: Boolean = false,
    isSingleLine: Boolean = false)(implicit val line: sourcecode.Line) {

  def merge(other: PartialFunction[Decision, Decision], newExpire: Int): Policy =
    Policy(f.orElse(other), newExpire)

  def merge(other: Policy, newExpire: Int): Policy =
    merge(other.f, newExpire)

  def merge(other: Policy): Policy = {
    val newExpire = expire.max(other.expire)
    merge(other, newExpire)
  }


  def andThen(other: Policy): Policy = {
    if (this.f == Policy.emptyPf) other
    else this.andThen(other.f)
  }

  /** Similar to PartialFunction.andThen, except applies second pf even if the
    * first pf is not defined at argument.
    */
  def andThen(otherF: PartialFunction[Decision, Decision]): Policy = {
    // TODO(olafur) optimize?
    val newPf: PartialFunction[Decision, Decision] = {
      case x =>
        otherF.applyOrElse(f.applyOrElse(x, identity[Decision]),
                           identity[Decision])
    }
    copy(f = newPf)
  }

  override def toString = s"P:${line.value}(D=$noDequeue)"
}

object Policy {
  val IdentityPolicy: PartialFunction[Decision, Decision] = {
    case d => throw NoopDefaultPolicyApplied(d)
  }

  val emptyPf = PartialFunction.empty[Decision, Decision]

  val empty = new Policy(IdentityPolicy, Integer.MAX_VALUE) {

    override def toString: String = "NoPolicy"
  }

  val NoPolicy = empty
}
