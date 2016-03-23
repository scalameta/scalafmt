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
                  expire: Int,
                  noDequeue: Boolean = false)(
    implicit val line: sourcecode.Line) {

  /** Similar to PartialFunction.andThen, except applies second pf even if the
    * first pf is not defined at argument.
    */
  def andThen(otherF: PartialFunction[Decision, Decision]) = {
    // TODO(olafur) optimize?
    val newPf: PartialFunction[Decision, Decision] = {
      case x =>
        otherF.applyOrElse(
            f.applyOrElse(x, identity[Decision]), identity[Decision])
    }
    copy(f = newPf)
  }

  override def toString = s"P:${line.value}(D=$noDequeue)"
}

object Policy {
  val IdentityPolicy: PartialFunction[Decision, Decision] = {
    case d => throw NoopDefaultPolicyApplied(d)
  }

  val empty = new Policy(IdentityPolicy, Integer.MAX_VALUE) {

    override def toString: String = "NoPolicy"
  }

  val NoPolicy = empty
}
