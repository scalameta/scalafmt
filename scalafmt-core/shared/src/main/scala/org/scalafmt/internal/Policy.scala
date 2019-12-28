package org.scalafmt.internal

import scala.meta.tokens.Token

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
    f: Policy.Pf,
    expire: Int,
    noDequeue: Boolean = false,
    isSingleLine: Boolean = false
)(implicit val line: sourcecode.Line) {

  def orElse(other: Policy.Pf, minExpire: Int = 0): Policy =
    if (other == Policy.emptyPf) this
    else
      copy(
        f = if (f == Policy.emptyPf) other else f.orElse(other),
        expire = math.max(minExpire, expire)
      )

  def orElse(other: Policy): Policy =
    orElse(other.f, other.expire)

  def orElse(other: Option[Policy]): Policy =
    other.fold(this)(orElse)

  def andThen(other: Policy): Policy = {
    if (this.f == Policy.emptyPf) other
    else this.andThen(other.f)
  }

  /** Similar to PartialFunction.andThen, except applies second pf even if the
    * first pf is not defined at argument.
    */
  def andThen(otherF: Policy.Pf): Policy = {
    if (otherF == Policy.emptyPf) this
    else if (this.f == Policy.emptyPf) copy(f = otherF)
    else {
      // TODO(olafur) optimize?
      val newPf: Policy.Pf = {
        case x =>
          otherF.applyOrElse(
            f.applyOrElse(x, identity[Decision]),
            identity[Decision]
          )
      }
      copy(f = newPf)
    }
  }

  override def toString = s"P:${line.value}(D=$noDequeue)"
}

object Policy {

  type Pf = PartialFunction[Decision, Decision]

  val IdentityPolicy: Pf = {
    case d => throw NoopDefaultPolicyApplied(d)
  }

  val emptyPf: Pf = PartialFunction.empty

  val empty = new Policy(IdentityPolicy, Integer.MAX_VALUE) {

    override def toString: String = "NoPolicy"
  }

  val NoPolicy = empty

  def apply(func: Token => Pf)(token: Token): Policy =
    new Policy(func(token), token.end)

}
