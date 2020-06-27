package org.scalafmt.internal

import scala.meta.tokens.Token

/**
  * The decision made by [[Router]].
  *
  * Used by [[Policy]] to enforce non-local formatting.
  */
abstract class Policy {

  /** applied to every decision until expire */
  def f: Policy.Pf

  def exists(pred: Policy.Clause => Boolean): Boolean
  def filter(pred: Policy.Clause => Boolean): Policy
  def unexpired(pos: Int): Policy = filter(_.endPos > pos)

  def &(other: Policy): Policy =
    if (other.isEmpty) this else new Policy.AndThen(this, other)
  def |(other: Policy): Policy =
    if (other.isEmpty) this else new Policy.OrElse(this, other)

  @inline
  final def unexpiredOpt(pos: Int): Option[Policy] =
    Some(unexpired(pos)).filter(_.nonEmpty)

  @inline
  final def &(other: Option[Policy]): Policy = other.fold(this)(&)
  @inline
  final def |(other: Option[Policy]): Policy = other.fold(this)(|)

  @inline
  final def isEmpty: Boolean = this eq Policy.NoPolicy
  @inline
  final def nonEmpty: Boolean = this ne Policy.NoPolicy

}

object Policy {

  type Pf = PartialFunction[Decision, Seq[Split]]

  object NoPolicy extends Policy {
    override def toString: String = "NoPolicy"
    override def f: Pf = PartialFunction.empty
    override def |(other: Policy): Policy = other
    override def &(other: Policy): Policy = other

    override def unexpired(pos: Int): Policy = this
    override def filter(pred: Clause => Boolean): Policy = this
    override def exists(pred: Clause => Boolean): Boolean = false
  }

  def apply(token: Token)(f: Pf)(implicit line: sourcecode.Line): Policy =
    apply(token.end)(f)

  def apply(
      endPos: Int,
      noDequeue: Boolean = false
  )(f: Pf)(implicit line: sourcecode.Line): Policy =
    new Clause(f, endPos, noDequeue)

  class Clause(
      val f: Policy.Pf,
      val endPos: Int,
      val noDequeue: Boolean = false
  )(implicit val line: sourcecode.Line)
      extends Policy {
    override def toString = {
      val noDeqPrefix = if (noDequeue) "!" else ""
      s"P:${line.value}<$endPos${noDeqPrefix}d"
    }

    override def filter(pred: Clause => Boolean): Policy =
      if (pred(this)) this else NoPolicy

    override def exists(pred: Clause => Boolean): Boolean = pred(this)
  }

  private class OrElse(p1: Policy, p2: Policy) extends Policy {
    override lazy val f: Pf = p1.f.orElse(p2.f)

    override def filter(pred: Clause => Boolean): Policy =
      p1.filter(pred) | p2.filter(pred)

    override def exists(pred: Clause => Boolean): Boolean =
      p1.exists(pred) || p2.exists(pred)

    override def toString: String = s"($p1 | $p2)"
  }

  private class AndThen(p1: Policy, p2: Policy) extends Policy {
    override lazy val f: Pf = {
      case x =>
        p2.f.applyOrElse(
          p1.f.andThen(x.withSplits _).applyOrElse(x, identity[Decision]),
          (y: Decision) => y.splits
        )
    }

    override def filter(pred: Clause => Boolean): Policy =
      p1.filter(pred) & p2.filter(pred)

    override def exists(pred: Clause => Boolean): Boolean =
      p1.exists(pred) || p2.exists(pred)

    override def toString: String = s"($p1 & $p2)"
  }

}
