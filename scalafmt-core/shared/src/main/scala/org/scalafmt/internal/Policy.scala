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
  def unexpired(ft: FormatToken): Policy
  def noDequeue: Boolean

  def &(other: Policy): Policy =
    if (other.isEmpty) this else new Policy.AndThen(this, other)
  def |(other: Policy): Policy =
    if (other.isEmpty) this else new Policy.OrElse(this, other)

  @inline
  final def unexpiredOpt(ft: FormatToken): Option[Policy] =
    Some(unexpired(ft)).filter(_.nonEmpty)

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

    override def unexpired(ft: FormatToken): Policy = this
    override def filter(pred: Clause => Boolean): Policy = this
    override def exists(pred: Clause => Boolean): Boolean = false
    override def noDequeue: Boolean = false
  }

  def apply(
      expire: Token,
      endPolicy: End,
      noDequeue: Boolean = false
  )(f: Pf)(implicit line: sourcecode.Line): Policy =
    new ClauseImpl(f, expire.end, endPolicy, noDequeue)

  abstract class Clause(implicit val line: sourcecode.Line) extends Policy {
    val f: Policy.Pf
    val endPos: Int
    val endPolicy: End

    override def toString = {
      val noDeqPrefix = if (noDequeue) "!" else ""
      s"${line.value}$endPolicy$endPos${noDeqPrefix}d"
    }

    override def unexpired(ft: FormatToken): Policy =
      if (endPolicy.notExpiredBy(ft, endPos)) this else NoPolicy

    override def filter(pred: Clause => Boolean): Policy =
      if (pred(this)) this else NoPolicy

    override def exists(pred: Clause => Boolean): Boolean = pred(this)
  }

  private class ClauseImpl(
      val f: Policy.Pf,
      val endPos: Int,
      val endPolicy: End,
      val noDequeue: Boolean
  )(implicit line: sourcecode.Line)
      extends Clause

  private class OrElse(p1: Policy, p2: Policy) extends Policy {
    override lazy val f: Pf = p1.f.orElse(p2.f)

    override def unexpired(ft: FormatToken): Policy =
      p1.unexpired(ft) | p2.unexpired(ft)

    override def filter(pred: Clause => Boolean): Policy =
      p1.filter(pred) | p2.filter(pred)

    override def exists(pred: Clause => Boolean): Boolean =
      p1.exists(pred) || p2.exists(pred)

    override def noDequeue: Boolean =
      p1.noDequeue || p2.noDequeue

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

    override def unexpired(ft: FormatToken): Policy =
      p1.unexpired(ft) & p2.unexpired(ft)

    override def filter(pred: Clause => Boolean): Policy =
      p1.filter(pred) & p2.filter(pred)

    override def exists(pred: Clause => Boolean): Boolean =
      p1.exists(pred) || p2.exists(pred)

    override def noDequeue: Boolean =
      p1.noDequeue || p2.noDequeue

    override def toString: String = s"($p1 & $p2)"
  }

  object Proxy {
    def apply(
        policy: Policy
    )(factory: Policy => Pf)(implicit line: sourcecode.Line): Policy =
      if (policy.isEmpty) NoPolicy
      else new Proxy(policy, factory)
  }

  private class Proxy(
      policy: Policy,
      factory: Policy => Policy.Pf
  )(implicit line: sourcecode.Line)
      extends Policy {
    override val f: Pf = factory(policy)

    override def exists(pred: Clause => Boolean): Boolean = policy.exists(pred)

    override def filter(pred: Clause => Boolean): Policy =
      Proxy(policy.filter(pred))(factory)

    override def unexpired(ft: FormatToken): Policy =
      Proxy(policy.unexpired(ft))(factory)

    override def noDequeue: Boolean = policy.noDequeue

    override def toString: String = s"*($policy)"
  }

  sealed trait End {
    def notExpiredBy(ft: FormatToken, endPos: Int): Boolean
  }
  object End {
    case object After extends End {
      def notExpiredBy(ft: FormatToken, endPos: Int): Boolean =
        ft.left.end <= endPos
      override def toString: String = ">"
    }
    case object Before extends End {
      def notExpiredBy(ft: FormatToken, endPos: Int): Boolean =
        ft.right.end < endPos
      override def toString: String = "<"
    }
    case object On extends End {
      def notExpiredBy(ft: FormatToken, endPos: Int): Boolean =
        ft.right.end <= endPos
      override def toString: String = "@"
    }
  }

}
