package org.scalafmt.internal

import scala.meta.tokens.Token
import org.scalameta.FileLine

/** The decision made by [[Router]].
  *
  * Used by [[Policy]] to enforce non-local formatting.
  */
abstract class Policy {

  /** applied to every decision until expire */
  def f: Policy.Pf
  def rank: Int

  def filter(pred: Policy.Clause => Boolean): Policy
  def unexpired(ft: FormatToken): Policy
  def noDequeue: Boolean
  def switch(trigger: Token, on: Boolean): Policy

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

  @inline
  def noPolicy: Policy = NoPolicy

  object NoPolicy extends Policy {
    override def toString: String = "NoPolicy"
    override def f: Pf = PartialFunction.empty
    override def |(other: Policy): Policy = other
    override def &(other: Policy): Policy = other

    override def rank: Int = 0
    override def unexpired(ft: FormatToken): Policy = this
    override def filter(pred: Clause => Boolean): Policy = this
    override def switch(trigger: Token, on: Boolean): Policy = this
    override def noDequeue: Boolean = false
  }

  def apply(
      endPolicy: End.WithPos,
      noDequeue: Boolean = false,
      rank: Int = 0
  )(f: Pf)(implicit fileLine: FileLine): Policy =
    new ClauseImpl(f, endPolicy, noDequeue, rank)

  def after(
      token: Token,
      noDequeue: Boolean = false,
      rank: Int = 0
  )(f: Pf)(implicit fileLine: FileLine): Policy =
    apply(End.After(token), noDequeue, rank)(f)

  def before(
      token: Token,
      noDequeue: Boolean = false,
      rank: Int = 0
  )(f: Pf)(implicit fileLine: FileLine): Policy =
    apply(End.Before(token), noDequeue, rank)(f)

  def after(trigger: Token, policy: Policy)(implicit
      fileLine: FileLine
  ): Policy =
    new Switch(NoPolicy, trigger, policy)

  def before(policy: Policy, trigger: Token)(implicit
      fileLine: FileLine
  ): Policy =
    new Switch(policy, trigger, NoPolicy)

  def on(
      token: Token,
      noDequeue: Boolean = false
  )(f: Pf)(implicit fileLine: FileLine): Policy =
    apply(End.On(token), noDequeue)(f)

  abstract class Clause(implicit val fileLine: FileLine) extends Policy {
    val endPolicy: End.WithPos

    override def toString = {
      val noDeqPrefix = if (noDequeue) "!" else ""
      s"[$fileLine]$endPolicy${noDeqPrefix}d"
    }

    override def unexpired(ft: FormatToken): Policy =
      if (endPolicy.notExpiredBy(ft)) this else NoPolicy

    override def filter(pred: Clause => Boolean): Policy =
      if (pred(this)) this else NoPolicy

    override def switch(trigger: Token, on: Boolean): Policy = this
  }

  private class ClauseImpl(
      val f: Policy.Pf,
      val endPolicy: End.WithPos,
      val noDequeue: Boolean,
      val rank: Int = 0
  )(implicit fileLine: FileLine)
      extends Clause

  private class OrElse(p1: Policy, p2: Policy) extends Policy {
    override lazy val f: Pf = p1.f.orElse(p2.f)

    override def rank: Int = math.min(p1.rank, p2.rank)

    override def unexpired(ft: FormatToken): Policy =
      p1.unexpired(ft) | p2.unexpired(ft)

    override def filter(pred: Clause => Boolean): Policy =
      p1.filter(pred) | p2.filter(pred)

    override def switch(trigger: Token, on: Boolean): Policy =
      p1.switch(trigger, on) | p2.switch(trigger, on)

    override def noDequeue: Boolean =
      p1.noDequeue || p2.noDequeue

    override def toString: String = s"($p1 | $p2)"
  }

  private class AndThen(p1: Policy, p2: Policy) extends Policy {
    override lazy val f: Pf = { case x =>
      p2.f.applyOrElse(
        p1.f.andThen(x.withSplits _).applyOrElse(x, identity[Decision]),
        (y: Decision) => y.splits
      )
    }

    override def rank: Int = math.min(p1.rank, p2.rank)

    override def unexpired(ft: FormatToken): Policy =
      p1.unexpired(ft) & p2.unexpired(ft)

    override def filter(pred: Clause => Boolean): Policy =
      p1.filter(pred) & p2.filter(pred)

    override def switch(trigger: Token, on: Boolean): Policy =
      p1.switch(trigger, on) & p2.switch(trigger, on)

    override def noDequeue: Boolean =
      p1.noDequeue || p2.noDequeue

    override def toString: String = s"($p1 & $p2)"
  }

  class Delay(policy: Policy, begPolicy: End.WithPos)(implicit
      fileLine: FileLine
  ) extends Policy {
    override def f: Pf = PartialFunction.empty
    override def rank: Int = 0
    override def filter(pred: Clause => Boolean): Policy = this
    override def switch(trigger: Token, on: Boolean): Policy = {
      val switched = policy.switch(trigger, on)
      if (switched eq policy) this else new Delay(switched, begPolicy)
    }
    override def unexpired(ft: FormatToken): Policy =
      if (begPolicy.notExpiredBy(ft)) this else policy.unexpired(ft)
    override def noDequeue: Boolean = policy.noDequeue
    override def toString: String = s"$begPolicy:$policy"
  }

  object Delay {
    def apply(policy: Policy, begPolicy: End.WithPos)(implicit
        fileLine: FileLine
    ): Policy =
      if (policy.isEmpty) policy else new Delay(policy, begPolicy)
  }

  class Relay(before: Policy, after: Policy)(implicit
      fileLine: FileLine
  ) extends Policy {
    override def f: Pf = before.f
    override def rank: Int = before.rank
    override def filter(pred: Clause => Boolean): Policy = conv(_.filter(pred))
    override def switch(trigger: Token, on: Boolean): Policy =
      conv(_.switch(trigger, on))
    override def unexpired(ft: FormatToken): Policy = conv(_.unexpired(ft))
    override def noDequeue: Boolean = before.noDequeue
    override def toString: String = s"REL:[$fileLine]($before,$after)"

    private def conv(func: Policy => Policy): Policy = {
      val filtered = func(before)
      if (filtered eq before) this
      else if (filtered.isEmpty) func(after)
      else new Relay(filtered, after)
    }
  }

  object Relay {
    def apply(before: Policy, after: Policy)(implicit
        fileLine: FileLine
    ): Policy =
      if (before.isEmpty) after
      else if (after.isEmpty) before
      else new Relay(before, after)
  }

  class Switch(before: Policy, trigger: Token, after: Policy)(implicit
      fileLine: FileLine
  ) extends Policy {
    override def f: Pf = before.f
    override def rank: Int = before.rank
    override def filter(pred: Clause => Boolean): Policy = conv(_.filter(pred))
    override def switch(trigger: Token, on: Boolean): Policy =
      if (trigger ne this.trigger) conv(_.switch(trigger, on))
      else { if (on) before else after.switch(trigger, false) }
    override def unexpired(ft: FormatToken): Policy = conv(_.unexpired(ft))
    override def noDequeue: Boolean = before.noDequeue
    override def toString: String = s"SW:[$fileLine]($before,$trigger,$after)"

    private def conv(func: Policy => Policy): Policy = {
      val filtered = func(before)
      if (filtered eq before) this
      else new Switch(filtered, trigger, after)
    }
  }

  object Proxy {
    def apply(
        policy: Policy,
        end: End.WithPos
    )(factory: Policy => Pf)(implicit fileLine: FileLine): Policy =
      if (policy.isEmpty) NoPolicy
      else new Proxy(policy, factory, end)
  }

  private class Proxy(
      policy: Policy,
      factory: Policy => Policy.Pf,
      override val endPolicy: End.WithPos
  )(implicit fileLine: FileLine)
      extends Policy.Clause {
    override val f: Pf = factory(policy)
    override def rank: Int = policy.rank

    override def filter(pred: Clause => Boolean): Policy =
      if (!pred(this)) NoPolicy
      else Proxy(policy.filter(pred), endPolicy)(factory)

    override def switch(trigger: Token, on: Boolean): Policy = {
      val switched = policy.switch(trigger, on)
      if (switched eq policy) this else Proxy(switched, endPolicy)(factory)
    }

    override def unexpired(ft: FormatToken): Policy =
      if (!endPolicy.notExpiredBy(ft)) NoPolicy
      else Proxy(policy.unexpired(ft), endPolicy)(factory)

    override def noDequeue: Boolean = policy.noDequeue

    override def toString: String = s"*($policy)$endPolicy"
  }

  sealed trait End extends (Token => End.WithPos) {
    def apply(endPos: Int): End.WithPos
    final def apply(token: Token): End.WithPos = apply(token.end)
  }
  object End {
    sealed trait WithPos {
      def notExpiredBy(ft: FormatToken): Boolean
    }
    case object After extends End {
      def apply(endPos: Int): WithPos =
        new End.WithPos {
          def notExpiredBy(ft: FormatToken): Boolean = ft.left.end <= endPos
          override def toString: String = s">$endPos"
        }
    }
    case object Before extends End {
      def apply(endPos: Int): WithPos =
        new End.WithPos {
          def notExpiredBy(ft: FormatToken): Boolean = ft.right.end < endPos
          override def toString: String = s"<$endPos"
        }
    }
    case object On extends End {
      def apply(endPos: Int): WithPos =
        new End.WithPos {
          def notExpiredBy(ft: FormatToken): Boolean = ft.right.end <= endPos
          override def toString: String = s"@$endPos"
        }
    }
  }

}
