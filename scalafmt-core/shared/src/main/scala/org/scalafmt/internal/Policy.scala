package org.scalafmt.internal

import org.scalameta.FileLine
import scala.meta.tokens.Token

import scala.language.implicitConversions

/** The decision made by [[Router]].
  *
  * Used by [[Policy]] to enforce non-local formatting.
  */
abstract class Policy {

  /** applied to every decision until expire */
  def f: Policy.Pf
  def rank: Int

  def filter(pred: Policy.Clause => Boolean): Policy
  def exists(pred: Policy.Clause => Boolean): Boolean
  def appliesUntil(nextft: FormatToken)(pred: Policy.Clause => Boolean): Boolean
  def unexpired(split: Split, nextft: FormatToken): Policy
  def noDequeue: Boolean
  def switch(trigger: Token, on: Boolean): Policy

  def &(other: Policy): Policy =
    if (other.isEmpty) this else new Policy.AndThen(this, other)
  def |(other: Policy): Policy =
    if (other.isEmpty) this else new Policy.OrElse(this, other)
  def ==>(other: Policy)(implicit fileLine: FileLine): Policy =
    if (other.isEmpty) this else new Policy.Relay(this, other)
  def <==(other: Policy.End.WithPos): Policy = new Policy.Expire(this, other)
  def ?(flag: Boolean): Policy = if (flag) this else Policy.NoPolicy

  @inline
  final def unexpiredOpt(split: Split, nextft: FormatToken): Option[Policy] =
    Some(unexpired(split, nextft)).filter(_.nonEmpty)

  @inline
  final def &(other: Option[Policy]): Policy = other.fold(this)(&)
  @inline
  final def |(other: Option[Policy]): Policy = other.fold(this)(|)
  @inline
  final def ==>(other: Option[Policy]): Policy = other.fold(this)(==>)

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
    override def ==>(other: Policy)(implicit fileLine: FileLine): Policy = other
    override def <==(other: Policy.End.WithPos): Policy = this
    override def ?(flag: Boolean): Policy = this

    override def rank: Int = 0
    override def unexpired(split: Split, nextft: FormatToken): Policy = this
    override def appliesUntil(nextft: FormatToken)(
        pred: Policy.Clause => Boolean,
    ): Boolean = false
    override def filter(pred: Clause => Boolean): Policy = this
    override def exists(pred: Clause => Boolean): Boolean = false
    override def switch(trigger: Token, on: Boolean): Policy = this
    override def noDequeue: Boolean = false
  }

  def apply(
      endPolicy: End.WithPos,
      prefix: String,
      noDequeue: Boolean = false,
      rank: Int = 0,
  )(f: Pf)(implicit fileLine: FileLine): Policy =
    new ClauseImpl(f, endPolicy, prefix, noDequeue, rank)

  def after(
      token: Token,
      prefix: String,
      noDequeue: Boolean = false,
      rank: Int = 0,
  )(f: Pf)(implicit fileLine: FileLine): Policy =
    apply(End > token, prefix, noDequeue, rank)(f)

  def before(
      token: Token,
      prefix: String,
      noDequeue: Boolean = false,
      rank: Int = 0,
  )(f: Pf)(implicit fileLine: FileLine): Policy =
    apply(End < token, prefix, noDequeue, rank)(f)

  def after(trigger: Token, policy: Policy)(implicit
      fileLine: FileLine,
  ): Policy = new Switch(NoPolicy, trigger, policy)

  def before(policy: Policy, trigger: Token)(implicit
      fileLine: FileLine,
  ): Policy = new Switch(policy, trigger, NoPolicy)

  def on(token: Token, prefix: String, noDequeue: Boolean = false, rank: Int = 0)(
      f: Pf,
  )(implicit fileLine: FileLine): Policy =
    apply(End == token, prefix, noDequeue, rank = rank)(f)

  abstract class Clause(implicit val fileLine: FileLine) extends Policy {
    val endPolicy: End.WithPos
    def prefix: String
    def suffix: String = ""

    override def toString = {
      val prefixWithColon = prefix match {
        case "" => ""
        case x => s"$x:"
      }
      val noDeqPrefix = if (noDequeue) "!" else ""
      val suffixWithColon = suffix match {
        case "" => ""
        case x => s":$x"
      }
      s"$prefixWithColon[$fileLine]$endPolicy${noDeqPrefix}d$suffixWithColon"
    }

    override def unexpired(split: Split, nextft: FormatToken): Policy =
      if (endPolicy.notExpiredBy(nextft)) this else NoPolicy

    override def filter(pred: Clause => Boolean): Policy =
      if (pred(this)) this else NoPolicy

    override def exists(pred: Clause => Boolean): Boolean = pred(this)

    override def switch(trigger: Token, on: Boolean): Policy = this

    override def appliesUntil(nextft: FormatToken)(
        pred: Policy.Clause => Boolean,
    ): Boolean = endPolicy.notExpiredBy(nextft) && pred(this)
  }

  private class ClauseImpl(
      val f: Policy.Pf,
      val endPolicy: End.WithPos,
      val prefix: String,
      val noDequeue: Boolean,
      val rank: Int = 0,
  )(implicit fileLine: FileLine)
      extends Clause

  abstract class WithConv extends Policy {
    override def unexpired(split: Split, nextft: FormatToken): Policy =
      conv(_.unexpired(split, nextft))

    override def filter(pred: Clause => Boolean): Policy = conv(_.filter(pred))

    override def switch(trigger: Token, on: Boolean): Policy =
      conv(_.switch(trigger, on))

    protected def conv(pred: Policy => Policy): Policy
  }

  private class OrElse(p1: Policy, p2: Policy) extends WithConv {
    override lazy val f: Pf = p1.f.orElse(p2.f)

    override def rank: Int = math.min(p1.rank, p2.rank)

    override def noDequeue: Boolean = p1.noDequeue || p2.noDequeue

    override def toString: String = s"($p1 | $p2)"

    protected def conv(pred: Policy => Policy): Policy = {
      val np1 = pred(p1)
      val np2 = pred(p2)
      if (np1.eq(p1) && np2.eq(p2)) this else np1 | np2
    }

    override def appliesUntil(nextft: FormatToken)(
        pred: Policy.Clause => Boolean,
    ): Boolean = p1.appliesUntil(nextft)(pred) && p2.appliesUntil(nextft)(pred)

    override def exists(pred: Clause => Boolean): Boolean = p1.exists(pred) ||
      p2.exists(pred)
  }

  private class AndThen(p1: Policy, p2: Policy) extends WithConv {
    override lazy val f: Pf = { case x =>
      p2.f.applyOrElse(
        p1.f.andThen(x.withSplits _).applyOrElse(x, identity[Decision]),
        (y: Decision) => y.splits,
      )
    }

    override def rank: Int = math.min(p1.rank, p2.rank)

    override def noDequeue: Boolean = p1.noDequeue || p2.noDequeue

    override def toString: String = s"($p1 & $p2)"

    protected def conv(pred: Policy => Policy): Policy = {
      val np1 = pred(p1)
      val np2 = pred(p2)
      if (np1.eq(p1) && np2.eq(p2)) this else np1 & np2
    }

    override def appliesUntil(nextft: FormatToken)(
        pred: Policy.Clause => Boolean,
    ): Boolean = p1.appliesUntil(nextft)(pred) || p2.appliesUntil(nextft)(pred)

    override def exists(pred: Clause => Boolean): Boolean = p1.exists(pred) ||
      p2.exists(pred)
  }

  private class Expire(policy: Policy, endPolicy: End.WithPos)
      extends WithConv {
    override def f: Pf = policy.f
    override def rank: Int = policy.rank
    override def switch(trigger: Token, on: Boolean): Policy = {
      val res = policy.switch(trigger, on)
      if (res eq policy) this else res
    }
    override def unexpired(split: Split, nextft: FormatToken): Policy =
      if (!endPolicy.notExpiredBy(nextft)) NoPolicy
      else super.unexpired(split, nextft)
    override def noDequeue: Boolean = policy.noDequeue
    override def toString: String = s"$policy <== $endPolicy"

    protected def conv(func: Policy => Policy): Policy = {
      val filtered = func(policy)
      if (filtered eq policy) this else filtered <== endPolicy
    }

    override def appliesUntil(nextft: FormatToken)(
        pred: Policy.Clause => Boolean,
    ): Boolean = endPolicy.notExpiredBy(nextft) &&
      policy.appliesUntil(nextft)(pred)

    override def exists(pred: Clause => Boolean): Boolean = policy.exists(pred)
  }

  private class Delay(policy: Policy, begPolicy: End.WithPos) extends Policy {
    override def f: Pf = PartialFunction.empty
    override def rank: Int = 0
    override def filter(pred: Clause => Boolean): Policy = this
    override def exists(pred: Clause => Boolean): Boolean = policy.exists(pred)
    override def switch(trigger: Token, on: Boolean): Policy = this
    override def unexpired(split: Split, nextft: FormatToken): Policy =
      if (begPolicy.notExpiredBy(nextft)) this
      else policy.unexpired(split, nextft)
    override def noDequeue: Boolean = policy.noDequeue
    override def toString: String = s"$begPolicy ==> $policy"

    override def appliesUntil(nextft: FormatToken)(
        pred: Policy.Clause => Boolean,
    ): Boolean = false
  }

  private class Relay(before: Policy, after: Policy)(implicit fileLine: FileLine)
      extends WithConv {
    override def f: Pf = before.f
    override def rank: Int = before.rank
    override def noDequeue: Boolean = before.noDequeue
    override def toString: String = s"REL:[$fileLine]($before ==> $after)"

    protected def conv(func: Policy => Policy): Policy = {
      val filtered = func(before)
      if (filtered.isEmpty) func(after)
      else if (filtered eq before) this
      else new Relay(filtered, after)
    }

    override def appliesUntil(nextft: FormatToken)(
        pred: Policy.Clause => Boolean,
    ): Boolean = before.appliesUntil(nextft)(pred) &&
      after.appliesUntil(nextft)(pred)

    override def exists(pred: Clause => Boolean): Boolean = before
      .exists(pred) || after.exists(pred)
  }

  class RelayOnSplit(
      before: Policy,
      pred: (Split, FormatToken) => Boolean,
      after: Policy,
  )(implicit fileLine: FileLine)
      extends WithConv {
    override def f: Pf = before.f
    override def rank: Int = before.rank
    override def unexpired(split: Split, nextft: FormatToken): Policy =
      if (pred(split, nextft)) after.unexpired(split, nextft)
      else super.unexpired(split, nextft)

    override def noDequeue: Boolean = before.noDequeue
    override def toString: String = s"REL?:[$fileLine]($before ??? $after)"

    protected def conv(func: Policy => Policy): Policy = {
      val filtered = func(before)
      if (filtered eq before) this else new RelayOnSplit(filtered, pred, after)
    }

    override def appliesUntil(nextft: FormatToken)(
        pred: Policy.Clause => Boolean,
    ): Boolean = before.appliesUntil(nextft)(pred) &&
      after.appliesUntil(nextft)(pred)

    override def exists(pred: Clause => Boolean): Boolean = before
      .exists(pred) || after.exists(pred)
  }

  object RelayOnSplit {
    def apply(
        pred: (Split, FormatToken) => Boolean,
    )(before: Policy, after: Policy)(implicit fileLine: FileLine): Policy =
      if (before.isEmpty) after else new RelayOnSplit(before, pred, after)
  }

  class Switch(before: Policy, trigger: Token, after: Policy)(implicit
      fileLine: FileLine,
  ) extends WithConv {
    override def f: Pf = before.f
    override def rank: Int = before.rank
    override def switch(trigger: Token, on: Boolean): Policy =
      if (trigger ne this.trigger) super.switch(trigger, on)
      else if (on) before
      else after.switch(trigger, false)
    override def noDequeue: Boolean = before.noDequeue
    override def toString: String = s"SW:[$fileLine]($before,$trigger,$after)"

    protected def conv(func: Policy => Policy): Policy = {
      val filtered = func(before)
      if (filtered eq before) this else new Switch(filtered, trigger, after)
    }

    override def appliesUntil(nextft: FormatToken)(
        pred: Policy.Clause => Boolean,
    ): Boolean = before.appliesUntil(nextft)(pred) &&
      after.appliesUntil(nextft)(pred)

    override def exists(pred: Clause => Boolean): Boolean = before
      .exists(pred) || after.exists(pred)
  }

  object Proxy {
    def apply(policy: Policy, end: End.WithPos)(
        factory: Policy => Pf,
    )(implicit fileLine: FileLine): Policy =
      if (policy.isEmpty) NoPolicy else new Proxy(policy, factory, end)
  }

  private class Proxy(
      policy: Policy,
      factory: Policy => Policy.Pf,
      override val endPolicy: End.WithPos,
  )(implicit fileLine: FileLine)
      extends Policy.Clause {
    override val f: Pf = factory(policy)
    override def rank: Int = policy.rank

    override def filter(pred: Clause => Boolean): Policy =
      if (!pred(this)) NoPolicy
      else Proxy(policy.filter(pred), endPolicy)(factory)

    override def exists(pred: Clause => Boolean): Boolean = policy.exists(pred)

    override def switch(trigger: Token, on: Boolean): Policy = {
      val switched = policy.switch(trigger, on)
      if (switched eq policy) this else Proxy(switched, endPolicy)(factory)
    }

    override def unexpired(split: Split, nextft: FormatToken): Policy =
      if (!endPolicy.notExpiredBy(nextft)) NoPolicy
      else Proxy(policy.unexpired(split, nextft), endPolicy)(factory)

    override def appliesUntil(nextft: FormatToken)(
        pred: Policy.Clause => Boolean,
    ): Boolean = policy.appliesUntil(nextft)(pred)

    override def noDequeue: Boolean = policy.noDequeue

    override val prefix: String = "*"
    override def suffix: String = s"($policy)"
  }

  final class Map(
      val endPolicy: Policy.End.WithPos,
      val noDequeue: Boolean = false,
      val rank: Int = 0,
      desc: => String = "",
  )(pred: Split => Split)(implicit fileLine: FileLine)
      extends Policy.Clause {
    private object PredicateDecision {
      def unapply(d: Decision): Option[Seq[Split]] = {
        var replaced = false
        def applyMap(s: Split): Option[Split] = Option(pred(s)).filter { ss =>
          (s eq ss) || {
            replaced = true
            !ss.isIgnored
          }
        }
        val splits = d.splits.flatMap(applyMap)
        if (replaced) Some(splits) else None
      }
    }
    override val f: Policy.Pf = { case PredicateDecision(ss) => ss }
    override def prefix: String = {
      val evalDesc = desc
      if (evalDesc.isEmpty) "MAP" else s"MAP[$evalDesc]"
    }
  }

  sealed trait End extends (Token => End.WithPos) {
    def apply(endPos: Int): End.WithPos
    final def apply(token: Token): End.WithPos = apply(token.end)
  }
  object End {
    def <(endPos: Int): End.WithPos = Before(endPos)
    def <(token: Token): End.WithPos = this < token.end

    def >(endPos: Int): End.WithPos = After(endPos)
    def >(token: Token): End.WithPos = this > token.end

    def ==(endPos: Int): End.WithPos = On(endPos)
    def ==(token: Token): End.WithPos = this == token.end

    sealed trait WithPos {
      def notExpiredBy(ft: FormatToken): Boolean
      def ==>(policy: Policy): Policy =
        if (policy.isEmpty) NoPolicy else new Policy.Delay(policy, this)
    }
    case object After extends End {
      def apply(endPos: Int): WithPos = new End.WithPos {
        def notExpiredBy(ft: FormatToken): Boolean = ft.left.end <= endPos
        override def toString: String = s">$endPos"
      }
    }
    case object Before extends End {
      def apply(endPos: Int): WithPos = new End.WithPos {
        def notExpiredBy(ft: FormatToken): Boolean = ft.right.end < endPos
        override def toString: String = s"<$endPos"
      }
    }
    case object On extends End {
      def apply(endPos: Int): WithPos = new End.WithPos {
        def notExpiredBy(ft: FormatToken): Boolean = ft.right.end <= endPos
        override def toString: String = s"@$endPos"
      }
    }
  }

  implicit def implicitOptionPolicyToPolicy(obj: Option[Policy]): Policy = obj
    .getOrElse(NoPolicy)

  class PolicyBoolean(private val flag: Boolean) extends AnyVal {
    def &&(other: => Policy): Policy = if (flag) other else NoPolicy
    def ||(other: => Policy): Policy = if (flag) NoPolicy else other
  }

  def ?(flag: Boolean) = new PolicyBoolean(flag)

}
