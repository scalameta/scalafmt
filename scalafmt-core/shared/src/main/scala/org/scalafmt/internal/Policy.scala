package org.scalafmt.internal

import org.scalafmt.util.LoggerOps

import org.scalameta.FileLine
import scala.meta.tokens.{Token => T}

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
  def appliesUntil(nextft: FT)(pred: Policy.Clause => Boolean): Boolean
  def unexpired(split: Split, nextft: FT): Policy
  def noDequeue: Boolean
  def switch(trigger: T, on: Boolean): Policy

  def &(other: Policy): Policy =
    if (other.isEmpty) this else new Policy.AndThen(this, other)
  def |(other: Policy): Policy =
    if (other.isEmpty) this else new Policy.OrElse(this, other)
  def ==>(other: Policy)(implicit fileLine: FileLine): Policy =
    if (other.isEmpty) this else new Policy.Relay(this, other)
  def <==(other: Policy.End.WithPos): Policy = new Policy.Expire(this, other)
  def ?(flag: Boolean): Policy = if (flag) this else Policy.NoPolicy

  @inline
  final def unexpiredOpt(split: Split, nextft: FT): Option[Policy] =
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
    override def unexpired(split: Split, nextft: FT): Policy = this
    override def appliesUntil(nextft: FT)(
        pred: Policy.Clause => Boolean,
    ): Boolean = false
    override def filter(pred: Clause => Boolean): Policy = this
    override def exists(pred: Clause => Boolean): Boolean = false
    override def switch(trigger: T, on: Boolean): Policy = this
    override def noDequeue: Boolean = false
  }

  def apply(
      endPolicy: End.WithPos,
      prefix: String,
      noDequeue: Boolean = false,
      rank: Int = 0,
  )(f: Pf)(implicit fileLine: FileLine): Policy =
    new ClauseImpl(f, endPolicy, prefix, noDequeue, rank)

  def after(trigger: T, policy: Policy)(implicit fileLine: FileLine): Policy =
    new Switch(NoPolicy, trigger, policy)

  def before(policy: Policy, trigger: T)(implicit fileLine: FileLine): Policy =
    new Switch(policy, trigger, NoPolicy)

  def beforeLeft(
      exp: FT,
      prefix: String,
      noDequeue: Boolean = false,
      rank: Int = 0,
  )(f: Pf)(implicit fileLine: FileLine): Policy =
    apply(End < exp, prefix, noDequeue, rank)(f)

  def onLeft(exp: FT, prefix: String, noDequeue: Boolean = false, rank: Int = 0)(
      f: Pf,
  )(implicit fileLine: FileLine): Policy =
    apply(End <= exp, prefix, noDequeue, rank = rank)(f)

  def onRight(exp: FT, prefix: String, noDequeue: Boolean = false, rank: Int = 0)(
      f: Pf,
  )(implicit fileLine: FileLine): Policy =
    apply(End >= exp, prefix, noDequeue, rank = rank)(f)

  def afterRight(
      exp: FT,
      prefix: String,
      noDequeue: Boolean = false,
      rank: Int = 0,
  )(f: Pf)(implicit fileLine: FileLine): Policy =
    apply(End > exp, prefix, noDequeue, rank)(f)

  def onlyFor(on: FT, prefix: String, noDequeue: Boolean = false, rank: Int = 0)(
      f: Seq[Split] => Seq[Split],
  )(implicit fileLine: FileLine): Policy = Policy.End <= on ==>
    Policy.onRight(on, s"$prefix[${on.idx}]", noDequeue, rank) {
      case Decision(`on`, ss) => f(ss)
    }

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

    override def unexpired(split: Split, nextft: FT): Policy =
      if (endPolicy.notExpiredBy(nextft)) this else NoPolicy

    override def filter(pred: Clause => Boolean): Policy =
      if (pred(this)) this else NoPolicy

    override def exists(pred: Clause => Boolean): Boolean = pred(this)

    override def switch(trigger: T, on: Boolean): Policy = this

    override def appliesUntil(nextft: FT)(
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
    override def unexpired(split: Split, nextft: FT): Policy =
      conv(_.unexpired(split, nextft))

    override def filter(pred: Clause => Boolean): Policy = conv(_.filter(pred))

    override def switch(trigger: T, on: Boolean): Policy =
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

    override def appliesUntil(nextft: FT)(
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

    override def appliesUntil(nextft: FT)(
        pred: Policy.Clause => Boolean,
    ): Boolean = p1.appliesUntil(nextft)(pred) || p2.appliesUntil(nextft)(pred)

    override def exists(pred: Clause => Boolean): Boolean = p1.exists(pred) ||
      p2.exists(pred)
  }

  private class Expire(policy: Policy, endPolicy: End.WithPos)
      extends WithConv {
    override def f: Pf = policy.f
    override def rank: Int = policy.rank
    override def switch(trigger: T, on: Boolean): Policy = {
      val res = policy.switch(trigger, on)
      if (res eq policy) this else res
    }
    override def unexpired(split: Split, nextft: FT): Policy =
      if (!endPolicy.notExpiredBy(nextft)) NoPolicy
      else super.unexpired(split, nextft)
    override def noDequeue: Boolean = policy.noDequeue
    override def toString: String = s"$policy <== $endPolicy"

    protected def conv(func: Policy => Policy): Policy = {
      val filtered = func(policy)
      if (filtered eq policy) this else filtered <== endPolicy
    }

    override def appliesUntil(nextft: FT)(
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
    override def switch(trigger: T, on: Boolean): Policy = this
    override def unexpired(split: Split, nextft: FT): Policy =
      if (begPolicy.notExpiredBy(nextft)) this
      else policy.unexpired(split, nextft)
    override def noDequeue: Boolean = policy.noDequeue
    override def toString: String = s"$begPolicy ==> $policy"

    override def appliesUntil(nextft: FT)(
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

    override def appliesUntil(nextft: FT)(
        pred: Policy.Clause => Boolean,
    ): Boolean = before.appliesUntil(nextft)(pred) &&
      after.appliesUntil(nextft)(pred)

    override def exists(pred: Clause => Boolean): Boolean = before
      .exists(pred) || after.exists(pred)
  }

  class RelayOnSplit(
      before: Policy,
      trigger: (Split, FT) => Boolean,
      triggerEnd: Policy.End.WithPos,
      after: Policy,
  )(implicit fileLine: FileLine)
      extends WithConv {
    override def f: Pf = before.f
    override def rank: Int = before.rank
    override def unexpired(split: Split, nextft: FT): Policy =
      if (trigger(split, nextft)) after.unexpired(split, nextft)
      else if (!triggerEnd.notExpiredBy(nextft)) NoPolicy
      else super.unexpired(split, nextft)

    override def noDequeue: Boolean = before.noDequeue
    override def toString: String = s"REL?:[$fileLine]($before ??? $after)"

    protected def conv(func: Policy => Policy): Policy = {
      val filtered = func(before)
      if (filtered eq before) this
      else new RelayOnSplit(filtered, trigger, triggerEnd, after)
    }

    override def appliesUntil(nextft: FT)(
        pred: Policy.Clause => Boolean,
    ): Boolean = before.appliesUntil(nextft)(pred) &&
      after.appliesUntil(nextft)(pred)

    override def exists(pred: Clause => Boolean): Boolean = before
      .exists(pred) || after.exists(pred)
  }

  object RelayOnSplit {
    def by(triggerEnd: Policy.End.WithPos)(
        trigger: (Split, FT) => Boolean,
    )(before: Policy)(after: Policy)(implicit fileLine: FileLine): Policy =
      if (before.isEmpty) after
      else new RelayOnSplit(before, trigger, triggerEnd, after)
    def apply(trigger: (Split, FT) => Boolean)(before: Policy)(after: Policy)(
        implicit fileLine: FileLine,
    ): Policy = by(Policy.End.Never)(trigger)(before)(after)
  }

  class Switch(before: Policy, trigger: T, after: Policy)(implicit
      fileLine: FileLine,
  ) extends WithConv {
    override def f: Pf = before.f
    override def rank: Int = before.rank
    override def switch(trigger: T, on: Boolean): Policy =
      if (trigger ne this.trigger) super.switch(trigger, on)
      else if (on) before
      else after.switch(trigger, false)
    override def noDequeue: Boolean = before.noDequeue
    override def toString: String = s"SW:[$fileLine]($before,$trigger,$after)"

    protected def conv(func: Policy => Policy): Policy = {
      val filtered = func(before)
      if (filtered eq before) this else new Switch(filtered, trigger, after)
    }

    override def appliesUntil(nextft: FT)(
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

    override def switch(trigger: T, on: Boolean): Policy = {
      val switched = policy.switch(trigger, on)
      if (switched eq policy) this else Proxy(switched, endPolicy)(factory)
    }

    override def unexpired(split: Split, nextft: FT): Policy =
      if (!endPolicy.notExpiredBy(nextft)) NoPolicy
      else Proxy(policy.unexpired(split, nextft), endPolicy)(factory)

    override def appliesUntil(nextft: FT)(
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

  sealed trait End extends (FT => End.WithPos) {
    def apply(exp: FT): End.WithPos
  }
  object End {
    def <(exp: FT): End.WithPos = BeforeLeft(exp)
    def <=(exp: FT): End.WithPos = OnLeft(exp)
    def >=(exp: FT): End.WithPos = OnRight(exp)
    def >(exp: FT): End.WithPos = AfterRight(exp)

    @inline
    private def getDescWithoutPos(token: T): String = LoggerOps
      .tokWithoutPos(token)

    sealed trait WithPos {
      protected val endIdx: Int
      def notExpiredBy(ft: FT): Boolean = ft.idx <= endIdx
      def ==>(policy: Policy): Policy =
        if (policy.isEmpty) NoPolicy else new Policy.Delay(policy, this)
    }
    case object BeforeLeft extends End {
      def apply(exp: FT): WithPos = new End.WithPos {
        protected val endIdx: Int = exp.idx - 2
        override def toString: String =
          s"<${getDescWithoutPos(exp.left)}[${exp.idx}]"
      }
    }
    case object OnLeft extends End {
      def apply(exp: FT): WithPos = new End.WithPos {
        protected val endIdx: Int = exp.idx - 1
        override def toString: String =
          s"<=${getDescWithoutPos(exp.left)}[${exp.idx}]"
      }
    }
    case object OnRight extends End {
      def apply(exp: FT): WithPos = new End.WithPos {
        protected val endIdx: Int = exp.idx
        override def toString: String =
          s">=${getDescWithoutPos(exp.left)}[${exp.idx}]"
      }
    }
    case object AfterRight extends End {
      def apply(exp: FT): WithPos = new End.WithPos {
        protected val endIdx: Int = exp.idx + 1
        override def toString: String =
          s">${getDescWithoutPos(exp.left)}[${exp.idx}]"
      }
    }
    case object Never extends WithPos {
      override protected val endIdx: Int = Int.MaxValue
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
