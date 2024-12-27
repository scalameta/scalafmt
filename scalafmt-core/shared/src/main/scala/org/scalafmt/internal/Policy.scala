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
  import Policy._

  /** applied to every decision until expire */
  def f: Pf
  def rank: Int
  def terminal: Boolean
  def noDequeue: Boolean
  def maxEndPos: End.WithPos

  protected def asAfter(pos: End.WithPos): Policy
  protected def asUntil(pos: End.WithPos): Policy

  def filter(pred: Clause => Boolean): Policy
  def exists(pred: Clause => Boolean): Boolean
  def appliesUntil(nextft: FT)(pred: Clause => Boolean): Boolean
  def appliesOn(nextft: FT)(pred: Clause => Boolean): Option[Boolean]
  def unexpired(split: Split, nextft: FT): Policy
  def switch(trigger: T, on: Boolean): Policy

  def &(other: Policy): Policy =
    if (other.isEmpty) this else new AndThen(this, other)
  def |(other: Policy): Policy =
    if (other.isEmpty) this else new OrElse(this, other)
  def ==>(other: Policy): Policy = Relay(this, other)
  def <==(pos: End.WithPos): Policy = Expire(this, pos)
  def ?(flag: Boolean): Policy = if (flag) this else NoPolicy

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
  final def <(exp: FT): Policy = <==(End < exp)
  @inline
  final def <=(exp: FT): Policy = <==(End <= exp)
  @inline
  final def >=(exp: FT): Policy = <==(End >= exp)
  @inline
  final def >(exp: FT): Policy = <==(End > exp)

  @inline
  final def isEmpty: Boolean = this eq NoPolicy
  @inline
  final def nonEmpty: Boolean = this ne NoPolicy

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
    override def ==>(other: Policy): Policy = other
    override def <==(pos: End.WithPos): Policy = this
    override def ?(flag: Boolean): Policy = this

    override def rank: Int = 0
    override def terminal: Boolean = false
    override def noDequeue: Boolean = false
    override def maxEndPos: End.WithPos = End.Never

    override protected def asAfter(pos: End.WithPos): Policy = this
    override protected def asUntil(pos: End.WithPos): Policy = this

    override def unexpired(split: Split, nextft: FT): Policy = this
    override def appliesUntil(nextft: FT)(pred: Clause => Boolean): Boolean =
      false
    override def appliesOn(nextft: FT)(
        pred: Clause => Boolean,
    ): Option[Boolean] = None
    override def filter(pred: Clause => Boolean): Policy = this
    override def exists(pred: Clause => Boolean): Boolean = false
    override def switch(trigger: T, on: Boolean): Policy = this
  }

  def apply(
      prefix: String,
      noDequeue: Boolean = false,
      rank: Int = 0,
      terminal: Boolean = false,
  )(f: Pf)(implicit fl: FileLine): Policy =
    new ClauseImpl(f, prefix, noDequeue, rank, terminal = terminal)

  def after(trigger: T, policy: Policy)(implicit fl: FileLine): Policy =
    new Switch(NoPolicy, trigger, policy)

  def before(policy: Policy, trigger: T)(implicit fl: FileLine): Policy =
    new Switch(policy, trigger, NoPolicy)

  def beforeLeft(
      exp: FT,
      prefix: String,
      noDequeue: Boolean = false,
      rank: Int = 0,
      terminal: Boolean = false,
  )(f: Pf)(implicit fl: FileLine): Policy =
    apply(prefix, noDequeue, rank, terminal = terminal)(f) < exp

  def onLeft(
      exp: FT,
      prefix: String,
      noDequeue: Boolean = false,
      rank: Int = 0,
      terminal: Boolean = false,
  )(f: Pf)(implicit fl: FileLine): Policy =
    apply(prefix, noDequeue, rank = rank, terminal = terminal)(f) <= exp

  def onRight(
      exp: FT,
      prefix: String,
      noDequeue: Boolean = false,
      rank: Int = 0,
      terminal: Boolean = false,
  )(f: Pf)(implicit fl: FileLine): Policy =
    apply(prefix, noDequeue, rank = rank, terminal = terminal)(f) >= exp

  def afterRight(
      exp: FT,
      prefix: String,
      noDequeue: Boolean = false,
      rank: Int = 0,
      terminal: Boolean = false,
  )(f: Pf)(implicit fl: FileLine): Policy =
    apply(prefix, noDequeue, rank, terminal = terminal)(f) > exp

  def onlyFor(
      on: FT,
      prefix: String,
      noDequeue: Boolean = false,
      rank: Int = 0,
      terminal: Boolean = false,
  )(f: Seq[Split] => Seq[Split])(implicit fl: FileLine): Policy = End <= on ==>
    onRight(on, s"$prefix[${on.idx}]", noDequeue, rank, terminal = terminal) {
      case Decision(`on`, ss) => f(ss)
    }

  abstract class Clause(implicit val fl: FileLine) extends Policy {
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
      s"$prefixWithColon[$fl]${noDeqPrefix}d$suffixWithColon"
    }

    override protected def asAfter(pos: End.WithPos): Policy = this
    override protected def asUntil(pos: End.WithPos): Policy = this

    override def unexpired(split: Split, nextft: FT): Policy = this

    override def filter(pred: Clause => Boolean): Policy =
      if (pred(this)) this else NoPolicy

    override def exists(pred: Clause => Boolean): Boolean = pred(this)

    override def switch(trigger: T, on: Boolean): Policy = this

    override def maxEndPos: End.WithPos = End.Never
    override def appliesUntil(nextft: FT)(pred: Clause => Boolean): Boolean =
      pred(this)

    override def appliesOn(nextft: FT)(
        pred: Clause => Boolean,
    ): Option[Boolean] = Some(pred(this))
  }

  private class ClauseImpl(
      val f: Pf,
      val prefix: String,
      val noDequeue: Boolean,
      val rank: Int = 0,
      val terminal: Boolean = false,
  )(implicit fl: FileLine)
      extends Clause

  abstract class WithConv extends Policy {
    override protected def asAfter(pos: End.WithPos): Policy =
      conv(_.asAfter(pos))
    override protected def asUntil(pos: End.WithPos): Policy =
      conv(_.asUntil(pos))

    override def unexpired(split: Split, nextft: FT): Policy =
      conv(_.unexpired(split, nextft))

    override def filter(pred: Clause => Boolean): Policy = conv(_.filter(pred))

    override def switch(trigger: T, on: Boolean): Policy =
      conv(_.switch(trigger, on))

    protected def conv(pred: Policy => Policy): Policy
  }

  private def appliesOnAll(nextft: FT, p1: Policy, p2: Policy)(
      pred: Clause => Boolean,
  ): Option[Boolean] = {
    def r2 = p2.appliesOn(nextft)(pred)
    p1.appliesOn(nextft)(pred).fold(r2)(r1 => Some(r1 && !r2.contains(false)))
  }

  private def appliesOnAny(nextft: FT, p1: Policy, p2: Policy)(
      pred: Clause => Boolean,
  ): Option[Boolean] = {
    def r2 = p2.appliesOn(nextft)(pred)
    p1.appliesOn(nextft)(pred).fold(r2)(r1 => Some(r1 || r2.contains(true)))
  }

  private def appliesOn1st(nextft: FT, p1: Policy, p2: Policy)(
      pred: Clause => Boolean,
  ): Option[Boolean] = p1.appliesOn(nextft)(pred)
    .orElse(p2.appliesOn(nextft)(pred))

  private class OrElse(p1: Policy, p2: Policy) extends WithConv {
    override def toString: String = s"($p1 | $p2)"

    override lazy val f: Pf = p1.f.orElse(p2.f)
    override def rank: Int = math.min(p1.rank, p2.rank)
    override def terminal: Boolean = p1.terminal || p2.terminal
    override def noDequeue: Boolean = p1.noDequeue || p2.noDequeue
    override def maxEndPos: End.WithPos = p1.maxEndPos.max(p2.maxEndPos)

    protected def conv(pred: Policy => Policy): Policy = {
      val np1 = pred(p1)
      val np2 = pred(p2)
      if (np1.eq(p1) && np2.eq(p2)) this else np1 | np2
    }

    override def appliesUntil(nextft: FT)(pred: Clause => Boolean): Boolean = p1
      .appliesUntil(nextft)(pred) && p2.appliesUntil(nextft)(pred)
    override def appliesOn(nextft: FT)(
        pred: Clause => Boolean,
    ): Option[Boolean] = appliesOnAll(nextft, p1, p2)(pred)

    override def exists(pred: Clause => Boolean): Boolean = p1.exists(pred) ||
      p2.exists(pred)
  }

  private class AndThen(p1: Policy, p2: Policy) extends WithConv {
    override def toString: String = s"($p1 & $p2)"

    override lazy val f: Pf = { case x =>
      p2.f.applyOrElse(
        p1.f.andThen(x.withSplits _).applyOrElse(x, identity[Decision]),
        (y: Decision) => y.splits,
      )
    }

    override def rank: Int = math.min(p1.rank, p2.rank)
    override def terminal: Boolean = p1.terminal || p2.terminal
    override def noDequeue: Boolean = p1.noDequeue || p2.noDequeue
    override def maxEndPos: End.WithPos = p1.maxEndPos.max(p2.maxEndPos)

    protected def conv(pred: Policy => Policy): Policy = {
      val np1 = pred(p1)
      val np2 = pred(p2)
      if (np1.eq(p1) && np2.eq(p2)) this else np1 & np2
    }

    override def appliesUntil(nextft: FT)(pred: Clause => Boolean): Boolean = p1
      .appliesUntil(nextft)(pred) || p2.appliesUntil(nextft)(pred)
    override def appliesOn(nextft: FT)(
        pred: Clause => Boolean,
    ): Option[Boolean] = appliesOnAny(nextft, p1, p2)(pred)

    override def exists(pred: Clause => Boolean): Boolean = p1.exists(pred) ||
      p2.exists(pred)
  }

  private object Expire {
    def apply(policy: Policy, endPolicy: End.WithPos): Expire =
      policy.asUntil(endPolicy) match {
        case p: Expire => p
        case p => new Expire(p, endPolicy)
      }
  }

  private class Expire private (policy: Policy, endPolicy: End.WithPos)
      extends WithConv {
    override def toString: String = s"$policy <== $endPolicy"

    override def f: Pf = policy.f
    override def rank: Int = policy.rank
    override def terminal: Boolean = policy.terminal
    override def noDequeue: Boolean = policy.noDequeue
    override def maxEndPos: End.WithPos = endPolicy.min(policy.maxEndPos)

    override protected def asAfter(pos: End.WithPos): Policy =
      if (pos >= endPolicy) NoPolicy
      else {
        val p = policy.asAfter(pos)
        if (p eq policy) this else new Expire(p, endPolicy)
      }
    override protected def asUntil(pos: End.WithPos): Policy =
      if (pos >= endPolicy) this else policy.asUntil(pos)

    override def unexpired(split: Split, nextft: FT): Policy =
      if (!endPolicy.notExpiredBy(nextft)) NoPolicy
      else super.unexpired(split, nextft)

    protected def conv(func: Policy => Policy): Policy = {
      val filtered = func(policy)
      if (filtered eq policy) this else filtered <== endPolicy
    }

    override def appliesUntil(nextft: FT)(pred: Clause => Boolean): Boolean =
      endPolicy.notExpiredBy(nextft) && policy.appliesUntil(nextft)(pred)
    override def appliesOn(
        nextft: FT,
    )(pred: Clause => Boolean): Option[Boolean] =
      if (!endPolicy.notExpiredBy(nextft)) None
      else policy.appliesOn(nextft)(pred)

    override def exists(pred: Clause => Boolean): Boolean = policy.exists(pred)
  }

  private object Delay {
    def apply(policy: Policy, begPolicy: End.WithPos): Delay =
      policy.asAfter(begPolicy) match {
        case p: Delay => p
        case p => new Delay(p, begPolicy)
      }
  }

  private class Delay private (val policy: Policy, val begPolicy: End.WithPos)
      extends Policy {
    override def toString: String = s"$begPolicy ==> $policy"

    override def f: Pf = PartialFunction.empty
    override def rank: Int = 0
    override def terminal: Boolean = policy.terminal
    override def noDequeue: Boolean = policy.noDequeue
    override def maxEndPos: End.WithPos = begPolicy.max(policy.maxEndPos)

    override def <==(pos: End.WithPos): Policy =
      if (pos <= begPolicy) NoPolicy else Expire(this, pos)
    override def ==>(other: Policy): Policy =
      Relay(this, other.asAfter(begPolicy))

    override protected def asAfter(pos: End.WithPos): Policy =
      if (pos <= begPolicy) this else policy.asAfter(pos)
    override protected def asUntil(pos: End.WithPos): Policy =
      if (pos <= begPolicy) NoPolicy
      else {
        val p = policy.asUntil(pos)
        if (p eq policy) this else new Delay(p, begPolicy)
      }

    override def filter(pred: Clause => Boolean): Policy = this
    override def exists(pred: Clause => Boolean): Boolean = policy.exists(pred)
    override def switch(trigger: T, on: Boolean): Policy = this
    override def unexpired(split: Split, nextft: FT): Policy =
      if (begPolicy.notExpiredBy(nextft)) this
      else policy.unexpired(split, nextft)

    override def appliesUntil(nextft: FT)(pred: Clause => Boolean): Boolean =
      false
    override def appliesOn(
        nextft: FT,
    )(pred: Clause => Boolean): Option[Boolean] =
      if (begPolicy.notExpiredBy(nextft)) None
      else policy.appliesOn(nextft)(pred)
  }

  abstract class WithBeforeAndAfter extends WithConv {
    def before: Policy
    def after: Policy

    protected def withBefore(before: Policy)(func: Policy => Policy): Policy

    override protected def conv(func: Policy => Policy): Policy = {
      val filtered = func(before)
      if (filtered eq before) this else withBefore(filtered)(func)
    }

    override def f: Pf = before.f
    override def rank: Int = before.rank
    override def noDequeue: Boolean = before.noDequeue
    override def terminal: Boolean = before.terminal || after.terminal
    override def maxEndPos: End.WithPos = before.maxEndPos.max(after.maxEndPos)

    override def appliesUntil(nextft: FT)(pred: Clause => Boolean): Boolean =
      before.appliesUntil(nextft)(pred) && after.appliesUntil(nextft)(pred)

    override def appliesOn(nextft: FT)(
        pred: Clause => Boolean,
    ): Option[Boolean] = appliesOnAll(nextft, before, after)(pred)

    override def exists(pred: Clause => Boolean): Boolean = before
      .exists(pred) || after.exists(pred)
  }

  private object Relay {
    def apply(before: Policy, after: Policy): Policy =
      if (after.isEmpty) before else new Relay(before, after)
  }

  private class Relay private (val before: Policy, val after: Policy)
      extends WithBeforeAndAfter {
    override def toString: String = s"$before ==> $after"
    override protected def withBefore(before: Policy)(
        func: Policy => Policy,
    ): Policy = if (before.isEmpty) func(after) else new Relay(before, after)
    override def appliesOn(nextft: FT)(
        pred: Clause => Boolean,
    ): Option[Boolean] = appliesOn1st(nextft, before, after)(pred)
  }

  class RelayOnSplit(
      val before: Policy,
      trigger: (Split, FT) => Boolean,
      triggerEnd: End.WithPos,
      val after: Policy,
  )(implicit fl: FileLine)
      extends WithBeforeAndAfter {
    override def toString: String = s"REL?:[$fl]($before ??? $after)"

    override def unexpired(split: Split, nextft: FT): Policy =
      if (trigger(split, nextft)) after.unexpired(split, nextft)
      else if (!triggerEnd.notExpiredBy(nextft)) NoPolicy
      else super.unexpired(split, nextft)

    override protected def withBefore(before: Policy)(
        func: Policy => Policy,
    ): Policy = new RelayOnSplit(before, trigger, triggerEnd, after)
  }

  object RelayOnSplit {
    def by(triggerEnd: End.WithPos)(
        trigger: (Split, FT) => Boolean,
    )(before: Policy)(after: Policy)(implicit fl: FileLine): Policy =
      if (before.isEmpty) after
      else new RelayOnSplit(before, trigger, triggerEnd, after)
    def apply(trigger: (Split, FT) => Boolean)(before: Policy)(after: Policy)(
        implicit fl: FileLine,
    ): Policy = by(End.Never)(trigger)(before)(after)
  }

  class Switch(val before: Policy, trigger: T, val after: Policy)(implicit
      fl: FileLine,
  ) extends WithBeforeAndAfter {
    override def toString: String = s"SW:[$fl]($before,$trigger,$after)"

    override def switch(trigger: T, on: Boolean): Policy =
      if (trigger ne this.trigger) super.switch(trigger, on)
      else if (on) before
      else after.switch(trigger, on = false)

    override protected def withBefore(before: Policy)(
        func: Policy => Policy,
    ): Policy = new Switch(before, trigger, after)
  }

  final class Map(
      val noDequeue: Boolean = false,
      val rank: Int = 0,
      desc: => String = "",
      val terminal: Boolean = false,
  )(pred: Split => Split)(implicit fl: FileLine)
      extends Clause {
    private object PredicateDecision {
      def unapply(d: Decision): Option[Seq[Split]] = {
        var replaced = false
        def applyMap(s: Split): Option[Split] = Option(pred(s)).filter(ss =>
          (s eq ss) || {
            replaced = true
            !ss.isIgnored
          },
        )
        val splits = d.splits.flatMap(applyMap)
        if (replaced) Some(splits) else None
      }
    }
    override val f: Pf = { case PredicateDecision(ss) => ss }
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

    sealed trait WithPos extends Ordered[WithPos] {
      val endIdx: Int
      def notExpiredBy(ft: FT): Boolean = ft.idx <= endIdx
      def ==>(policy: Policy): Policy = Delay(policy, this)
      override final def compare(that: WithPos): Int = endIdx - that.endIdx
      def max(that: WithPos): WithPos = if (this < that) that else this
      def min(that: WithPos): WithPos = if (this > that) that else this
    }
    case object BeforeLeft extends End {
      def apply(exp: FT): WithPos = new End.WithPos {
        val endIdx: Int = exp.idx - 2
        override def toString: String =
          s"<${getDescWithoutPos(exp.left)}[${exp.idx}]"
      }
    }
    case object OnLeft extends End {
      def apply(exp: FT): WithPos = new End.WithPos {
        val endIdx: Int = exp.idx - 1
        override def toString: String =
          s"<=${getDescWithoutPos(exp.left)}[${exp.idx}]"
      }
    }
    case object OnRight extends End {
      def apply(exp: FT): WithPos = new End.WithPos {
        val endIdx: Int = exp.idx
        override def toString: String =
          s">=${getDescWithoutPos(exp.left)}[${exp.idx}]"
      }
    }
    case object AfterRight extends End {
      def apply(exp: FT): WithPos = new End.WithPos {
        val endIdx: Int = exp.idx + 1
        override def toString: String =
          s">${getDescWithoutPos(exp.left)}[${exp.idx}]"
      }
    }
    case object Never extends WithPos {
      override val endIdx: Int = Int.MaxValue
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
