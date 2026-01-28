package org.scalafmt.internal

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.Policy.NoPolicy
import org.scalafmt.util.PolicyOps

import org.scalameta.FileLine
import scala.meta.tokens.{Token => T}

case class OptimalToken(
    token: FT,
    killOnFail: Boolean,
    recurseOnly: Boolean = false,
    ignorePenalty: Boolean = false,
) {
  override def toString: String = {
    val kof = if (killOnFail) "!" else ""
    val rec = if (recurseOnly) "+" else ""
    s"${token.left}$kof[${token.idx}]$rec"
  }
}

/** A Split is the whitespace between two non-whitespace tokens.
  *
  * Consider a split to be an edge in a search graph and [[FormatToken]] are the
  * nodes.
  *
  * NB: there's a historical inconsistency in how splits are sorted; when they
  * are initially considered, cost is the primary factor (and hence, because of
  * stable sort, earlier split with the same cost will take precedence).
  * However, when a search state is added into the priority queue, preference is
  * given to states with lower cost, further token and, unlike above, a LATER
  * line defining the split.
  *
  * A possible reason for the latter is to give those "secondary" splits a
  * chance to move through the BestFirstSearch algorithm, as otherwise a
  * sequence of primary splits might end up as the winning solution even if it
  * exceeds the maxColumn margins, because a secondary split was deemed unlikely
  * to win and moved to a backup priority queue.
  *
  * @param modExt
  *   whitespace and indents
  * @param cost
  *   How good is this output? Lower is better.
  * @param policy
  *   How does this split affect other later splits?
  * @param fileLine
  *   For debugging, to retrace from which case in [[Router]] this split
  *   originates.
  */
case class Split(
    modExt: ModExt,
    private val cost: Int,
    policy: Policy = NoPolicy,
    neededTags: Set[SplitTag] = Set.empty,
    activeTags: Set[SplitTag] = Set.empty,
    optimalAt: Option[OptimalToken] = None,
    penalty: Int = 0,
    rank: Int = 0,
)(implicit val fileLineStack: FileLineStack) {
  import PolicyOps._

  def withNoIndent: Split = mod match {
    case x: NewlineT if !x.noIndent =>
      copy(modExt = modExt.copy(mod = x.copy(noIndent = true)))
    case _ => this
  }

  @inline
  def costWithoutPenalty: Int = cost

  @inline
  def costWithPenalty: Int = cost + penalty.max(0)

  @inline
  def noCost: Boolean = cost <= 0 && penalty <= 0

  @inline
  def fileLine: FileLine = fileLineStack.fileLineLast

  @inline
  def isNL: Boolean = modExt.isNL

  @inline
  def mod: Modification = modExt.mod

  @inline
  def length: Int = mod.length

  @inline
  def isIgnored: Boolean = neededTags eq Split.ignoredTags

  @inline
  def isActive: Boolean = neededTags == activeTags

  @inline
  def isActiveFor(splitTag: SplitTag): Boolean = activeTags(splitTag)

  @inline
  def isNeededFor(splitTag: SplitTag): Boolean = neededTags(splitTag)

  private def ignored: Split =
    if (isIgnored) this else copy(neededTags = Split.ignoredTags)

  @inline
  def notIf(flag: Boolean): Split = onlyIf(!flag)

  @inline
  def onlyIf(flag: Boolean): Split = if (flag) this else ignored

  def onlyFor(splitTag: SplitTag): Split =
    if (isIgnored) this
    else {
      val newNeededTags = neededTags + splitTag
      if (newNeededTags eq neededTags) this else copy(neededTags = newNeededTags)
    }

  def activateFor(splitTag: SplitTag): Split =
    if (isIgnored) this
    else {
      val newActiveTags = activeTags + splitTag
      if (newActiveTags eq activeTags) this else copy(activeTags = newActiveTags)
    }

  def deActivateFor(splitTag: SplitTag): Split =
    if (isIgnored) this
    else {
      val newActiveTags = activeTags - splitTag
      if (newActiveTags eq activeTags) this else copy(activeTags = newActiveTags)
    }

  def preActivateFor(splitTag: SplitTag): Split =
    if (isIgnored) this
    else {
      val newActiveTags = activeTags + splitTag
      val newNeededTags = neededTags + splitTag
      if ((newActiveTags eq activeTags) && (newNeededTags eq neededTags)) this
      else copy(activeTags = newActiveTags, neededTags = newNeededTags)
    }

  def preActivateFor(splitTag: Option[SplitTag]): Split =
    if (isIgnored) this else splitTag.fold(this)(preActivateFor)

  def forThisLine(implicit fileLine: FileLine): Split =
    if (isIgnored) this
    else copy()(fileLineStack.forThisLine(fileLine.file, fileLine.line))

  def withOptimalTokenOpt(
      token: => Option[FT],
      killOnFail: Boolean,
      extend: Boolean = false,
      recurseOnly: Boolean = false,
      ignorePenalty: Boolean = false,
  ): Split = withOptimalAt(
    token.map(OptimalToken(
      _,
      killOnFail = killOnFail,
      recurseOnly = recurseOnly,
      ignorePenalty = ignorePenalty,
    )),
    extend = extend,
  )

  def withOptimalToken(
      token: => FT,
      killOnFail: Boolean,
      ignore: Boolean = false,
      extend: Boolean = false,
      recurseOnly: Boolean = false,
      ignorePenalty: Boolean = false,
  ): Split = withOptimalAt(
    Some(OptimalToken(
      token,
      killOnFail = killOnFail,
      recurseOnly = recurseOnly,
      ignorePenalty = ignorePenalty,
    )),
    ignore,
    extend = extend,
  )

  def withOptimalAt(
      fOptimalAt: => Option[OptimalToken],
      ignore: Boolean = false,
      extend: Boolean = false,
  ): Split =
    if (ignore || isIgnored) this
    else fOptimalAt match {
      case None => this
      case optAt @ Some(opt) =>
        require(cost == 0, s"can't set optimal, cost=$cost")
        def amend() = copy(optimalAt = optAt)
        this.optimalAt match {
          case None => amend()
          case Some(x) if extend =>
            if (x.token.idx < opt.token.idx) amend() else this
          case _ =>
            throw new UnsupportedOperationException("Can't reset optimal token")
        }
    }

  def withPolicy(
      newPolicy: => Policy,
      ignore: Boolean = false,
      extend: Boolean = false,
  ): Split =
    if (ignore) this
    else if (extend) andPolicy(newPolicy)
    else if (isIgnored) this
    else if (policy.isEmpty) {
      val policyEval = newPolicy
      if (policyEval.isEmpty) this else copy(policy = policyEval)
    } else throw new UnsupportedOperationException("Use orPolicy or andPolicy")

  def withSingleLine(
      expire: => FT,
      exclude: => TokenRanges = TokenRanges.empty,
      noSyntaxNL: Boolean = false,
      killOnFail: => Option[Boolean] = None,
      rank: Int = 0,
      extend: Boolean = false,
      ignore: Boolean = false,
      noOptimal: Boolean = false,
      recurseOnly: Boolean = false,
      ignorePenalty: Boolean = false,
  )(implicit fileLine: FileLine, style: ScalafmtConfig): Split =
    if (isIgnored || ignore) this
    else {
      val expireEval = expire
      val excludeEval = exclude
      withOptimalToken(
        expireEval,
        killOnFail = killOnFail.getOrElse(excludeEval.isEmpty),
        extend = extend,
        ignore = noOptimal,
        recurseOnly = recurseOnly,
        ignorePenalty = ignorePenalty,
      ).withSingleLineNoOptimal(
        expireEval,
        excludeEval,
        noSyntaxNL,
        rank,
        extend = extend,
      )
    }

  def withSingleLineNoOptimal(
      expire: => FT,
      exclude: => TokenRanges = TokenRanges.empty,
      noSyntaxNL: Boolean = false,
      rank: Int = 0,
      ignore: Boolean = false,
      extend: Boolean = false,
  )(implicit fileLine: FileLine, style: ScalafmtConfig): Split = withPolicy(
    SingleLineBlock(expire, exclude, noSyntaxNL = noSyntaxNL, rank = rank),
    ignore = ignore,
    extend = extend,
  )

  def orPolicy(newPolicy: Policy): Split =
    if (isIgnored || newPolicy.isEmpty) this
    else copy(policy = policy | newPolicy)

  def andPolicy(newPolicy: Policy): Split =
    if (isIgnored || newPolicy.isEmpty) this
    else copy(policy = policy & newPolicy)

  def andPolicy(newPolicy: => Policy, ignore: Boolean): Split =
    if (ignore) this else andPolicy(newPolicy)

  def andFirstPolicy(newPolicy: Policy): Split =
    if (isIgnored || newPolicy.isEmpty) this
    else copy(policy = newPolicy & policy)

  def withPenalty(penalty: Int): Split =
    if (isIgnored) this else copy(penalty = this.penalty + penalty)

  def withIndent(length: => Length, expire: => FT, when: ExpiresOn): Split =
    withIndent(length, expire, when, ignore = false)

  def withIndent(
      length: => Length,
      expire: => FT,
      when: ExpiresOn,
      ignore: Boolean,
  ): Split = withMod(modExt.withIndent(length, expire, when), ignore)

  def withIndentOpt(
      length: => Length,
      expire: Option[FT],
      when: ExpiresOn,
  ): Split = withMod(modExt.withIndentOpt(length, expire, when))

  def withIndent(indent: => Indent, ignore: Boolean = false): Split =
    withMod(modExt.withIndent(indent), ignore)

  def withIndentOpt(indent: => Option[Indent]): Split =
    withMod(modExt.withIndentOpt(indent))

  def withIndents(indents: Indent*): Split = withMod(modExt.withIndents(indents))

  def withIndents(indents: Seq[Indent], ignore: Boolean = false): Split =
    withMod(modExt.withIndents(indents), ignore)

  def switch(trigger: T, on: Boolean): Split =
    if (isIgnored) this
    else {
      val switchedModExt = modExt.switch(trigger, on)
      val switchedPolicy = policy.switch(trigger, on)
      if (policy.eq(switchedPolicy) && modExt.eq(switchedModExt)) this
      else copy(modExt = switchedModExt, policy = switchedPolicy)
    }

  def withMod(mod: Modification): Split =
    withMod(modExt.copy(mod = mod), this.mod eq mod)

  def withMod(modExtByName: => ModExt, ignore: Boolean = false): Split =
    if (ignore || isIgnored) this
    else {
      val modExt = modExtByName
      if (this.modExt eq modExt) this else copy(modExt = modExt)
    }

  override def toString = {
    val prefix =
      if (isIgnored) "!"
      else {
        val wantedTags = neededTags.filterNot(activeTags).mkString(",")
        val unusedTags = activeTags.filterNot(neededTags).mkString(",")
        if (unusedTags.nonEmpty) s"[$wantedTags!$unusedTags]"
        else if (wantedTags.nonEmpty) s"[$wantedTags]"
        else ""
      }
    val opt = optimalAt.fold("")(", opt=" + _)
    s"""${prefix}c=$cost[$penalty] $modExt:[$fileLineStack]($policy$opt)"""
  }
}

object Split {

  private val ignoredTags = Set[SplitTag](null)

  def ignored(implicit fileLineStack: FileLineStack) = Split(ModExt(NoSplit), 0)
    .ignored

  def apply(ignore: Boolean, cost: Int)(modExt: => ModExt)(implicit
      fileLineStack: FileLineStack,
  ): Split = if (ignore) ignored else Split(modExt, cost)

  def opt(mod: Modification, cost: Int, policy: Policy = Policy.NoPolicy)(
      implicit fileLineStack: FileLineStack,
  ): Split = if (mod eq null) ignored else Split(mod, cost, policy)

  implicit class ImplicitSeqSplit(private val obj: Seq[Split]) extends AnyVal {
    def penalize(penalty: Int): Seq[Split] = obj.map(_.withPenalty(penalty))
    def penalizeIf(penalty: Int)(f: Split => Boolean): Seq[Split] = obj
      .map(x => if (f(x)) x.withPenalty(penalty) else x)
    def penalizeNL(penalty: Int): Seq[Split] = penalizeIf(penalty)(_.isNL)
  }

}
