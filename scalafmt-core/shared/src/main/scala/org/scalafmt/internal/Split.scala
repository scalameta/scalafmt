package org.scalafmt.internal

import scala.meta.tokens.Token

import org.scalafmt.internal.Policy.NoPolicy
import org.scalafmt.util.PolicyOps
import org.scalafmt.util.TokenOps

case class OptimalToken(token: Token, killOnFail: Boolean = false) {
  override def toString: String = s"$token:${token.end}"
}

/**
  * A Split is the whitespace between two non-whitespace tokens.
  *
  * Consider a split to be an edge in a search graph and [[FormatToken]]
  * are the nodes.
  *
  * NB: there's a historical inconsistency in how splits are sorted; when
  * they are initially considered, cost is the primary factor (and hence,
  * because of stable sort, earlier split with the same cost will take
  * precedence). However, when a search state is added into the priority
  * queue, preference is given to states with lower cost, further token
  * and, unlike above, a LATER line defining the split.
  *
  * A possible reason for the latter is to give those "secondary" splits
  * a chance to move through the BestFirstSearch algorithm, as otherwise
  * a sequence of primary splits might end up as the winning solution
  * even if it exceeds the maxColumn margins, because a secondary split
  * was deemed unlikely to win and moved to a backup priority queue.
  *
  * @param modExt whitespace and indents
  * @param cost How good is this output? Lower is better.
  * @param policy How does this split affect other later splits?
  * @param line For debugging, to retrace from which case in [[Router]]
  *             this split originates.
  */
case class Split(
    modExt: ModExt,
    cost: Int,
    neededTags: Set[SplitTag] = Set.empty,
    activeTags: Set[SplitTag] = Set.empty,
    policy: Policy = NoPolicy,
    optimalAt: Option[OptimalToken] = None
)(implicit val line: sourcecode.Line) {
  import PolicyOps._
  import TokenOps._

  def adapt(formatToken: FormatToken): Split =
    modExt.mod match {
      case n: NewlineT if !n.noIndent && rhsIsCommentedOut(formatToken) =>
        copy(modExt = modExt.copy(mod = NewlineT(n.isDouble, noIndent = true)))
      case _ => this
    }

  @inline
  def indentation: String = modExt.indentation

  @inline
  def isNL: Boolean = modExt.mod.isNewline

  @inline
  def length: Int = modExt.mod.length

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

  def onlyFor(splitTag: SplitTag, ignore: Boolean = false): Split =
    if (isIgnored || ignore || isNeededFor(splitTag)) this
    else copy(neededTags = neededTags + splitTag)(line = line)

  def activateFor(splitTag: SplitTag): Split =
    if (isIgnored || isActiveFor(splitTag)) this
    else copy(activeTags = activeTags + splitTag)(line = line)

  def preActivateFor(splitTag: SplitTag): Split =
    if (isIgnored) this
    else
      copy(
        activeTags = activeTags + splitTag,
        neededTags = neededTags + splitTag
      )(line = line)

  def preActivateFor(splitTag: Option[SplitTag]): Split =
    if (isIgnored) this else splitTag.fold(this)(preActivateFor)

  def forThisLine(implicit line: sourcecode.Line): Split =
    if (isIgnored) this else copy()(line = line)

  def getCost(ifActive: Int => Int, ifIgnored: => Int): Int =
    if (isIgnored) ifIgnored else ifActive(cost)

  def withOptimalTokenOpt(
      token: => Option[Token],
      killOnFail: Boolean = false
  ): Split =
    withOptimalAt(token.map(OptimalToken(_, killOnFail)))

  def withOptimalToken(
      token: => Token,
      killOnFail: Boolean = false,
      ignore: Boolean = false
  ): Split =
    if (ignore) this else withOptimalAt(Some(OptimalToken(token, killOnFail)))

  def withOptimalAt(optimalAt: => Option[OptimalToken]): Split = {
    require(this.optimalAt.isEmpty)
    if (isIgnored) this else copy(optimalAt = optimalAt)
  }

  def withPolicy(newPolicy: => Policy, ignore: Boolean = false): Split = {
    if (!policy.isEmpty)
      throw new UnsupportedOperationException("Use orPolicy or andPolicy")
    if (isIgnored || ignore) this else copy(policy = newPolicy)
  }

  def withSingleLine(
      expire: Token,
      exclude: => TokenRanges = TokenRanges.empty,
      noSyntaxNL: Boolean = false,
      killOnFail: Boolean = false
  )(implicit line: sourcecode.Line): Split =
    withSingleLineAndOptimal(
      expire,
      expire,
      exclude,
      noSyntaxNL,
      killOnFail
    )

  def withSingleLineOpt(
      expire: Option[Token],
      exclude: => TokenRanges = TokenRanges.empty,
      noSyntaxNL: Boolean = false,
      killOnFail: Boolean = false
  )(implicit line: sourcecode.Line): Split =
    expire.fold(this)(
      withSingleLine(_, exclude, noSyntaxNL, killOnFail)
    )

  def withSingleLineAndOptimal(
      expire: Token,
      optimal: Token,
      exclude: => TokenRanges = TokenRanges.empty,
      noSyntaxNL: Boolean = false,
      killOnFail: Boolean = false
  )(implicit line: sourcecode.Line): Split =
    withOptimalToken(optimal, killOnFail)
      .withSingleLineNoOptimal(expire, exclude, noSyntaxNL)

  def withSingleLineNoOptimal(
      expire: Token,
      exclude: => TokenRanges = TokenRanges.empty,
      noSyntaxNL: Boolean = false
  )(implicit line: sourcecode.Line): Split =
    withPolicy(SingleLineBlock(expire, exclude, noSyntaxNL = noSyntaxNL))

  def withPolicyOpt(newPolicy: => Option[Policy]): Split =
    if (isIgnored) this else newPolicy.fold(this)(withPolicy(_))

  def orPolicy(newPolicy: Policy): Split =
    if (isIgnored || newPolicy.isEmpty) this
    else copy(policy = policy | newPolicy)(line = line)

  def andPolicy(newPolicy: Policy): Split =
    if (isIgnored || newPolicy.isEmpty) this
    else copy(policy = policy & newPolicy)(line = line)

  def andPolicy(newPolicy: => Policy, ignore: Boolean): Split =
    if (ignore) this else andPolicy(newPolicy)

  def andPolicyOpt(newPolicy: => Option[Policy]): Split =
    if (isIgnored) this else newPolicy.fold(this)(andPolicy)

  def andFirstPolicy(newPolicy: Policy): Split =
    if (isIgnored || newPolicy.isEmpty) this
    else copy(policy = newPolicy & policy)(line = line)

  def andFirstPolicyOpt(newPolicy: => Option[Policy]): Split =
    if (isIgnored) this else newPolicy.fold(this)(andFirstPolicy)

  def withPenalty(penalty: Int): Split =
    if (isIgnored || penalty <= 0) this else copy(cost = cost + penalty)

  def withIndent(length: => Length, expire: => Token, when: ExpiresOn): Split =
    withMod(modExt.withIndent(length, expire, when))

  def withIndentOpt(
      length: => Length,
      expire: Option[Token],
      when: ExpiresOn
  ): Split =
    withMod(modExt.withIndentOpt(length, expire, when))

  def withIndent(indent: => Indent, ignore: Boolean = false): Split =
    withMod(modExt.withIndent(indent), ignore)

  def withIndentOpt(indent: => Option[Indent]): Split =
    withMod(modExt.withIndentOpt(indent))

  def withIndents(indents: Seq[Indent], ignore: Boolean = false): Split =
    withMod(modExt.withIndents(indents), ignore)

  def switch(trigger: Token): Split =
    if (isIgnored) this
    else {
      val switchedModExt = modExt.switch(trigger)
      val switchedPolicy = policy.switch(trigger)
      if (policy.eq(switchedPolicy) && modExt.eq(switchedModExt)) this
      else copy(modExt = switchedModExt, policy = switchedPolicy)
    }

  def withMod(mod: Modification): Split =
    withMod(modExt.copy(mod = mod), this.modExt.mod eq mod)

  def withMod(modExtByName: => ModExt, ignore: Boolean = false): Split =
    if (ignore || isIgnored) this
    else {
      val modExt = modExtByName
      if (this.modExt eq modExt) this else copy(modExt = modExt)(line = line)
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
    s"""$prefix${modExt.mod}:${line.value}(cost=$cost, indents=$indentation, $policy$opt)"""
  }
}

object Split {

  private val ignoredTags = Set[SplitTag](null)

  def ignored(implicit line: sourcecode.Line) =
    Split(ModExt(NoSplit), 0).ignored

  def apply(ignore: Boolean, cost: Int)(
      modExt: ModExt
  )(implicit line: sourcecode.Line): Split =
    if (ignore) ignored else Split(modExt, cost)

}
