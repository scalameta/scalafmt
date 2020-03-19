package org.scalafmt.internal

import scala.meta.tokens.Token

import org.scalafmt.internal.Length.Num
import org.scalafmt.internal.Policy.NoPolicy
import org.scalafmt.util.TokenOps

case class OptimalToken(token: Token, killOnFail: Boolean = false)

/**
  * A Split is the whitespace between two non-whitespace tokens.
  *
  * Consider a split to be an edge in a search graph and [[FormatToken]]
  * are the nodes.
  *
  * @param modification Is this a space, no space, newline or 2 newlines?
  * @param cost How good is this output? Lower is better.
  * @param indents Does this add indentation?
  * @param policy How does this split affect other later splits?
  * @param line For debugging, to retrace from which case in [[Router]]
  *             this split originates.
  *
  */
case class Split(
    modification: Modification,
    cost: Int,
    tag: SplitTag = SplitTag.Active,
    indents: Vector[Indent[Length]] = Vector.empty[Indent[Length]],
    policy: Policy = NoPolicy,
    optimalAt: Option[OptimalToken] = None
)(implicit val line: sourcecode.Line) {
  import TokenOps._

  def adapt(formatToken: FormatToken): Split = modification match {
    case n: NewlineT if !n.noIndent && rhsIsCommentedOut(formatToken) =>
      copy(modification = NewlineT(n.isDouble, noIndent = true))
    case _ => this
  }

  val indentation = indents
    .map(_.length match {
      case Num(x) => x.toString
      case x => x.toString
    })
    .mkString("[", ", ", "]")

  def length: Int = modification match {
    case m if m.isNewline => 0
    case NoSplit => 0
    case Space => 1
    case Provided(code) =>
      val firstLine = code.indexOf("\n")
      if (firstLine == -1) code.length
      else firstLine
  }

  @inline
  def isIgnored: Boolean = isTaggedFor(SplitTag.Ignored)

  @inline
  def isActive: Boolean = isTaggedFor(SplitTag.Active)

  @inline
  def isTaggedFor(tag: SplitTag): Boolean = this.tag == tag

  @inline
  def notIf(flag: Boolean): Split = onlyIf(!flag)

  def onlyIf(flag: Boolean): Split =
    if (flag || isIgnored) this else copy(tag = SplitTag.Ignored)

  def onlyFor(tag: SplitTag): Split =
    if (isIgnored) this
    else if (isActive) copy(tag = tag)
    else throw new UnsupportedOperationException("Multiple tags unsupported")

  def withOptimalTokenOpt(
      token: => Option[Token],
      killOnFail: Boolean = false
  ): Split =
    withOptimalAt(token.map(OptimalToken(_, killOnFail)))

  def withOptimalToken(token: => Token, killOnFail: Boolean = false): Split =
    withOptimalAt(Some(OptimalToken(token, killOnFail)))

  def withOptimalAt(optimalAt: => Option[OptimalToken]): Split = {
    require(this.optimalAt.isEmpty)
    if (isIgnored) this else copy(optimalAt = optimalAt)
  }

  def withPolicy(newPolicy: => Policy): Split = {
    if (policy != NoPolicy)
      throw new UnsupportedOperationException("Can't have two policies yet.")
    if (isIgnored) this else copy(policy = newPolicy)
  }

  def withSingleLine(
      expire: Token,
      killOnFail: Boolean = false
  )(implicit line: sourcecode.Line): Split =
    withSingleLineAndOptimal(expire, expire, killOnFail)

  def withSingleLineAndOptimal(
      expire: Token,
      optimal: Token,
      killOnFail: Boolean = false
  )(implicit line: sourcecode.Line): Split =
    withOptimalToken(optimal, killOnFail).withPolicy(SingleLineBlock(expire))

  def withPolicyOpt(newPolicy: => Option[Policy]): Split =
    if (isIgnored) this else newPolicy.fold(this)(withPolicy(_))

  def orElsePolicy(newPolicy: Policy): Split =
    if (isIgnored || newPolicy.isEmpty) this
    else if (policy.isEmpty) copy(policy = newPolicy)
    else copy(policy = policy.orElse(newPolicy))

  def andThenPolicy(newPolicy: Policy): Split =
    if (isIgnored || newPolicy.isEmpty) this
    else if (policy.isEmpty) copy(policy = newPolicy)
    else copy(policy = policy.andThen(newPolicy))

  def withPenalty(penalty: Int): Split =
    if (isIgnored) this else copy(cost = cost + penalty)

  def withIndent(length: => Length, expire: => Token, when: ExpiresOn): Split =
    if (isIgnored) this
    else
      length match {
        case Num(0) => this
        case x => copy(indents = Indent(x, expire, when) +: indents)
      }

  def withIndent(indent: Indent[Length]): Split =
    withIndent(indent.length, indent.expire, indent.expiresAt)

  def withIndents(indents: Seq[Indent[Length]]): Split =
    indents.foldLeft(this)(_ withIndent _)

  override def toString = {
    val prefix = tag match {
      case SplitTag.Ignored => "!"
      case SplitTag.Active => ""
      case _ => s"[$tag]"
    }
    s"""$prefix$modification:${line.value}(cost=$cost, indents=$indentation, $policy)"""
  }
}

object Split {

  def ignored(implicit line: sourcecode.Line) =
    Split(NoSplit, 0, tag = SplitTag.Ignored)

}
