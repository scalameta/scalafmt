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
    ignoreIf: Boolean = false,
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

  def onlyIf(flag: Boolean): Split =
    if (ignoreIf || flag) this else copy(ignoreIf = true)

  def withOptimalToken(token: Option[Token]): Split = token match {
    case Some(token) => withOptimalToken(token)
    case _ => this
  }

  def withOptimalToken(token: Token, killOnFail: Boolean = false): Split = {
    require(optimalAt.isEmpty)
    copy(optimalAt = Some(OptimalToken(token, killOnFail)))
  }

  def withPolicy(newPolicy: Policy): Split = {
    if (policy != NoPolicy)
      throw new UnsupportedOperationException("Can't have two policies yet.")
    copy(policy = newPolicy)
  }

  def withPolicy(newPolicy: Option[Policy]): Split =
    newPolicy.fold(this)(withPolicy)

  def orElsePolicy(newPolicy: Policy): Split =
    if (newPolicy.isEmpty) this
    else if (policy.isEmpty) copy(policy = newPolicy)
    else copy(policy = policy.orElse(newPolicy))

  def andThenPolicy(newPolicy: Policy): Split =
    if (newPolicy.isEmpty) this
    else if (policy.isEmpty) copy(policy = newPolicy)
    else copy(policy = policy.andThen(newPolicy))

  def withPenalty(penalty: Int): Split =
    copy(cost = cost + penalty)

  def withIndent(length: Length, expire: Token, expiresOn: ExpiresOn): Split =
    length match {
      case Num(0) => this
      case _ => copy(indents = Indent(length, expire, expiresOn) +: indents)
    }

  def sameSplit(other: Split): Boolean =
    this.modification == other.modification &&
      this.line.value == other.line.value && this.cost == other.cost

  override def toString = {
    val prefix = if (ignoreIf) "!" else ""
    s"""$prefix$modification:${line.value}(cost=$cost, indents=$indentation, $policy)"""
  }
}

object Split {

  def ignored(implicit line: sourcecode.Line) =
    Split(NoSplit, 0, ignoreIf = true)

}
