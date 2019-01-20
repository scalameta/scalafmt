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
  * @param penalty Does this split overflow the column limit?
  * @param line For debugging, to retrace from which case in [[Router]]
  *             this split originates.
  *
  */

object Split{
  final val emptyArray = Array[Indent[Length]]()
}

case class Split(
    modification: Modification,
    cost: Int,
    ignoreIf: Boolean = false,
    indents: Seq[Indent[Length]] = Split.emptyArray,
    policy: Policy = NoPolicy,
    penalty: Boolean = false,
    optimalAt: Option[OptimalToken] = None)(
    implicit val line: sourcecode.Line) {
  import TokenOps._

  def adapt(formatToken: FormatToken): Split = modification match {
    case n: NewlineT if !n.noIndent && rhsIsCommentedOut(formatToken) =>
      copy(modification = NewlineT(n.isDouble, noIndent = true))
    case _ => this
  }

  def indentation = indents
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

  def withOptimalToken(token: Option[Token]): Split = token match {
    case Some(token) => withOptimalToken(token)
    case _ => this
  }

  def withOptimalToken(token: Token, killOnFail: Boolean = false): Split = {
    require(optimalAt.isEmpty)
    new Split(
      modification,
      cost,
      ignoreIf,
      indents,
      policy,
      true,
      Some(OptimalToken(token, killOnFail)))(line)
  }

  def withPolicy(newPolicy: Policy): Split = {
    val update =
      if (policy == NoPolicy) newPolicy
      else
        throw new UnsupportedOperationException("Can't have two policies yet.")
    new Split(modification, cost, ignoreIf, indents, update, true, optimalAt)(
      line)
  }

  def withPenalty(penalty: Int): Split =
    new Split(
      modification,
      cost + penalty,
      ignoreIf,
      indents,
      policy,
      true,
      optimalAt)(line)

  def withIndent(length: Length, expire: Token, expiresOn: ExpiresOn): Split = {
    length match {
      case Num(0) => this
      case _ =>
        new Split(
          modification,
          cost,
          ignoreIf,
          Indent(length, expire, expiresOn) +: indents,
          policy,
          penalty,
          optimalAt)(line)
    }
  }

  def sameSplit(other: Split): Boolean =
    this.modification == other.modification &&
      this.line.value == other.line.value && this.cost == other.cost

  override def toString =
    s"""$modification:${line.value}(cost=$cost, indents=$indentation, $policy)"""
}
