package org.scalafmt.internal

import scala.meta.tokens.Token
import org.scalafmt.internal.Length.Num
import org.scalafmt.internal.Policy.NoPolicy
import org.scalafmt.util.TokenOps
import org.scalafmt.util.TokenOps.rhsIsCommentedOut

import scala.meta.tokens.Token.Ident

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
case class Split(
    modification: Modification,
    cost: Int,
    ignoreIf: Boolean = false,
    indents: Seq[Indent[Length]] = Array.empty[Indent[Length]],
    policy: Policy = NoPolicy,
    optimalAt: Option[OptimalToken] = None)(
  implicit val line: sourcecode.Line) {
  import TokenOps._
  //def penalty: Boolean = policy != NoPolicy || optimalAt.isDefined ||addedPenalty

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
      Some(OptimalToken(token, killOnFail)))(line)
  }

  def withPolicy(newPolicy: Policy): Split = {
    val update =
      if (policy == NoPolicy) newPolicy
      else
        throw new UnsupportedOperationException("Can't have two policies yet.")
    new Split(modification, cost, ignoreIf, indents, update, optimalAt)(
      line)
  }

  def withPenalty(penalty: Int): Split =
    new Split(
      modification,
      cost + penalty,
      ignoreIf,
      indents,
      policy,
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
          optimalAt)(line)
    }
  }

  def sameSplit(other: Split): Boolean =
    this.modification == other.modification &&
      this.line.value == other.line.value && this.cost == other.cost

  override def toString =
    s"""$modification:${line.value}(cost=$cost, indents=$indentation, $policy)"""
}

object Split {

  final def withIndent(
             modification: Modification,
             cost: Int,
             ignoreIf: Boolean = false,
             indent: Indent[Length],
             policy: Policy = NoPolicy
  )(implicit line: sourcecode.Line) ={
    val indents = indent.length match {
      case Num(0) => emptyIndent
      case _ => Array(indent)
    }
    new Split(
      modification,
      cost,
      ignoreIf,
      indents,
      policy)(line)
  }

  val emptyIndent = Array[Indent[Length]]()

  final def withIdentAndOptionToken(
             modification: Modification,
             cost: Int,
             indent: Indent[Length],
             token: Option[Token],
             policy: Policy = NoPolicy,
             ignoreIf: Boolean = false
           )(implicit line: sourcecode.Line) = {
    val indents = indent.length match {
      case Num(0) => emptyIndent
      case _ => Array(indent)
    }

    val optimaltoken = token.map(OptimalToken(_))

    new Split(
      modification,
      cost,
      ignoreIf,
      indents,
      policy,
      optimaltoken)(line)
  }

  @inline
  final def withToken(
             modification: Modification,
             cost: Int,
             token: Token,
             ignoreIf: Boolean = false,
             indents: Seq[Indent[Length]] = emptyIndent,
             policy: Policy = NoPolicy,
             killOnFail: Boolean = false
           )(implicit line: sourcecode.Line) = new Split(
    modification,
    cost,
    ignoreIf,
    indents,
    policy,
    Some(OptimalToken(token, killOnFail))
  )(line)

  @inline
  final def withIdentAndToken(
             modification: Modification,
             cost: Int,
             indent: Indent[Length],
             token: Token,
             ignoreIf: Boolean = false,
             policy: Policy = NoPolicy,
             killOnFail: Boolean = false
           )(implicit line: sourcecode.Line) = new Split(
    modification,
    cost,
    ignoreIf,
    Array(indent),
    policy,
    Some(OptimalToken(token, killOnFail))
  )(line)
}