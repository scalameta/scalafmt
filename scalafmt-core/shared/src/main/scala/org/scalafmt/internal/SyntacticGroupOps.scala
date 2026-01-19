package org.scalafmt.internal

import org.scalafmt.internal.{SyntacticGroup => g}

import scala.meta.internal.trees._
import scala.meta.{Lit, Term, Tree}

import scala.annotation.tailrec

object SyntacticGroupOps {

  def operatorNeedsParenthesis(
      outerOp: String,
      innerOp: String,
      side: Side,
      what: AnyRef,
  ): Boolean = {
    val outerIsLeftAssoc = outerOp.isLeftAssoc
    if (outerIsLeftAssoc != innerOp.isLeftAssoc) true
    else if (what ne g.Type) {
      val diffPrecedence = outerOp.precedence - innerOp.precedence
      if (diffPrecedence < 0) !outerIsLeftAssoc // TODO: not needed
      else if (diffPrecedence > 0) // TODO: is it optional for patterns?
        outerIsLeftAssoc || (what eq g.Term)
      else outerIsLeftAssoc != side.isLeft
    } else outerIsLeftAssoc != side.isLeft
  }

  @tailrec
  def startsWithNumericLiteral(tree: Tree): Boolean = tree match {
    case _: Lit.Int | _: Lit.Long | _: Lit.Double | _: Lit.Float | _: Lit.Byte |
        _: Lit.Short => true
    case t: Term.Select => startsWithNumericLiteral(t.qual)
    case _ => false
  }

  def groupNeedsParenthesis(
      outerGroup: SyntacticGroup,
      innerGroup: SyntacticGroup,
      side: Side,
  ): Boolean = (outerGroup, innerGroup) match {
    case (g.Term.InfixExpr(outerOp), g.Term.InfixExpr(innerOp)) =>
      operatorNeedsParenthesis(outerOp, innerOp, side, what = g.Term)

    case (g.Type.InfixTyp(outerOp), g.Type.InfixTyp(innerOp)) =>
      operatorNeedsParenthesis(outerOp, innerOp, side, what = g.Type)

    case (g.Pat.Pattern3(outerOp), g.Pat.Pattern3(innerOp)) =>
      operatorNeedsParenthesis(outerOp, innerOp, side, what = g.Pat)

    case (_: g.Term.PrefixExpr, g.Term.PrefixArg(_, _: g.Term.PrefixExpr)) =>
      true

    case (g.Term.PrefixExpr("-"), g.Term.PrefixArg(Term.Select(tree, _), _))
        if startsWithNumericLiteral(tree) => true

    case _ => outerGroup.precedence > innerGroup.precedence
  }

}
