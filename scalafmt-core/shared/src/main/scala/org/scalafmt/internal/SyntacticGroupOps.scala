package org.scalafmt.internal

import org.scalafmt.internal.{SyntacticGroup => g}
import org.scalafmt.util.InfixApp

import scala.annotation.tailrec
import scala.meta.{Lit, Term, Tree}
import scala.meta.internal.trees._

object SyntacticGroupOps {

  def operatorNeedsParenthesis(
      outerOperator: String,
      innerOperator: String,
      customAssociativity: Boolean,
      customPrecedence: Boolean,
      side: Side,
      forceRight: Boolean = false,
  ): Boolean = {

    def isLeftAssociative(name: String): Boolean = !customAssociativity ||
      InfixApp.isLeftAssoc(name)

    def precedence(name: String): Int =
      if (customPrecedence) Term.Name(name).precedence else 0

    val outerOperatorIsLeftAssociative = isLeftAssociative(outerOperator)
    val innerOperatorIsLeftAssociative = isLeftAssociative(innerOperator)

    if (outerOperatorIsLeftAssociative ^ innerOperatorIsLeftAssociative) true
    else {
      val isLeft = outerOperatorIsLeftAssociative
      val isRight = !outerOperatorIsLeftAssociative

      val outerOperatorPrecedence = precedence(outerOperator)
      val innerOperatorPrecedence = precedence(innerOperator)

      if (outerOperatorPrecedence < innerOperatorPrecedence) isRight
      else if (outerOperatorPrecedence == innerOperatorPrecedence) isLeft ^
        side.isLeft
      else isLeft || forceRight
    }
  }

  @tailrec
  def startsWithNumericLiteral(tree: Tree): Boolean = tree match {
    case _: Lit.Int | _: Lit.Long | _: Lit.Double | _: Lit.Float | _: Lit.Byte |
        _: Lit.Short => true
    case Term.Select(tree0, _) => startsWithNumericLiteral(tree0)
    case _ => false
  }

  def groupNeedsParenthesis(
      outerGroup: SyntacticGroup,
      innerGroup: SyntacticGroup,
      side: Side,
  ): Boolean = (outerGroup, innerGroup) match {
    case (g.Term.InfixExpr(outerOperator), g.Term.InfixExpr(innerOperator)) =>
      operatorNeedsParenthesis(
        outerOperator,
        innerOperator,
        customAssociativity = true,
        customPrecedence = true,
        side,
        forceRight = true,
      )
    case (g.Type.InfixTyp(outerOperator), g.Type.InfixTyp(innerOperator)) =>
      operatorNeedsParenthesis(
        outerOperator,
        innerOperator,
        customAssociativity = true,
        customPrecedence = false,
        side,
      )
    case (g.Pat.Pattern3(outerOperator), g.Pat.Pattern3(innerOperator)) =>
      operatorNeedsParenthesis(
        outerOperator,
        innerOperator,
        customAssociativity = true,
        customPrecedence = true,
        side,
      )

    case (_: g.Term.PrefixExpr, g.Term.PrefixArg(_, _: g.Term.PrefixExpr)) =>
      true

    case (g.Term.PrefixExpr("-"), g.Term.PrefixArg(Term.Select(tree, _), _))
        if startsWithNumericLiteral(tree) => true

    case _ => outerGroup.precedence > innerGroup.precedence
  }

}
