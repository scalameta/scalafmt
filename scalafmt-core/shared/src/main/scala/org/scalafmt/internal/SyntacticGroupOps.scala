package org.scalafmt.internal

import org.scalafmt.internal.{SyntacticGroup => g}

import scala.meta.Dialect
import scala.meta.internal.trees._

object SyntacticGroupOps {

  def operatorNeedsParenthesis(
      outerOp: String,
      innerOp: String,
      side: Side,
      what: AnyRef,
  )(implicit dialect: Dialect): Boolean = {
    val outerIsLeftAssoc = outerOp.isLeftAssoc
    if (outerIsLeftAssoc != innerOp.isLeftAssoc) true
    else if ((what ne g.Type) || dialect.useInfixTypePrecedence) {
      val diffPrecedence = outerOp.precedence - innerOp.precedence
      diffPrecedence > 0 ||
      diffPrecedence == 0 && outerIsLeftAssoc != side.isLeft
    } else outerIsLeftAssoc != side.isLeft
  }

  def groupNeedsParenthesis(
      outerGroup: SyntacticGroup,
      innerGroup: SyntacticGroup,
      side: Side,
  )(implicit dialect: Dialect): Boolean = (outerGroup, innerGroup) match {
    case (g.Term.InfixExpr(outerOp), g.Term.InfixExpr(innerOp)) =>
      operatorNeedsParenthesis(outerOp, innerOp, side, what = g.Term)

    case (g.Type.InfixTyp(outerOp), g.Type.InfixTyp(innerOp)) =>
      operatorNeedsParenthesis(outerOp, innerOp, side, what = g.Type)

    case (g.Pat.Pattern3(outerOp), g.Pat.Pattern3(innerOp)) =>
      operatorNeedsParenthesis(outerOp, innerOp, side, what = g.Pat)

    case _ => outerGroup.precedence > innerGroup.precedence
  }

}
