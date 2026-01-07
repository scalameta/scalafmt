package org.scalafmt.util

import scala.meta._
import scala.meta.internal.trees._

object InfixApp {

  implicit class XtensionInfix(private val tree: Member.Infix) extends AnyVal {
    @inline
    def precedence: Int = InfixApp.getPrecedence(tree.op.value)

    def singleArg: Option[Tree] = tree.arg match {
      case x: Member.ArgClause => TreeOps.getSingleElement(x.values)
      case x => Some(x)
    }

    def args: Seq[Tree] = tree.arg match {
      case Member.ArgClause(v) => v
      case arg => arg :: Nil
    }

    def nestedInfixApps: Seq[Member.Infix] = (tree.lhs :: singleArg.toList)
      .collect { case x: Member.Infix => x }

  }

  def unapply(tree: Tree): Option[Member.Infix] = tree match {
    case t: Member.Infix => Some(t)
    case _ => None
  }

  // https://scala-lang.org/files/archive/spec/2.11/06-expressions.html#infix-operations
  // Operators ending in a colon `:' are right-associative. All other operators are left-associative.
  @inline
  def isLeftAssoc(op: String): Boolean = op.isLeftAssoc

  @inline
  def getPrecedence(op: String): Int = {
    val idx = op.lastIndexWhere(_ != '=')
    if (idx < 0) op else op.substring(0, idx + 1)
  }.precedence

}

object WithChain {
  def unapply(t: Type.With): Option[Type.With] = {
    // self types, params, val/def/var/type definitions or declarations
    val top = TreeOps.topTypeWith(t)
    top.parent.collect {
      case _: Defn | _: Decl | _: Term.Param | _: Self | _: Type.ArgClause =>
        top
    }
  }
}

object ParamClauseParent {
  def unapply(t: Member.ParamClause): Option[Tree] = t.parent match {
    case Some(p: Member.ParamClauseGroup) => p.parent
    case p => p
  }
}
