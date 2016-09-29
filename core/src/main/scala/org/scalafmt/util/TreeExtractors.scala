package org.scalafmt.util

import scala.collection.immutable.Seq
import scala.meta.Pat
import scala.meta.Term
import scala.meta.Tree

object InfixApplication {
  def unapply(tree: Tree): Option[(Tree, Term.Name, Seq[Tree])] = tree match {
    case infix: Term.ApplyInfix => Some((infix.lhs, infix.op, infix.args))
    case infix: Pat.ExtractInfix => Some((infix.lhs, infix.ref, infix.rhs))
    case _ => None
  }
}
