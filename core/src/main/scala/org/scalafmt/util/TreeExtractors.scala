package org.scalafmt.util

import scala.collection.immutable.Seq
import scala.meta.Name
import scala.meta.Pat
import scala.meta.Term
import scala.meta.Tree
import scala.meta.Type

object InfixApplication {
  def unapply(tree: Tree): Option[(Tree, Name, Seq[Tree])] = tree match {
    case infix: Type.ApplyInfix => Some((infix.lhs, infix.op, infix.children))
    case infix: Term.ApplyInfix => Some((infix.lhs, infix.op, infix.args))
    case infix: Pat.ExtractInfix => Some((infix.lhs, infix.ref, infix.rhs))
    case _ => None
  }
}
