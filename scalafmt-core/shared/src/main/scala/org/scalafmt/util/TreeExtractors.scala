package org.scalafmt.util

import scala.collection.immutable.Seq
import scala.meta.Defn
import scala.meta.Name
import scala.meta.Pat
import scala.meta.Template
import scala.meta.Term
import scala.meta.Tree
import scala.meta.Type

object InfixApplication {
  def unapply(tree: Tree): Option[(Tree, Name, Seq[Tree])] = tree match {
    case infix: Type.ApplyInfix => Some((infix.lhs, infix.op, infix.children))
    case infix: Term.ApplyInfix => Some((infix.lhs, infix.op, infix.args))
    case infix: Pat.ExtractInfix => Some((infix.lhs, infix.op, infix.rhs))
    case _ => None
  }
}

/**
  * Pattern extractor to concisely and safely query parent chain.
  *
  * Example:
  * {{{
  *   tree match {
  *     case (_: Term) `:parent:` (_: Defn) `:parent:` (_: Source) => ???
  *   }
  * }}}
  * The name is backquoted to get right associativity while using
  * alphabetic characters
  */
object `:parent:` {
  def unapply(tree: Tree): Option[(Tree, Tree)] = tree.parent match {
    case Some(parent) =>
      Some(tree -> parent)
    case _ => None
  }
}

object SelfAnnotation {
  def unapply(t: Type.With): Option[Type.With] =
    TreeOps.topTypeWith(t) match {
      case (top: Type.With) `:parent:` (_: Term.Param) `:parent:` (_: Template) =>
        Some(top)
      case (top: Type.With) `:parent:` (_: Defn.Type) =>
        Some(top)
      case _ => None
    }
}
