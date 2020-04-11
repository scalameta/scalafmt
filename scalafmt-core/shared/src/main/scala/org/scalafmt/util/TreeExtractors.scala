package org.scalafmt.util

import scala.collection.immutable.Seq
import scala.meta.Decl
import scala.meta.Defn
import scala.meta.Name
import scala.meta.Pat
import scala.meta.Self
import scala.meta.Term
import scala.meta.Tree
import scala.meta.Type

case class InfixApp(lhs: Tree, op: Name, rhs: Seq[Tree], all: Tree) {

  @inline
  def isAssignment: Boolean = InfixApp.isAssignment(op.value)

  // https://scala-lang.org/files/archive/spec/2.11/06-expressions.html#infix-operations
  // Operators ending in a colon `:' are right-associative. All other operators are left-associative.
  @inline
  def isRightAssoc: Boolean = op.value.last == ':'

  @inline
  lazy val precedence: Int =
    InfixApp.infixOpPrecedence.getOrElse(op.value.head, 0)

}

object InfixApp {

  def unapply(tree: Tree): Option[InfixApp] = tree match {
    case t: Type.ApplyInfix => Some(InfixApp(t.lhs, t.op, Seq(t.rhs), t))
    case t: Term.ApplyInfix => Some(InfixApp(t.lhs, t.op, t.args, t))
    case t: Pat.ExtractInfix => Some(InfixApp(t.lhs, t.op, t.rhs, t))
    case _ => None
  }

  // https://scala-lang.org/files/archive/spec/2.11/06-expressions.html#infix-operations
  private val infixOpPrecedence: Map[Char, Int] = {
    val builder = Map.newBuilder[Char, Int]
    def add(precedence: Int, ops: Char*): Unit = addSeq(precedence, ops)
    def addSeq(precedence: Int, ops: Iterable[Char]): Unit =
      ops.foreach(builder += _ -> precedence)
    // start with 1; all other special characters will get 0 by omission
    add(1, '*', '/', '%')
    add(2, '+', '-')
    add(3, ':')
    add(4, '<', '>')
    add(5, '=', '!')
    add(6, '&')
    add(7, '^')
    add(8, '|')
    addSeq(9, 'a' to 'z')
    addSeq(9, 'A' to 'Z')
    builder.result()
  }

  // https://scala-lang.org/files/archive/spec/2.11/06-expressions.html#assignment-operators
  private val nonAssignmentOperators = Set("<=", ">=", "!=")

  // https://scala-lang.org/files/archive/spec/2.11/06-expressions.html#infix-operations
  // The precedence of an assignment ... is lower than the precedence of any other ...
  def isAssignment(op: String): Boolean =
    op.lastOption.contains('=') && (op.length == 1 ||
      op.head != '=' && !nonAssignmentOperators.contains(op))

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

object WithChain {
  def unapply(t: Type.With): Option[Type.With] =
    TreeOps.topTypeWith(t) match {
      // self types, params, val/def/var/type definitions or declarations
      case (top: Type.With) `:parent:`
            (_: Defn | _: Decl | _: Term.Param | _: Self) =>
        Some(top)
      case _ =>
        None
    }
}
