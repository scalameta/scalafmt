package org

import scala.annotation.tailrec
import scala.meta.Tree
import scala.meta.tokens.Token

package object scalafmt {
  type Policy = PartialFunction[Decision, Decision]
  val NoPolicy = PartialFunction.empty[Decision, Decision]
  val IdentityPolicy: PartialFunction[Decision, Decision] = { case d => d }

  // TODO(olafur) Move these elsewhere.

  @tailrec
  final def childOf(child: Tree, tree: Tree): Boolean = {
    child == tree || (child.parent match {
        case Some(parent) => childOf(parent, tree)
        case _ => false
      })
  }

  def childOf(tok: Token, tree: Tree, owners: Map[Token, Tree]): Boolean =
    childOf(owners(tok), tree)

  // TODO(olafur) Move these elsewhere.

  @tailrec
  final def parents(tree: Tree, accum: Seq[Tree] = Seq.empty[Tree]): Seq[Tree] = {
    tree.parent match {
      case Some(parent) => parents(parent, parent +: accum)
      case _ => accum
    }
  }
}