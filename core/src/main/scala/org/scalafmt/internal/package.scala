package org.scalafmt

import scala.annotation.tailrec
import scala.meta.Tree
import scala.meta.tokens.Token

package object internal {
  val NoPolicy = Policy.empty

  // TODO(olafur) Move these elsewhere.

  @tailrec
  final def childOf(child: Tree, tree: Tree): Boolean = {
    child == tree ||
    (child.parent match {
          case Some(parent) => childOf(parent, tree)
          case _ => false
        })
  }

  def childOf(tok: Token, tree: Tree, owners: Map[TokenHash, Tree]): Boolean =
    childOf(owners(hash(tok)), tree)

  /**
    * For convenience when experimenting with different hashing strategies.
    */
  type TokenHash = Long

  /**
    * Custom hash code for token.
    *
    * The default hashCode is slow because it prevents conflicts between
    * tokens from different source files. We only care about getting a unique
    * identifier for the token inside this source file.
    *
    * The hash code works like this this:
    * Top 8 bits go to privateTag, a unique identifier for the tokens class.
    * Next 28 bits go to the tokens **start** offset byte.
    * Final 28 bits go to the tokens **end** offset byte.
    *
    * The only chance for collision is if two empty length tokens with the same
    * type lie next to each other. @xeno-by said this should not happen.
    */
  @inline
  def hash(token: Token): TokenHash = {
    val longHash: Long =
      (token.privateTag.toLong << (62 - 8)) |
      (token.start.toLong << (62 - (8 + 28))) | token.end
    longHash
  }

  @tailrec
  final def parents(
      tree: Tree, accum: Seq[Tree] = Seq.empty[Tree]): Seq[Tree] = {
    tree.parent match {
      case Some(parent) => parents(parent, parent +: accum)
      case _ => accum
    }
  }
}
