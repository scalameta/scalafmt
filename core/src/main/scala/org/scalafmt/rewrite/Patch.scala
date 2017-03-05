package org.scalafmt.rewrite

import org.scalafmt.rewrite.TokenPatch.{Add, Remove}

import scala.meta._
import scala.meta.tokens.Token

sealed abstract class Patch
abstract class TreePatch extends Patch
abstract class TokenPatch(val tok: Token, val newTok: String) extends TreePatch

object TokenPatch {
  case class Remove(override val tok: Token) extends TokenPatch(tok, "")
  def AddRight(tok: Token,
               toAdd: String,
               keepTok: Boolean = false): TokenPatch =
    Add(tok, "", toAdd, keepTok)
  def AddLeft(tok: Token,
              toAdd: String,
              keepTok: Boolean = false): TokenPatch =
    Add(tok, toAdd, "", keepTok)
  case class Add(override val tok: Token,
                 addLeft: String,
                 addRight: String,
                 keepTok: Boolean)
      extends TokenPatch(tok,
                         s"""$addLeft${if (keepTok) tok else ""}$addRight""")

}

object Patch {
  def merge(a: TokenPatch, b: TokenPatch): TokenPatch = (a, b) match {
    case (add1: Add, add2: Add) =>
      Add(add1.tok,
          add1.addLeft + add2.addLeft,
          add1.addRight + add2.addRight,
          add1.keepTok && add2.keepTok)
    case (_: Remove, add: Add) => add.copy(keepTok = false)
    case (add: Add, _: Remove) => add.copy(keepTok = false)
    case (rem: Remove, _: Remove) => rem
    case _ =>
      sys.error(s"""Can't merge token patches:
                   |1. $a
                   |2. $b""".stripMargin)
  }

  def apply(ast: Tree, patches: Seq[Patch])(implicit ctx: RewriteCtx): String = {
    val input = ast.tokens
    val tokenPatches = patches.collect { case e: TokenPatch => e }
    val patchMap: Map[(Int, Int), String] =
      (tokenPatches)
        .groupBy(t => t.tok.start -> t.tok.end)
        .mapValues(_.reduce(merge).newTok)
    input.toIterator
      .map(x => patchMap.getOrElse(x.start -> x.end, x.syntax))
      .mkString
  }
}
