package org.scalafmt.rewrite

import org.scalafmt.rewrite.TokenPatch.{Add, Remove}

import scala.meta._
import scala.meta.tokens.Token

sealed abstract class Patch
abstract class TreePatch extends Patch
abstract class TokenPatch(val tok: Token, val newTok: String) extends TreePatch

object TokenPatch {
  case class Remove(override val tok: Token) extends TokenPatch(tok, "")
  case class Replace(override val tok: Token, override val newTok: String)
      extends TokenPatch(tok, newTok)
  def AddRight(
      tok: Token,
      toAdd: String,
      keepTok: Boolean = false
  ): TokenPatch =
    Add(tok, "", toAdd, keepTok)
  def AddLeft(tok: Token, toAdd: String, keepTok: Boolean = false): TokenPatch =
    Add(tok, toAdd, "", keepTok)
  case class Add(
      override val tok: Token,
      addLeft: String,
      addRight: String,
      keepTok: Boolean
  ) extends TokenPatch(
        tok,
        s"""$addLeft${if (keepTok) tok else ""}$addRight"""
      )

}

object Patch {
  def merge(a: TokenPatch, b: TokenPatch): TokenPatch =
    (a, b) match {
      case (add1: Add, add2: Add) =>
        Add(
          add1.tok,
          add1.addLeft + add2.addLeft,
          add1.addRight + add2.addRight,
          add1.keepTok && add2.keepTok
        )
      case (_: Remove, add: Add) => add.copy(keepTok = false)
      case (add: Add, _: Remove) => add.copy(keepTok = false)
      case (rem: Remove, _: Remove) => rem
      case _ =>
        sys.error(s"""Can't merge token patches:
          |1. $a
          |2. $b""".stripMargin)
    }

}
