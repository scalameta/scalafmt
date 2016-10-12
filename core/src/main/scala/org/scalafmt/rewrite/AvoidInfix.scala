package org.scalafmt.rewrite

import scala.meta.Importee
import scala.meta.Tree
import scala.meta._
import scala.meta.tokens.Token.LF
import scala.meta.tokens.Token.LeftBrace
import scala.meta.tokens.Token.LeftParen

import org.scalafmt.util.RegexOps
import org.scalafmt.util.TokenOps
import org.scalafmt.util.logger

case object AvoidInfix extends Rewrite {
  override def rewrite(code: Tree, ctx: RewriteCtx): Seq[Patch] = {
    val matcher = ctx.style.rewrite.neverInfix.toMatcher
    code.collect {
      case infix: Term.ApplyInfix if matcher.matches(infix.op.value) =>
        val rhs = infix.args.last.tokens
        val last = rhs.last
        val head = rhs.head
        val (open, close) =
          if (head.is[LeftParen] ||
              head.is[LeftBrace]) "" -> ""
          else "(" -> ")"
        val op = infix.op.tokens.head
        val opNewline = {
          val next = ctx.tokenTraverser.nextToken(op)
          if (next.is[LF]) next
          else op
        }
        Seq(
          Patch(op, opNewline, s".${op.syntax}$open"),
          Patch(last, last, s"${last.syntax}$close")
        )
    }.flatten
  }
}
