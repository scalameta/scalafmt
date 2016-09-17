package org.scalafmt.rewrite

import scala.meta.Importee
import scala.meta.Tree
import scala.meta._
import scala.meta.tokens.Token.Equals
import scala.meta.tokens.Token.LF
import scala.meta.tokens.Token.LeftBrace
import scala.meta.tokens.Token.RightBrace
import scala.meta.tokens.Token.Space

import org.scalafmt.util.TreeOps._

import org.scalafmt.util.logger

/**
  * Removes/adds curly braces where desired.
  */
object RedundantBraces extends Rewrite {

  def isCandidate(d: Defn.Def, ctx: RewriteCtx): Boolean = {
    import ctx.style.rewrite.{redundantBraces => settings}
    def isBlock = d.body match {
      case t: Term.Block if t.stats.length == 1 =>
        !t.stats.head.is[Term.Block] && // Nested blocks are trickier
          d.body.tokens.head.is[LeftBrace] &&
          d.body.tokens.last.is[RightBrace]
      case _ => false
    }

    def disqualifiedByUnit =
      !settings.includeUnitMethods || d.decltpe.exists(_.syntax == "Unit")

    def bodyIsNotTooBig: Boolean =
      d.body match {
        case t: Term.Block =>
          val stat = t.stats.head
          val diff =
            stat.tokens.last.pos.end.line -
              stat.tokens.head.pos.start.line
          diff < settings.maxLines
        case _ => false
      }

    !isProcedureSyntax(d) &&
    isBlock &&
    !disqualifiedByUnit &&
    bodyIsNotTooBig
  }

  override def rewrite(code: Tree, ctx: RewriteCtx): Seq[Patch] = {
    import ctx.tokenTraverser._
    code.collect {
      case d: Defn.Def if isCandidate(d, ctx) =>
        val open = d.body.tokens.head
        val close = d.body.tokens.last
        val firstNewline = d.body.tokens.tail
          .find(!_.is[Space])
          .filter(_.is[LF])
          .getOrElse(open)
        val lastNewline = {
          val next = nextToken(close)
          if (next.is[LF]) next
          else close
        }
        Seq(
          Patch(open, firstNewline, ""),
          Patch(close, lastNewline, "")
        )

    }.flatten
  }
}
