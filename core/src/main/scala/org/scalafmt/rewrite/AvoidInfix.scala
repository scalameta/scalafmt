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
  // In a perfect world, we could just use
  // Tree.transform {
  //   case t: Term.ApplyInfix => Term.Apply(Term.Select(t.lhs, t.op), t.args)
  // }
  // and be done with it. However, until transform becomes token aware (see https://github.com/scalameta/scalameta/pull/457)
  // we will do these dangerous rewritings by hand.
  override def rewrite(code: Tree, ctx: RewriteCtx): Seq[Patch] = {
    val matcher = ctx.style.rewrite.neverInfix.toMatcher
    code.collect {
      case infix: Term.ApplyInfix
          if matcher.matches(infix.op.value) &&
            !infix.lhs.is[Term.ApplyInfix] =>
        val last = infix.args.last.tokens.last
        val head = infix.args.head.tokens.head
        val singleArgument = infix.args.length == 1
        val needsParens =
          singleArgument && // infix application owns parens if !singleArgument
            !head.is[LeftParen] &&
            !head.is[LeftBrace]
        val (open, close) = if (needsParens) "(" -> ")" else "" -> ""
        val op = infix.op.tokens.head
        /* We skip infix operators with crazy comments like this:
            foo infixOp // crazy comment
              (argument)
         */
        val hasCrazyComment =
          ctx.tokenTraverser
            .filter(op, head)(x => TokenOps.isInlineComment(x))
            .nonEmpty
        if (hasCrazyComment) Nil
        else {
          val removeNewlines =
            ctx.tokenTraverser
              .filter(op, head)(x => x.is[LF])
              .map(lf => Patch(lf, lf, ""))
          val rewriteInfixIntoTermApplication =
            Seq(
              Patch(op, op, s".${op.syntax}$open"),
              Patch(last, last, s"${last.syntax}$close")
            )
          removeNewlines ++ rewriteInfixIntoTermApplication
        }
    }.flatten
  }
}
