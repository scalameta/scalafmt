package org.scalafmt.rewrite

import org.scalafmt.util.TokenOps

import scala.meta.tokens.Token.{
  LF,
  LeftBrace,
  LeftParen,
  RightBrace,
  RightParen
}
import scala.meta.{Tree, _}

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
      case Term.ApplyInfix(lhs, op, _, args) if matcher.matches(op.value) =>
        val fstOpToken = op.tokens.head

        val selectorToBeAdded = Seq(
          TokenPatch.AddLeft(fstOpToken, ".", keepTok = true)
        )

        val fstArgsToken = args.head.tokens.head
        val lastArgsToken = args.last.tokens.last
        val fstIsNotLeftParenAndBrace = fstArgsToken
          .isNot[LeftParen] && fstArgsToken
          .isNot[LeftBrace]
        val lastIsNotRightParenAndBrace = lastArgsToken
          .isNot[RightParen] && lastArgsToken
          .isNot[RightBrace]
        val isSingleArg = args.size == 1
        val selectorParensToBeAdded =
          if (isSingleArg && (fstIsNotLeftParenAndBrace || lastIsNotRightParenAndBrace))
            Seq(TokenPatch.AddLeft(fstArgsToken, "(", keepTok = true),
                TokenPatch.AddRight(lastArgsToken, ")", keepTok = true))
          else
            Nil

        val lhsParensToBeAdded = lhs match {
          case Term.ApplyInfix(lhs1, op1, _, _)
              if !matcher.matches(op1.value)
                && lhs.tokens.head.isNot[LeftParen] =>
            Seq(TokenPatch.AddLeft(lhs.tokens.head, "(", keepTok = true),
                TokenPatch.AddRight(lhs.tokens.last, ")", keepTok = true))
          case _ => Nil
        }

        val toBeRemoved = ctx.tokenTraverser
          .filter(fstOpToken, fstArgsToken)(_.is[LF])
          .map(TokenPatch.Remove)

        val hasSingleLineComment = ctx.tokenTraverser
          .filter(fstOpToken, fstArgsToken)(TokenOps.isSingleLineComment)
          .nonEmpty

        if (hasSingleLineComment)
          Nil
        else
          lhsParensToBeAdded ++ selectorToBeAdded ++ selectorParensToBeAdded ++ toBeRemoved
    }.flatten
  }
}
