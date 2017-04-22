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

        val fstArgsToken = args.headOption.flatMap(_.tokens.headOption)
        val lastArgsToken = args.lastOption.flatMap(_.tokens.lastOption)
        val fstIsNotLeftParenAndBrace = fstArgsToken
          .exists(_.isNot[LeftParen]) && fstArgsToken
          .exists(_.isNot[LeftBrace])
        val lastIsNotRightParenAndBrace = lastArgsToken
          .exists(_.isNot[RightParen]) && lastArgsToken
          .exists(_.isNot[RightBrace])
        val isSingleArg = args.size == 1
        val selectorParensToBeAdded =
          if (isSingleArg && (fstIsNotLeftParenAndBrace || lastIsNotRightParenAndBrace)) {
            val selectorParens = for {
              fstToken <- fstArgsToken
              lastToken <- lastArgsToken
            } yield
              Seq(TokenPatch.AddLeft(fstToken, "(", keepTok = true),
                  TokenPatch.AddRight(lastToken, ")", keepTok = true))
            selectorParens.getOrElse(Seq.empty)
          } else
            Nil

        val lhsParensToBeAdded = lhs match {
          case Term.ApplyInfix(lhs1, op1, _, _)
              if !matcher.matches(op1.value)
                && lhs.tokens.head.isNot[LeftParen] =>
            Seq(TokenPatch.AddLeft(lhs.tokens.head, "(", keepTok = true),
                TokenPatch.AddRight(lhs.tokens.last, ")", keepTok = true))
          case _ => Nil
        }

        val toBeRemoved = fstArgsToken.fold(Seq.empty[TokenPatch])(
          token =>
            ctx.tokenTraverser
              .filter(fstOpToken, token)(_.is[LF])
              .map(TokenPatch.Remove))

        val hasSingleLineComment = fstArgsToken.exists(
          token =>
            ctx.tokenTraverser
              .filter(fstOpToken, token)(TokenOps.isSingleLineComment)
              .nonEmpty)

        if (hasSingleLineComment)
          Nil
        else
          lhsParensToBeAdded ++ selectorToBeAdded ++ selectorParensToBeAdded ++ toBeRemoved
    }.flatten
  }
}
