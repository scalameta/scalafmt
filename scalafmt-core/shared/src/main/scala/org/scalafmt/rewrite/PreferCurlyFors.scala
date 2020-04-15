package org.scalafmt.rewrite

import org.scalafmt.util.Whitespace

import scala.meta.tokens.Tokens
import scala.meta._

object PreferCurlyFors extends Rewrite {
  override def create(implicit ctx: RewriteCtx): RewriteSession =
    new PreferCurlyFors
}

/**
  * Replaces multi generator For / ForYield Expression parens and semi-colons
  * with braces and new-lines.
  *
  * For example,
  *
  *   for(a <- as; b <- bs if b > 2) yield (a, b)
  *
  * becomes,
  *
  *   for {
  *     a <- as
  *     b <- bs if b > 2
  *   } yield (a, b)
  *
  */
class PreferCurlyFors(implicit ctx: RewriteCtx) extends RewriteSession {

  import ctx.dialect
  import ctx.tokenTraverser._

  private def findForParens(forTokens: Tokens): Option[(Token, Token)] =
    for {
      forToken <- forTokens.find(_.is[Token.KwFor])
      leftParen <- findAfter(forToken) {
        case _: Token.LeftParen => Some(true)
        case Whitespace() => None
        case _ => Some(false)
      }
      rightParen <- ctx.getMatchingOpt(leftParen)
    } yield (leftParen, rightParen)

  private def findForSemiColons(forEnumerators: Seq[Enumerator]): Seq[Token] =
    for {
      enumerator <- forEnumerators
      token <-
        enumerator
          .tokens(ctx.style.runner.dialect)
          .headOption
          .toIterable
      semicolon <- findBefore(token) {
        case _: Token.Semicolon => Some(true)
        case Whitespace() => None
        case _ => Some(false)
      }.toIterable
    } yield semicolon

  private def rewriteFor(
      forTokens: Tokens,
      forEnumerators: Seq[Enumerator]
  ): Seq[TokenPatch] = {
    val builder = Seq.newBuilder[TokenPatch]

    findForParens(forTokens).foreach { parens =>
      val openBraceTokens =
        if (nextToken(parens._1).is[Token.LF]) "{" else "{\n"
      builder += TokenPatch.AddRight(parens._1, openBraceTokens)
      builder += TokenPatch.AddRight(parens._2, "}")
      findForSemiColons(forEnumerators).foreach { semiColon =>
        val semiColonReplacementTokens =
          if (nextToken(semiColon).is[Token.LF]) "" else "\n"
        builder += TokenPatch.AddRight(semiColon, semiColonReplacementTokens)
      }
    }

    builder.result()
  }

  private def hasMoreThanOneGenerator(
      forEnumerators: Seq[Enumerator]
  ): Boolean =
    forEnumerators.count(_.is[Enumerator.Generator]) > 1

  override def rewrite(tree: Tree): Unit =
    tree match {
      case fy: Term.ForYield if hasMoreThanOneGenerator(fy.enums) =>
        ctx.addPatchSet(rewriteFor(fy.tokens, fy.enums): _*)
      case f: Term.For if hasMoreThanOneGenerator(f.enums) =>
        ctx.addPatchSet(rewriteFor(f.tokens, f.enums): _*)
      case _ =>
    }

}
