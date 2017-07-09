package org.scalafmt.rewrite

import org.scalafmt.util.{TokenOps, Whitespace}

import scala.meta.tokens.Tokens
import scala.meta.{Tree, _}

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
case object PreferCurlyFors extends Rewrite {

  def findForParens(
      forTokens: Tokens,
      ctx: RewriteCtx): Option[(Token, Token)] = {
    import ctx.tokenTraverser._

    for {
      forToken <- forTokens.find(_.is[Token.KwFor])
      leftParen <- find(forToken)(_.isNot[Whitespace])
        .filter(_.is[Token.LeftParen])
      rightParen <- ctx.matchingParens.get(TokenOps.hash(leftParen))
    } yield (leftParen, rightParen)
  }

  def findForSemiColons(
      forEnumerators: Seq[Enumerator],
      ctx: RewriteCtx): Seq[Token] = {
    import ctx.tokenTraverser._

    for {
      enumerator <- forEnumerators
      token <- enumerator
        .tokens(ctx.style.runner.dialect)
        .headOption
        .toIterable
      semicolon <- reverseFind(token)(_.isNot[Whitespace])
        .filter(_.is[Token.Semicolon])
        .toIterable
    } yield semicolon
  }

  def rewriteFor(
      forTokens: Tokens,
      forEnumerators: Seq[Enumerator],
      ctx: RewriteCtx): Seq[Patch] = {
    import ctx.tokenTraverser._

    val builder = Seq.newBuilder[Patch]

    findForParens(forTokens, ctx).foreach { parens =>
      val openBraceTokens =
        if (nextToken(parens._1).is[Token.LF]) "{" else "{\n"
      builder += TokenPatch.AddRight(parens._1, openBraceTokens)
      builder += TokenPatch.AddRight(parens._2, "}")
      findForSemiColons(forEnumerators, ctx).foreach { semiColon =>
        val semiColonReplacementTokens =
          if (nextToken(semiColon).is[Token.LF]) "" else "\n"
        builder += TokenPatch.AddRight(semiColon, semiColonReplacementTokens)
      }
    }

    builder.result()
  }

  def hasMoreThanOneGenerator(forEnumerators: Seq[Enumerator]): Boolean =
    forEnumerators.count(_.is[Enumerator.Generator]) > 1

  override def rewrite(code: Tree, ctx: RewriteCtx): Seq[Patch] = {
    val builder = Seq.newBuilder[Patch]
    import ctx.dialect
    code.collect {
      case fy: Term.ForYield if hasMoreThanOneGenerator(fy.enums) =>
        builder ++= rewriteFor(fy.tokens, fy.enums, ctx)
      case f: Term.For if hasMoreThanOneGenerator(f.enums) =>
        builder ++= rewriteFor(f.tokens, f.enums, ctx)
    }
    builder.result()
  }
}
