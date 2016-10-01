package org.scalafmt.rewrite

import org.scalafmt.util.{TokenOps, Whitespace}

import scala.annotation.tailrec
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
object ReplaceForParensWithBraces extends Rewrite {

  def findForParens(forTokens: Tokens, ctx: RewriteCtx): Option[(Token, Token)] = {
    import ctx.tokenTraverser._

    @tailrec
    def nextValidLeftParen(token: Token):Option[Token] = {
      nextToken(token) match {
        case t if t.is[Token.LeftParen] => Option(t)
        case t if t.is[Whitespace] => nextValidLeftParen(t)
        case other => None
      }
    }

    for {
      forToken <- forTokens.find(_.is[Token.KwFor])
      leftParen <- nextValidLeftParen(forToken)
      rightParen <- ctx.matchingParens.get(TokenOps.hash(leftParen))
    } yield (leftParen, rightParen)
  }

  def findForSemiColons(forEnumerators: Seq[Enumerator], ctx: RewriteCtx):Seq[Token] = {
    import ctx.tokenTraverser._

    @tailrec
    def prevSemiColon(token: Token):Option[Token] = {
      prevToken(token) match {
        case t if t.is[Token.Semicolon] => Option(t)
        case t if t.is[Whitespace] => prevSemiColon(t)
        case other => None
      }
    }

    forEnumerators.flatMap { enumerator =>
      enumerator.tokens.headOption.flatMap(prevSemiColon).toList
    }
  }

  def rewriteFor(forTokens: Tokens, forEnumerators: Seq[Enumerator], ctx: RewriteCtx): Seq[Patch] = {
    import ctx.tokenTraverser._

    val builder = Seq.newBuilder[Patch]

    findForParens(forTokens, ctx).foreach { parens =>
      val openBraceTokens = if (nextToken(parens._1).is[Token.LF]) "{" else "{\n"
      builder += Patch(parens._1, parens._1, openBraceTokens)
      builder += Patch(parens._2, parens._2, "}")
      findForSemiColons(forEnumerators, ctx).foreach { semiColon =>
        val semiColonReplacementTokens = if (nextToken(semiColon).is[Token.LF]) "" else "\n"
        builder += Patch(semiColon, semiColon, semiColonReplacementTokens)
      }
    }

    builder.result()
  }

  override def rewrite(code: Tree, ctx: RewriteCtx): Seq[Patch] = {
    val builder = Seq.newBuilder[Patch]

    code.collect {
      case fy: Term.ForYield if fy.enums.size > 1 =>
        builder ++= rewriteFor(fy.tokens, fy.enums, ctx)
      case f: Term.For if f.enums.size > 1 =>
        builder ++= rewriteFor(f.tokens, f.enums, ctx)
    }
    builder.result()
  }
}
