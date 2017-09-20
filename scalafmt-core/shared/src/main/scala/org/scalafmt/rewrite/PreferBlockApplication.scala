package org.scalafmt.rewrite

import org.scalafmt.util.{TokenOps, Whitespace}

import scala.meta.tokens.Tokens

import scala.meta.tokens.Token.{RightBrace, LeftBrace, RightParen, LeftParen}
import scala.meta.{Tree, _}

/**
  * Replaces function block by block
  *
  * For example,
  *
  *    phases.foreach(phase => {
  *        g.phase = phase
  *        g.globalPhase = phase
  *    })
  *
  * becomes,
  *
  *    phases.foreach { phase =>
  *        g.phase = phase
  *        g.globalPhase = phase
  *   }
  *
  */
case object PreferBlockApplication extends Rewrite {
  private def isCandidateToRewrite(d: Term.Apply, ctx: RewriteCtx): Boolean = {
    def hasFunctionWithBlock = d.args match {
      case head :: _ =>
        head match {
          case t: Term.Function if hasBlock(t, ctx) => true
          case _ => false
        }
      case _ => false
    }

    def hasBlock(t: Term.Function, ctx: RewriteCtx) = t.body match {
      case Term.Block(_)
          if (t.body.tokens.head.is[LeftBrace] && t.body.tokens.last
            .is[RightBrace]) =>
        true
      case _ => false
    }
    hasFunctionWithBlock
  }

  private def removeInnerBlock(a: Term.Arg, ctx: RewriteCtx): Seq[Patch] = {
    val builder = Seq.newBuilder[Patch]
    a match {
      case t: Term.Function => {
        builder += TokenPatch.Remove(t.body.tokens.head)
        builder += TokenPatch.Remove(t.body.tokens.last)
      }
      case _ =>
    }
    builder.result()
  }

  private def findApplyParens(
      applyTokens: Tokens,
      ctx: RewriteCtx): Option[(Token, Token)] = {
    import ctx.tokenTraverser._
    for {
      leftParen <- applyTokens.find(_.is[Token.LeftParen])
      rightParen <- ctx.matchingParens.get(TokenOps.hash(leftParen))
    } yield (leftParen, rightParen)
  }

  private def rewriteApply(
      d: Term.Apply,
      applyTokens: Tokens,
      ctx: RewriteCtx): Seq[Patch] = {
    import ctx.tokenTraverser._
    val builder = Seq.newBuilder[Patch]
    findApplyParens(applyTokens, ctx).foreach { parens =>
      {
        val openBraceToken =
          if (nextToken(parens._1).is[Token.LF]) "{" else "{\n"
        builder += TokenPatch.AddRight(
          parens._1,
          openBraceToken,
          keepTok = false)
        builder += TokenPatch.AddRight(parens._2, "}", keepTok = false)
      }
    }
    builder.result()
  }

  override def rewrite(code: Tree, ctx: RewriteCtx): Seq[Patch] = {
    import ctx.tokenTraverser._
    val builder = Seq.newBuilder[Patch]
    code.collect {
      case d: Term.Apply if isCandidateToRewrite(d, ctx) =>
        val tokens = d.tokens
        builder ++= rewriteApply(d, tokens, ctx)
        builder ++= removeInnerBlock(d.args.head, ctx)
      case _ =>
    }
    builder.result()
  }
}
