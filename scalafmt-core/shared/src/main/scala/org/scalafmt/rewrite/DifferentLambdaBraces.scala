package org.scalafmt.rewrite

import scala.meta._
import org.scalafmt.util.TokenOps
import scala.collection.mutable.Builder

/**
  * Replaces curly braces to parens if no line break is necessary and vice versa if line break is necessary
  *
  * For example,
  *
  * xs.map { x => x + 1 }
  *
  * becomes
  *
  * xs.map(x => x + 1)
  *
  * and
  *
  * parametersList.map(parameter => applySettings(parameter))
  *
  * becomes
  *
  * parametersList.map { parameter =>
  *   applySettings(parameter)
  * }
  */
case object DifferentLambdaBraces extends Rewrite {
  private def parensToBraces(
      applyTerm: Term.Apply,
      builder: Builder[Patch, Seq[Patch]]
  ): Unit = {
    val (_, _, lastParen) =
      applyTerm.tokens.foldLeft((false, false, Option.empty[Token])) {
        case ((leftParenReplaced, arrowReplaced, lastParen), token) =>
          if (!leftParenReplaced && token.is[Token.LeftParen]) {
            builder += TokenPatch.AddRight(token, " { ")
            (true, arrowReplaced, lastParen)
          } else if (!arrowReplaced && token.is[Token.RightArrow]) {
            builder += TokenPatch.AddRight(token, "\n", keepTok = true)
            (leftParenReplaced, true, lastParen)
          } else if (token.is[Token.RightParen]) {
            (leftParenReplaced, arrowReplaced, Some(token))
          } else {
            (leftParenReplaced, arrowReplaced, lastParen)
          }
      }

    lastParen.foreach { paren =>
      builder += TokenPatch.AddRight(paren, "\n}")
    }
  }

  private def bracesToParens(
      blockTokens: Tokens,
      builder: Builder[Patch, Seq[Patch]]
  ): Unit = {
    val (_, lastBrace) = blockTokens.foldLeft(false, Option.empty[Token]) {
      case ((leftBraceReplaced, lastBrace), token) =>
        if (!leftBraceReplaced && token.is[Token.LeftBrace]) {
          builder += TokenPatch.AddRight(token, "(")
          (true, lastBrace)
        } else if (token.is[Token.RightBrace]) {
          (leftBraceReplaced, Some(token))
        } else {
          (leftBraceReplaced, lastBrace)
        }
    }
    lastBrace.foreach { brace =>
      builder += TokenPatch.AddRight(brace, ")")
    }
  }

  override def rewrite(code: Tree, ctx: RewriteCtx): Seq[Patch] = {
    val builder = Seq.newBuilder[Patch]
    import ctx.dialect

    code.traverse {
      case fy: Term.Apply =>
        fy.args match {
          case (f: Term.Function) :: Nil =>
            val tokens = f.tokens
            if (tokens.lastOption.fold(0)(_.pos.endColumn) > ctx.style.maxColumn && !tokens
                .exists(_.is[Token.LF])) {
              parensToBraces(fy, builder)
            }

          case (b: Term.Block) :: Nil =>
            b.stats match {
              case (f: Term.Function) :: Nil =>
                val tokens = b.tokens
                if (tokens.lastOption.fold(0)(_.pos.endColumn) <= ctx.style.maxColumn && !tokens
                    .exists(_.is[Token.LF])) {
                  bracesToParens(tokens, builder)
                }
              case _ =>
            }

          case _ =>
        }
    }

    builder.result()
  }
}
