package org.scalafmt.rewrite

import org.scalafmt.config.RedundantBracesSettings
import org.scalafmt.internal.Side
import org.scalafmt.internal.SyntacticGroupOps
import org.scalafmt.internal.TreeSyntacticGroup
import scala.meta.Tree
import scala.meta._
import scala.meta.tokens.Token.LF
import scala.meta.tokens.Token.LeftBrace
import scala.meta.tokens.Token.RightBrace
import org.scalafmt.util.TreeOps._
import org.scalafmt.util.Trivia

/**
  * Removes/adds curly braces where desired.
  */
case object RedundantBraces extends Rewrite {

  private type PatchBuilder =
    scala.collection.mutable.Builder[Patch, Seq[Patch]]

  @inline private def settings(
      implicit ctx: RewriteCtx): RedundantBracesSettings =
    ctx.style.rewrite.redundantBraces

  private def processInterpolation(t: Term.Interpolate)(
      implicit builder: PatchBuilder,
      ctx: RewriteCtx): Unit = {
    import ctx.tokenTraverser._

    def isIdentifierAtStart(value: String) =
      value.nonEmpty && (Character.isLetterOrDigit(value.head) || value.head == '_')

    t.parts.tail.zip(t.args).foreach {
      case (Lit(value: String), arg @ Term.Name(_))
          if !isIdentifierAtStart(value) =>
        val openBrace = prevToken(arg.tokens.head)
        val closeBrace = nextToken(arg.tokens.head)
        (openBrace, closeBrace) match {
          case (LeftBrace(), RightBrace()) =>
            builder += TokenPatch.Remove(openBrace)
            builder += TokenPatch.Remove(closeBrace)
          case _ =>
        }
      case _ =>
    }
  }

  override def rewrite(code: Tree, ctx: RewriteCtx): Seq[Patch] = {
    implicit def _ctx = ctx

    implicit val builder = Seq.newBuilder[Patch]

    code.traverse {

      case b: Term.Block =>
        processBlock(b)

      case t: Term.Interpolate if settings.stringInterpolation =>
        processInterpolation(t)

      case Term.Function(_, body) if settings.alwaysAroundMultilineLambda =>
        processLambdaBody(body)
    }

    builder.result()
  }

  private def processLambdaBody(
      body: Term)(implicit ctx: RewriteCtx, builder: PatchBuilder) = {
    val isSingleStatement = body.isNot[Term.Block]
    def isMultilineBody = body.tokens.head.pos.startLine < body.tokens.last.pos.endLine
    def rightToken: Option[Token] =
      ctx.tokenTraverser.find(body.tokens.last)(_.isNot[Trivia])
    if (isSingleStatement && isMultilineBody && rightToken.exists(!_.is[RightBrace])) {
      builder += TokenPatch.AddLeft(body.tokens.head, "{\n", keepTok = true)
      builder += TokenPatch.AddRight(body.tokens.last, "\n}", keepTok = true)
    }
  }

  private def removeTrailingLF(bodyEnd: Position, close: Token)(
      implicit builder: PatchBuilder,
      ctx: RewriteCtx): Unit =
    if (close.pos.startLine != bodyEnd.endLine) {
      import ctx.tokenTraverser._
      val next = nextToken(close)
      if (next.is[LF])
        builder += TokenPatch.Remove(next)
    }

  private def processBlock(
      b: Term.Block)(implicit builder: PatchBuilder, ctx: RewriteCtx): Unit =
    if (b.tokens.nonEmpty) {
      val open = b.tokens.head
      if (open.is[LeftBrace]) {
        val close = b.tokens.last
        if (removeBlock(b) && close.is[RightBrace]) {
          val endPos = if (b.stats.isEmpty) b.pos else b.stats.last.pos
          removeTrailingLF(endPos, close)
          builder += TokenPatch.Remove(open)
          builder += TokenPatch.Remove(close)
        }
      }
    }

  private def removeBlock(b: Term.Block)(implicit ctx: RewriteCtx): Boolean = {
    def exactlyOneStatement = b.stats.lengthCompare(1) == 0
    b.parent.exists {

      case _: Case =>
        settings.generalExpressions

      case _: Term.Apply =>
        // Example: as.map { _.toString }
        // Leave this alone for now.
        // In future there should be an option to surround such expressions with parens instead of braces
        false

      case d: Defn.Def =>
        def disqualifiedByUnit =
          !settings.includeUnitMethods && d.decltpe.exists(_.syntax == "Unit")
        def innerOk =
          b.stats.head match {
            case _: Term.Function | _: Defn => false
            case _ => true
          }
        settings.methodBodies &&
        exactlyOneStatement &&
        blockSizeIsOk(b) &&
        innerOk &&
        !isProcedureSyntax(d) &&
        !disqualifiedByUnit

      case _ =>
        settings.generalExpressions &&
          exactlyOneStatement &&
          blockSizeIsOk(b) &&
          !retainSingleStatBlock(b)
    }
  }

  /** Some blocks look redundant but aren't */
  private def retainSingleStatBlock(b: Term.Block): Boolean =
    b.parent.exists {
      case parentIf: Term.If =>
        // if (a) { if (b) c } else d
        //   ↑ cannot be replaced by ↓
        // if (a) if (b) c else d
        //   which would be equivalent to
        // if (a) { if (b) c else d }
        def insideIfThen = parentIf.thenp eq b
        def parentIfHasAnElse = parentIf.elsep.tokens.nonEmpty
        def blockIsIfWithoutElse = b.stats.head match {
          case childIf: Term.If => childIf.elsep.tokens.isEmpty
          case _ => false
        }
        insideIfThen && parentIfHasAnElse && blockIsIfWithoutElse

      case parent =>
        val side = parent match {
          case t: Term.ApplyInfix
              if t.args.lengthCompare(1) == 0 && (t.args.head eq b) =>
            Side.Right
          case _ => Side.Left
        }
        SyntacticGroupOps.groupNeedsParenthesis(
          TreeSyntacticGroup(parent),
          TreeSyntacticGroup(b.stats.head),
          side
        )
    }

  private def blockSizeIsOk(b: Term.Block)(implicit ctx: RewriteCtx): Boolean =
    b.tokens.isEmpty || {
      val diff =
        if (b.stats.isEmpty)
          b.tokens.last.pos.endLine - b.tokens.head.pos.startLine
        else
          b.stats.last.pos.endLine - b.stats.head.pos.startLine
      diff <= settings.maxLines
    }
}
