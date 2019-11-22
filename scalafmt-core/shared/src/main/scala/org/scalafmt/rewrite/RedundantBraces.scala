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

import org.scalafmt.util.TokenOps
import org.scalafmt.util.TreeOps._
import org.scalafmt.util.Whitespace

/**
  * Removes/adds curly braces where desired.
  */
case object RedundantBraces extends Rewrite {

  private type PatchBuilder =
    scala.collection.mutable.Builder[Patch, Seq[Patch]]

  @inline private def settings(
      implicit ctx: RewriteCtx
  ): RedundantBracesSettings =
    ctx.style.rewrite.redundantBraces

  private def processInterpolation(
      t: Term.Interpolate
  )(implicit builder: PatchBuilder, ctx: RewriteCtx): Unit = {
    import ctx.tokenTraverser._

    def isIdentifierAtStart(value: String) =
      value.nonEmpty && (Character.isLetterOrDigit(value.head) || value.head == '_')

    def isLiteralIdentifier(arg: Term.Name): Boolean =
      arg.syntax.startsWith("`") && arg.syntax.endsWith("`")

    /**
      * we need remain braces for interpolated literal identifiers: s"string  ${`type`}"
      * and identifiers started with '_': s"string  %{_id}"
      * otherwise formatting will result in compilation error (see https://github.com/scalameta/scalafmt/issues/1420)
      */
    def shouldTermBeEscaped(arg: Term.Name): Boolean =
      arg.value.head == '_' || isLiteralIdentifier(arg)

    t.parts.tail.zip(t.args).foreach {
      case (Lit(value: String), arg @ Term.Name(_))
          if !isIdentifierAtStart(value) && !shouldTermBeEscaped(arg) =>
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

      case t: Term.Apply if ctx.style.activeForEdition_2019_11 =>
        processApply(t)

      case b: Term.Block =>
        processBlock(b)

      case t: Term.Interpolate if settings.stringInterpolation =>
        processInterpolation(t)
    }

    builder.result()
  }

  private def processApply(
      tree: Term.Apply
  )(implicit builder: PatchBuilder, ctx: RewriteCtx): Unit = {
    val lastToken = tree.tokens.last
    if (settings.methodBodies && lastToken.is[Token.RightParen]) {
      tree.args match {
        case Nil =>
        case List(arg) => processSingleArgApply(arg, lastToken)
        case args => processMultiArgApply(args)
      }
    }
  }

  private def processSingleArgApply(
      arg: Term,
      rparen: Token
  )(implicit builder: PatchBuilder, ctx: RewriteCtx): Unit = arg match {
    // single-arg apply of a lambda
    // a(b => { c; d }) change to a { b => c; d }
    case f: Term.Function
        if f.tokens.last.is[Token.RightBrace] && getTermLineSpan(f) > 0 =>
      val rbrace = f.tokens.last
      val lbrace = ctx.matchingParens(TokenOps.hash(rbrace))
      val lparen = ctx.matchingParens(TokenOps.hash(rparen))
      builder += TokenPatch.Replace(lparen, lbrace.text)
      builder += TokenPatch.Remove(lbrace)
      builder += TokenPatch.Remove(rparen)
      removeTrailingLF(rbrace.pos, rparen)
    case _ =>
  }

  private def processMultiArgApply(
      args: Seq[Term]
  )(implicit builder: PatchBuilder, ctx: RewriteCtx): Unit = args.foreach {
    // multi-arg apply of single-stat lambdas
    // a(b => { c }, d => { e }) change to a(b => c, d => e)
    // a single-stat lambda with braces can be converted to one without braces,
    // but the reverse conversion isn't always possible
    case fun @ Term.Function(_, body)
        if fun.tokens.last.is[Token.RightBrace]
          && exactlyOneStatement(body) && isLineSpanOk(body) =>
      val rbrace = fun.tokens.last
      val lbrace = ctx.matchingParens(TokenOps.hash(rbrace))
      builder += TokenPatch.Remove(lbrace)
      builder += TokenPatch.Remove(rbrace)
      removeTrailingLF(lbrace.pos, rbrace)
      ctx.tokenTraverser
        .reverseFind(rbrace)(!Whitespace.unapply(_))
        .foreach(t => removeTrailingLF(t.pos, rbrace))
    case _ =>
  }

  private def removeTrailingLF(
      bodyEnd: Position,
      close: Token
  )(implicit builder: PatchBuilder, ctx: RewriteCtx): Unit =
    if (close.pos.startLine != bodyEnd.endLine) {
      import ctx.tokenTraverser._
      val next = nextToken(close)
      if (next.is[LF])
        builder += TokenPatch.Remove(next)
    }

  private def processBlock(
      b: Term.Block
  )(implicit builder: PatchBuilder, ctx: RewriteCtx): Unit =
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

  private def exactlyOneStatement(b: Term.Block): Boolean =
    b.stats.lengthCompare(1) == 0

  private def exactlyOneStatement(t: Term): Boolean = t match {
    case b: Term.Block => exactlyOneStatement(b)
    case _ => true
  }

  private def removeBlock(b: Term.Block)(implicit ctx: RewriteCtx): Boolean = {
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
        exactlyOneStatement(b) &&
        blockSizeIsOk(b) &&
        innerOk &&
        !isProcedureSyntax(d) &&
        !disqualifiedByUnit

      case p: Term.Function
          if ctx.style.activeForEdition_2019_11 && isBlockFunction(p) =>
        settings.methodBodies

      case _ =>
        settings.generalExpressions &&
          exactlyOneStatement(b) &&
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
    getBlockLineSpan(b) <= settings.maxLines

  private def isLineSpanOk(b: Term)(implicit ctx: RewriteCtx): Boolean = {
    val diff = b match {
      case b: Term.Block => getBlockLineSpan(b)
      case _ => getTermLineSpan(b)
    }
    diff <= settings.maxLines
  }

  private def getBlockLineSpan(b: Term.Block)(implicit ctx: RewriteCtx): Int =
    if (b.stats.isEmpty) getTermLineSpan(b)
    else b.stats.last.pos.endLine - b.stats.head.pos.startLine

  private def getTermLineSpan(b: Term)(implicit ctx: RewriteCtx): Int =
    if (b.tokens.isEmpty) 0
    else b.tokens.last.pos.endLine - b.tokens.head.pos.startLine

}
