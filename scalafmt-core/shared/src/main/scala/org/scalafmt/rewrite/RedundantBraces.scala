package org.scalafmt.rewrite

import org.scalafmt.config.RedundantBracesSettings
import org.scalafmt.internal.Side
import org.scalafmt.internal.SyntacticGroupOps
import org.scalafmt.internal.TreeSyntacticGroup
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
    scala.collection.mutable.Builder[TokenPatch, Seq[TokenPatch]]

  @inline private def settings(
      implicit ctx: RewriteCtx
  ): RedundantBracesSettings =
    ctx.style.rewrite.redundantBraces

  private def processInterpolation(
      t: Term.Interpolate
  )(implicit ctx: RewriteCtx): Unit = {
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
            ctx.addPatchSet(
              TokenPatch.Remove(openBrace),
              TokenPatch.Remove(closeBrace)
            )
          case _ =>
        }
      case _ =>
    }
  }

  override def rewrite(implicit ctx: RewriteCtx): Unit = {
    ctx.tree.traverse {

      case t: Term.Apply if ctx.style.activeForEdition_2019_11 =>
        processApply(t)

      case b: Term.Block =>
        processBlock(b)

      case t: Term.Interpolate if settings.stringInterpolation =>
        processInterpolation(t)
    }
  }

  private def processApply(
      tree: Term.Apply
  )(implicit ctx: RewriteCtx): Unit = {
    val lastToken = tree.tokens.last
    if (lastToken.is[Token.RightParen]) {
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
  )(implicit ctx: RewriteCtx): Unit = arg match {
    // single-arg apply of a lambda
    // a(b => { c; d }) change to a { b => c; d }
    case f: Term.Function
        if settings.methodBodies && f.tokens.last.is[Token.RightBrace] &&
          getTermLineSpan(f) > 0 =>
      val rbrace = f.tokens.last
      val lbrace = ctx.matchingParens(TokenOps.hash(rbrace))
      // we really wanted the first token of body but Block usually
      // points to the next non-whitespace token after opening brace
      if (lbrace.start <= f.body.tokens.head.start) {
        val lparen = ctx.matchingParens(TokenOps.hash(rparen))
        implicit val builder = Seq.newBuilder[TokenPatch]
        builder += TokenPatch.Replace(lparen, lbrace.text)
        builder += TokenPatch.Remove(lbrace)
        builder += TokenPatch.Remove(rparen)
        removeLFToAvoidEmptyLine(rparen)
        ctx.addPatchSet(builder.result(): _*)
      }
    case _ =>
  }

  private def processMultiArgApply(
      args: Seq[Term]
  )(implicit ctx: RewriteCtx): Unit = args.foreach {
    // multi-arg apply of single-stat lambdas
    // a(b => { c }, d => { e }) change to a(b => c, d => e)
    // a single-stat lambda with braces can be converted to one without braces,
    // but the reverse conversion isn't always possible
    case fun @ Term.Function(_, body)
        if settings.methodBodies && fun.tokens.last.is[Token.RightBrace] &&
          isSingleStatLineSpanOk(body) =>
      val rbrace = fun.tokens.last
      val lbrace = ctx.matchingParens(TokenOps.hash(rbrace))
      if (lbrace.start <= body.tokens.head.start) {
        implicit val builder = Seq.newBuilder[TokenPatch]
        builder += TokenPatch.Remove(lbrace)
        builder += TokenPatch.Remove(rbrace)
        removeLFToAvoidEmptyLine(rbrace)
        ctx.addPatchSet(builder.result(): _*)
      }
    case _ =>
  }

  // this is a helper function to be used with the token traverser
  // finds a newline not blocked by any non-whitespace characters
  private def isLFSkipWhitespace(token: Token): Option[Boolean] =
    token match {
      case _: LF => Some(true)
      case Whitespace() => None
      case _ => Some(false)
    }

  private def removeLFToAvoidEmptyLine(
      token: Token
  )(implicit builder: PatchBuilder, ctx: RewriteCtx): Unit = {
    if (ctx.tokenTraverser
        .findBefore(token)(isLFSkipWhitespace)
        .isDefined)
      ctx.tokenTraverser
        .findAfter(token)(isLFSkipWhitespace)
        .foreach(builder += TokenPatch.Remove(_))
  }

  private def processBlock(b: Term.Block)(implicit ctx: RewriteCtx): Unit =
    b.tokens.headOption.filter(_.is[LeftBrace]).foreach { open =>
      val close = b.tokens.last
      if (close.is[RightBrace] && okToRemoveBlock(b)) {
        implicit val builder = Seq.newBuilder[TokenPatch]
        val ok =
          if (b.parent.exists(_.is[Term.ApplyInfix])) {
            /* for infix, we will preserve the block unless the closing brace
             * follows a non-whitespace character on the same line as we don't
             * break lines around infix expressions.
             * we shouldn't join with the previous line (which might also end
             * in a comment), and if we keep the break before the right brace
             * we are removing, that will likely invalidate the expression. */
            ctx.tokenTraverser.findBefore(close)(isLFSkipWhitespace).isEmpty
          } else {
            removeLFToAvoidEmptyLine(close)
            true
          }
        if (ok) {
          builder += TokenPatch.Remove(open)
          builder += TokenPatch.Remove(close)
          ctx.addPatchSet(builder.result(): _*)
        }
      }
    }

  private def okToRemoveBlock(
      b: Term.Block
  )(implicit ctx: RewriteCtx): Boolean = {
    b.parent.exists {

      case p: Case =>
        settings.generalExpressions && {
          (p.body eq b) || shouldRemoveSingleStatBlock(b)
        }

      case _: Term.Apply =>
        // Example: as.map { _.toString }
        // Leave this alone for now.
        // In future there should be an option to surround such expressions with parens instead of braces
        false

      case d: Defn.Def =>
        def disqualifiedByUnit =
          !settings.includeUnitMethods && d.decltpe.exists(_.syntax == "Unit")
        def innerOk(s: Stat) = s match {
          case _: Term.Function | _: Defn => false
          case _ => true
        }
        settings.methodBodies &&
        getSingleStatIfLineSpanOk(b).exists(innerOk) &&
        !isProcedureSyntax(d) &&
        !disqualifiedByUnit

      case p: Term.Function
          if ctx.style.activeForEdition_2019_11 && isBlockFunction(p) =>
        settings.methodBodies

      case _ =>
        settings.generalExpressions && shouldRemoveSingleStatBlock(b)
    }
  }

  /** Some blocks look redundant but aren't */
  private def shouldRemoveSingleStatBlock(
      b: Term.Block
  )(implicit ctx: RewriteCtx): Boolean =
    getSingleStatIfLineSpanOk(b).exists { stat =>
      !b.parent.exists {
        case parentIf: Term.If if stat.is[Term.If] =>
          // if (a) { if (b) c } else d
          //   ↑ cannot be replaced by ↓
          // if (a) if (b) c else d
          //   which would be equivalent to
          // if (a) { if (b) c else d }
          def insideIfThen = parentIf.thenp eq b
          def parentIfHasAnElse = parentIf.elsep.tokens.nonEmpty
          def blockIsIfWithoutElse =
            stat.asInstanceOf[Term.If].elsep.tokens.isEmpty
          insideIfThen && parentIfHasAnElse && blockIsIfWithoutElse

        case p: Term.ApplyInfix =>
          stat match {
            case _: Term.ApplyInfix =>
              val useRight = p.args.lengthCompare(1) == 0 && (p.args.head eq b)
              SyntacticGroupOps.groupNeedsParenthesis(
                TreeSyntacticGroup(p),
                TreeSyntacticGroup(stat),
                if (useRight) Side.Right else Side.Left
              )
            case _: Name | _: Lit => false
            case _ => true // don't allow other non-infix
          }

        case parent =>
          SyntacticGroupOps.groupNeedsParenthesis(
            TreeSyntacticGroup(parent),
            TreeSyntacticGroup(stat),
            Side.Left
          )
      }
    }

  private def isSingleStatLineSpanOk(
      b: Term
  )(implicit ctx: RewriteCtx): Boolean =
    b match {
      case b: Term.Block => getSingleStatIfLineSpanOk(b).isDefined
      case _ => getTermLineSpan(b) <= settings.maxLines
    }

  private def getSingleStatIfLineSpanOk(
      b: Term.Block
  )(implicit ctx: RewriteCtx): Option[Stat] =
    if (b.stats.lengthCompare(1) != 0) None
    else {
      val s = b.stats.head
      val spanOk = (s.pos.endLine - s.pos.startLine) <= settings.maxLines
      if (spanOk) Some(s) else None
    }

  private def getTermLineSpan(b: Term)(implicit ctx: RewriteCtx): Int =
    if (b.tokens.isEmpty) 0
    else b.tokens.last.pos.endLine - b.tokens.head.pos.startLine

}
