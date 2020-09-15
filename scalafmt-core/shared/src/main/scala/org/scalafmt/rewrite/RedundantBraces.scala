package org.scalafmt.rewrite

import scala.annotation.tailrec

import org.scalafmt.config.RedundantBracesSettings
import org.scalafmt.internal.Side
import org.scalafmt.internal.SyntacticGroupOps
import org.scalafmt.internal.TreeSyntacticGroup
import scala.meta._
import scala.meta.tokens.Token.LeftBrace
import scala.meta.tokens.Token.RightBrace

import org.scalafmt.util.InfixApp
import org.scalafmt.util.TreeOps._

object RedundantBraces extends Rewrite {
  override def create(implicit ctx: RewriteCtx): RewriteSession =
    new RedundantBraces

  def needParensAroundParams(f: Term.Function): Boolean =
    /* either we have parens or no type; multiple params or
     * no params guarantee parens, so we look for type and
     * parens only for a single param */
    f.params match {
      case List(param) if param.decltpe.nonEmpty =>
        val leftParen = f.tokens.find(_.is[Token.LeftParen])
        !leftParen.exists(_.start <= param.tokens.head.start)
      case _ => false
    }

  def canRewriteWithParens(b: Term.Block): Boolean =
    getBlockSingleStat(b).exists {
      case f: Term.Function => canRewriteWithParens(f)
      case _: Term.Assign => false // disallowed in 2.13
      case _: Defn => false
      case _ => true
    }

  /* guard for statements requiring a wrapper block
   * "foo { x => y; z }" can't become "foo(x => y; z)" */
  @tailrec
  def canRewriteWithParens(f: Term.Function, nested: Boolean = false): Boolean =
    !needParensAroundParams(f) && (getTermSingleStat(f.body) match {
      case Some(t: Term.Function) => canRewriteWithParens(t, true)
      case Some(_: Defn) => false
      case x => nested || x.isDefined
    })

}

/**
  * Removes/adds curly braces where desired.
  */
class RedundantBraces(implicit ctx: RewriteCtx) extends RewriteSession {

  import RedundantBraces._

  private val settings: RedundantBracesSettings =
    ctx.style.rewrite.redundantBraces

  private val redundantParensFunc =
    if (!ctx.style.rewrite.rules.contains(RedundantParens)) None
    else Some((new RedundantParens).rewriteFunc)

  private def processInterpolation(t: Term.Interpolate): Unit = {
    import ctx.tokenTraverser._

    def isIdentifierAtStart(value: String) =
      value.headOption.exists(x => Character.isLetterOrDigit(x) || x == '_')

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
      case (Lit.String(value), arg: Term.Name)
          if !isIdentifierAtStart(value) && !shouldTermBeEscaped(arg) =>
        val open = prevToken(arg.tokens.head)
        if (open.is[LeftBrace]) {
          val close = nextToken(arg.tokens.head)
          if (close.is[RightBrace])
            ctx.addPatchSet(TokenPatch.Remove(open), TokenPatch.Remove(close))
        }
      case _ =>
    }
  }

  override def rewrite(tree: Tree): Unit =
    tree match {
      case t: Term.Apply =>
        processApply(t)

      case t: Init =>
        processInit(t)

      case b: Term.Block =>
        processBlock(b, okToRemoveBlock)

      case t: Term.Interpolate if settings.stringInterpolation =>
        processInterpolation(t)

      case _ =>
    }

  private def processInit(tree: Init): Unit =
    tree.argss.foreach(processMultiArgApply)

  private def processApply(tree: Term.Apply): Unit = {
    val lastToken = tree.tokens.last
    if (lastToken.is[Token.RightParen]) {
      tree.args match {
        case Nil =>
        case List(arg) => processSingleArgApply(arg, lastToken)
        case args => processMultiArgApply(args)
      }
    }
  }

  private def processSingleArgApply(arg: Term, rparen: Token): Unit =
    arg match {
      // single-arg apply of a lambda
      // a(b => { c; d }) change to a { b => c; d }
      case f: Term.Function
          if okToRemoveAroundFunctionBody(f.body, true) &&
            f.tokens.last.is[Token.RightBrace] =>
        val rbrace = f.tokens.last
        val lbrace = ctx.getMatching(rbrace)
        // we really wanted the first token of body but Block usually
        // points to the next non-whitespace token after opening brace
        if (lbrace.start <= f.body.tokens.head.start) {
          val lparen = ctx.getMatching(rparen)
          implicit val builder = Seq.newBuilder[TokenPatch]
          builder += TokenPatch.Replace(lparen, lbrace.text)
          removeBraces(lbrace, rparen)
          ctx.removeLFToAvoidEmptyLine(rparen)
          ctx.addPatchSet(builder.result(): _*)
        }
      case _ =>
    }

  private def processMultiArgApply(args: Seq[Term]): Unit =
    args.foreach {
      // multi-arg apply of single-stat lambdas
      // a(b => { c }, d => { e }) change to a(b => c, d => e)
      // a single-stat lambda with braces can be converted to one without braces,
      // but the reverse conversion isn't always possible
      case fun @ Term.Function(_, body)
          if okToRemoveAroundFunctionBody(body, false) &&
            fun.tokens.last.is[Token.RightBrace] =>
        val rbrace = fun.tokens.last
        val lbrace = ctx.getMatching(rbrace)
        if (lbrace.start <= body.tokens.head.start) {
          implicit val builder = Seq.newBuilder[TokenPatch]
          removeBraces(lbrace, rbrace)
          ctx.removeLFToAvoidEmptyLine(rbrace)
          ctx.addPatchSet(builder.result(): _*)
        }
      case b: Term.Block =>
        processBlock(b, okToRemoveBlockWithinApply)
      case _ =>
    }

  private def processBlock(b: Term.Block, check: Term.Block => Boolean): Unit =
    b.tokens.headOption.filter(_.is[LeftBrace]).foreach { open =>
      val close = b.tokens.last
      if (close.is[RightBrace] && check(b)) {
        implicit val builder = Seq.newBuilder[TokenPatch]
        val ok = b.parent match {
          case Some(InfixApp(_)) =>
            /* for infix, we will preserve the block unless the closing brace
             * follows a non-whitespace character on the same line as we don't
             * break lines around infix expressions.
             * we shouldn't join with the previous line (which might also end
             * in a comment), and if we keep the break before the right brace
             * we are removing, that will likely invalidate the expression. */
            val canRemove: ((Token, Option[Token.LF])) => Boolean =
              if (!ctx.style.newlines.formatInfix) {
                case (_, lfOpt) => lfOpt.isEmpty
              }
              else {
                case (nonWs, lfOpt) =>
                  if (nonWs.is[Token.Comment]) lfOpt.isEmpty
                  else {
                    lfOpt.foreach(builder += TokenPatch.Remove(_))
                    true
                  }
              }
            Seq(
              // ensure can remove opening brace
              ctx.tokenTraverser.findAfter(open) _,
              // ensure can remove closing brace
              ctx.tokenTraverser.findBefore(close) _
            ).forall(ctx.findNonWhitespaceWith(_).exists(canRemove))
          case _ =>
            ctx.removeLFToAvoidEmptyLine(close)
            true
        }
        if (ok) {
          removeBraces(open, close)
          ctx.addPatchSet(builder.result(): _*)
        }
      }
    }

  private def okToRemoveBlock(b: Term.Block): Boolean = {
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
        settings.methodBodies &&
        getSingleStatIfLineSpanOk(b).exists(innerOk) &&
        !isProcedureSyntax(d) &&
        !disqualifiedByUnit

      case p: Term.Function if isFunctionWithBraces(p) =>
        okToRemoveAroundFunctionBody(b, true)

      case _: Term.If =>
        settings.ifElseExpressions && shouldRemoveSingleStatBlock(b)

      case _ =>
        settings.generalExpressions && shouldRemoveSingleStatBlock(b)
    }
  }

  private def innerOk(s: Stat) =
    s match {
      case _: Term.Function | _: Defn => false
      case _ => true
    }

  private def okToRemoveBlockWithinApply(b: Term.Block): Boolean =
    getSingleStatIfLineSpanOk(b).exists {
      case f: Term.Function if needParensAroundParams(f) => false
      case Term.Function(_, fb: Term.Block) =>
        // don't rewrite block if the inner block will be rewritten, too
        // sometimes a function body block doesn't have braces
        fb.tokens.headOption.exists(_.is[Token.LeftBrace]) &&
          !okToRemoveAroundFunctionBody(fb, true)
      case _ => true
    }

  /** Some blocks look redundant but aren't */
  private def shouldRemoveSingleStatBlock(b: Term.Block): Boolean =
    getSingleStatIfLineSpanOk(b).exists { stat =>
      innerOk(stat) && !b.parent.exists {
        case _: Term.Try | _: Term.TryWithHandler =>
          // "try (x).y" or "try { x }.y" isn't supported until scala 2.13
          // inside exists, return true if rewrite is OK
          !stat.tokens.headOption.exists {
            case x: Token.LeftParen =>
              ctx.getMatchingOpt(x) match {
                case Some(y) if y ne stat.tokens.last =>
                  redundantParensFunc.exists { func =>
                    findFirstTreeBetween(stat, x, y).exists {
                      func.isDefinedAt
                    }
                  }
                case _ => true
              }
            case x: Token.LeftBrace =>
              ctx.getMatchingOpt(x) match {
                case Some(y) if y ne stat.tokens.last =>
                  findFirstTreeBetween(stat, x, y).exists {
                    case z: Term.Block => okToRemoveBlock(z)
                    case _ => false
                  }
                case _ => true
              }
            case _ => true
          }

        // can't do it for try until 2.13.3
        case _ if ctx.isPrefixExpr(stat) => false

        case parentIf: Term.If if stat.is[Term.If] =>
          // if (a) { if (b) c } else d
          //   ↑ cannot be replaced by ↓
          // if (a) if (b) c else d
          //   which would be equivalent to
          // if (a) { if (b) c else d }
          def insideIfThen = parentIf.thenp eq b
          @tailrec
          def existsIfWithoutElse(t: Term.If): Boolean =
            t.elsep match {
              case x: Term.If => existsIfWithoutElse(x)
              case _ => ifWithoutElse(t)
            }
          insideIfThen && !ifWithoutElse(parentIf) &&
          existsIfWithoutElse(stat.asInstanceOf[Term.If])

        case p: Term.ApplyInfix =>
          stat match {
            case t: Term.ApplyInfix if !RewriteCtx.hasPlaceholder(t) =>
              val useRight = isSingleElement(p.args, b)
              SyntacticGroupOps.groupNeedsParenthesis(
                TreeSyntacticGroup(p),
                TreeSyntacticGroup(t),
                if (useRight) Side.Right else Side.Left
              )
            case _ => true // don't allow other non-infix
          }

        case Term.Match(`b`, _) => true

        case parent =>
          SyntacticGroupOps.groupNeedsParenthesis(
            TreeSyntacticGroup(parent),
            TreeSyntacticGroup(stat),
            Side.Left
          )
      }
    }

  private def okToRemoveAroundFunctionBody(
      b: Term,
      okIfMultipleStats: Boolean
  ): Boolean =
    settings.methodBodies && (getTermSingleStat(b) match {
      case Some(_: Term.PartialFunction) => false
      case Some(s) => getTermLineSpan(s) <= settings.maxLines
      case _ => okIfMultipleStats
    })

  private def getSingleStatIfLineSpanOk(b: Term.Block): Option[Stat] =
    getBlockSingleStat(b).filter(getTermLineSpan(_) <= settings.maxLines)

  private def removeBraces(lbrace: Token, rbrace: Token)(implicit
      builder: Rewrite.PatchBuilder
  ): Unit = {
    builder += TokenPatch.AddLeft(lbrace, " ", keepTok = false)
    builder += TokenPatch.AddRight(rbrace, " ", keepTok = false)
  }

}
