package org.scalafmt.rewrite

import scala.annotation.tailrec
import scala.meta._
import scala.meta.tokens.Token

import org.scalafmt.config.{RedundantBracesSettings, ScalafmtConfig}
import org.scalafmt.internal._
import org.scalafmt.util.TreeOps._

object RedundantBraces extends Rewrite with FormatTokensRewrite.RuleFactory {

  import FormatTokensRewrite._

  override def enabled(implicit style: ScalafmtConfig): Boolean = true

  override def create(ftoks: FormatTokens): Rule =
    new RedundantBraces(ftoks)

  def needParensAroundParams(f: Term.Function): Boolean =
    /* either we have parens or no type; multiple params or
     * no params guarantee parens, so we look for type and
     * parens only for a single param */
    f.paramClause match {
      case pc @ Term.ParamClause(param :: Nil, _) =>
        param.decltpe.nonEmpty && !pc.tokens.head.is[Token.LeftParen]
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
    !needParensAroundParams(f) && (getTreeSingleStat(f.body) match {
      case Some(t: Term.Function) => canRewriteWithParens(t, true)
      case Some(_: Defn) => false
      case x => nested || x.isDefined
    })

  // we might not keep it but will hint to onRight
  private def replaceWithLeftParen(implicit ft: FormatToken): Replacement =
    replaceTokenBy("(") { x =>
      new Token.LeftParen(x.input, x.dialect, x.start)
    }

  private def replaceWithEquals(implicit ft: FormatToken): Replacement =
    replaceTokenBy("=") { x =>
      new Token.Equals(x.input, x.dialect, x.start)
    }

}

/** Removes/adds curly braces where desired.
  */
class RedundantBraces(ftoks: FormatTokens) extends FormatTokensRewrite.Rule {

  import FormatTokensRewrite._
  import RedundantBraces._

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    RedundantBraces.enabled

  override def onToken(implicit
      ft: FormatToken,
      style: ScalafmtConfig
  ): Option[Replacement] = Option {
    ft.right match {
      case _: Token.LeftBrace => onLeftBrace
      case _: Token.LeftParen => onLeftParen
      case _ => null
    }
  }

  override def onRight(left: Replacement, hasFormatOff: Boolean)(implicit
      ft: FormatToken,
      style: ScalafmtConfig
  ): Option[(Replacement, Replacement)] = Option {
    ft.right match {
      case _: Token.RightBrace => onRightBrace(left)
      case _: Token.RightParen => onRightParen(left)
      case _ => null
    }
  }

  private def onLeftParen(implicit
      ft: FormatToken,
      style: ScalafmtConfig
  ): Replacement = {
    val rt = ft.right
    val rtOwner = ft.meta.rightOwner
    def lpFunction = okToReplaceFunctionInSingleArgApply(rtOwner).map {
      case (`rt`, f) =>
        f.body match {
          case b: Term.Block =>
            ftoks.getHead(b) match {
              case FormatToken(_: Token.LeftBrace, _, lbm) =>
                replaceToken(lbm.left.text, Some(rtOwner), lbm.idx - 1 :: Nil) {
                  new Token.LeftBrace(rt.input, rt.dialect, rt.start)
                }
              case _ => null
            }
          case _ => null
        }
      case _ => null
    }
    // single-arg apply of a partial function
    // a({ case b => c; d }) change to a { case b => c; d }
    def lpPartialFunction = rtOwner match {
      case ta @ Term.ArgClause(arg :: Nil, _) =>
        getOpeningParen(ta).map { lp =>
          val ko = lp.ne(rt) || getBlockNestedPartialFunction(arg).isEmpty
          if (ko) null else removeToken
        }
      case _ => None
    }

    lpFunction.orElse(lpPartialFunction).orNull
  }

  private def onRightParen(
      left: Replacement
  )(implicit ft: FormatToken): (Replacement, Replacement) =
    (left, removeToken)

  private def onLeftBrace(implicit
      ft: FormatToken,
      style: ScalafmtConfig
  ): Replacement = {
    onLeftBrace(ft.meta.rightOwner)
  }

  @tailrec
  private def onLeftBrace(owner: Tree)(implicit
      ft: FormatToken,
      style: ScalafmtConfig
  ): Replacement = {
    owner match {
      case t: Term.ArgClause =>
        t.values match {
          case arg :: Nil if t.pos.start == arg.pos.start => onLeftBrace(arg)
          case _ => null
        }
      case t: Term.Function if t.tokens.last.is[Token.RightBrace] =>
        if (!okToRemoveFunctionInApplyOrInit(t)) null
        else if (okToReplaceFunctionInSingleArgApply(t)) replaceWithLeftParen
        else removeToken
      case t: Term.Block =>
        if (getBlockNestedPartialFunction(t).isDefined) removeToken
        else if (okToReplaceBlockInSingleArgApply(t)) replaceWithLeftParen
        else if (processBlock(t)) removeToken
        else null
      case _: Term.Interpolate
          if style.rewrite.redundantBraces.stringInterpolation &&
            processInterpolation =>
        removeToken
      case Importer(_, List(x))
          if !(x.is[Importee.Rename] || x.is[Importee.Unimport]) ||
            style.dialect.allowAsForImportRename &&
            (ConvertToNewScala3Syntax.enabled ||
              !x.tokens.exists(_.is[Token.RightArrow])) =>
        removeToken
      case t: Ctor.Secondary
          if t.stats.isEmpty && isDefnBodiesEnabled(noParams = false) =>
        val prevIsEquals = ftoks.prevNonComment(ft).left.is[Token.Equals]
        if (prevIsEquals) removeToken else replaceWithEquals
      case _ => null
    }
  }

  private def onRightBrace(
      left: Replacement
  )(implicit ft: FormatToken): (Replacement, Replacement) =
    left.ft match {
      case lft @ FormatToken(_, _: Token.LeftParen, _)
          if left.how eq ReplacementType.Replace =>
        val right = replaceTokenBy("}", ft.meta.rightOwner.parent) { rt =>
          // shifted right
          new Token.RightBrace(rt.input, rt.dialect, rt.start + 1)
        }
        (removeToken(lft), right)
      case _ => (left, removeToken)
    }

  private def settings(implicit
      style: ScalafmtConfig
  ): RedundantBracesSettings =
    style.rewrite.redundantBraces

  private def redundantParensFunc(implicit
      style: ScalafmtConfig
  ): Option[Rule] =
    if (!style.rewrite.rules.contains(RedundantParens)) None
    else Some(RedundantParens.create(ftoks))

  private def processInterpolation(implicit ft: FormatToken): Boolean = {
    def isIdentifierAtStart(value: String) =
      value.headOption.exists(x => Character.isLetterOrDigit(x) || x == '_')

    def isLiteralIdentifier(arg: Term.Name): Boolean = {
      val syntax = arg.toString()
      syntax.headOption.contains('`') && syntax.lastOption.contains('`')
    }

    /** we need to keep braces
      *   - for interpolated literal identifiers: {{{s"string ${`type`}"}}}
      *   - and identifiers starting with '_': {{{s"string %{_id}"}}}, otherwise
      *     formatting will result in compilation error (see
      *     https://github.com/scalameta/scalafmt/issues/1420)
      */
    def shouldTermBeEscaped(arg: Term.Name): Boolean =
      arg.value.head == '_' || isLiteralIdentifier(arg)

    val ft2 = ftoks(ft, 2) // should point to "name}"
    ft2.right.is[Token.RightBrace] && (ft2.meta.leftOwner match {
      case t: Term.Name => !shouldTermBeEscaped(t)
      case _ => false
    }) && (ftoks(ft2, 2).right match { // skip splice end, to get interpolation part
      case Token.Interpolation.Part(value) => !isIdentifierAtStart(value)
      case _ => false
    })
  }

  private def okToReplaceBlockInSingleArgApply(
      b: Term.Block
  )(implicit style: ScalafmtConfig): Boolean =
    b.parent.exists {
      case f: Term.Function =>
        okToReplaceFunctionInSingleArgApply(f)
      case _ => false
    }

  private def okToReplaceFunctionInSingleArgApply(f: Term.Function)(implicit
      style: ScalafmtConfig
  ): Boolean =
    f.parent.flatMap(okToReplaceFunctionInSingleArgApply).exists(_._2 eq f)

  private def getOpeningParen(t: Term.ArgClause): Option[Token.LeftParen] =
    ftoks.getHead(t).left match {
      case lp: Token.LeftParen => Some(lp)
      case _ => None
    }

  // single-arg apply of a lambda
  // a(b => { c; d }) change to a { b => c; d }
  private def okToReplaceFunctionInSingleArgApply(
      tree: Tree
  )(implicit style: ScalafmtConfig): Option[(Token.LeftParen, Term.Function)] =
    tree match {
      case ta @ Term.ArgClause((func: Term.Function) :: Nil, _) if {
            val body = func.body
            (body.is[Term.Block] || func.tokens.last.ne(body.tokens.last)) &&
            ta.parent.exists(_.is[Term.Apply]) &&
            okToRemoveAroundFunctionBody(body, true)
          } =>
        getOpeningParen(ta).map((_, func))
      case _ => None
    }

  // multi-arg apply of single-stat lambdas
  // a(b => { c }, d => { e }) change to a(b => c, d => e)
  // a single-stat lambda with braces can be converted to one without braces,
  // but the reverse conversion isn't always possible
  private def okToRemoveFunctionInApplyOrInit(
      t: Term.Function
  )(implicit style: ScalafmtConfig): Boolean =
    t.parent match {
      case Some(p: Term.ArgClause) =>
        p.parent match {
          case Some(_: Init) =>
            okToRemoveAroundFunctionBody(t.body, false)
          case Some(_: Term.Apply) =>
            getOpeningParen(p).isDefined &&
            okToRemoveAroundFunctionBody(t.body, p.values)
          case _ => false
        }
      case _ => false
    }

  private def processBlock(
      b: Term.Block
  )(implicit ft: FormatToken, style: ScalafmtConfig): Boolean =
    (ft.right match {
      case lb: Token.LeftBrace =>
        b.tokens.headOption.contains(lb) && b.tokens.last.is[Token.RightBrace]
      case rb: Token.RightBrace =>
        b.tokens.lastOption.contains(rb) && b.tokens.head.is[Token.LeftBrace]
      case _ => false
    }) && okToRemoveBlock(b) && (b.parent match {
      case Some(p: Term.ArgClause) => p.parent.exists(checkValidInfixParent)
      case Some(p) => checkValidInfixParent(p)
      case _ => true
    })

  private def checkValidInfixParent(
      p: Tree
  )(implicit ft: FormatToken, style: ScalafmtConfig): Boolean =
    p match {
      case _: Member.Infix =>
        /* for infix, we will preserve the block unless the closing brace
         * follows a non-whitespace character on the same line as we don't
         * break lines around infix expressions.
         * we shouldn't join with the previous line (which might also end
         * in a comment), and if we keep the break before the right brace
         * we are removing, that will likely invalidate the expression. */
        def checkOpen = {
          val nft = ftoks.next(ft)
          nft.noBreak ||
          style.formatInfix(p) && !nft.right.is[Token.Comment]
        }
        def checkClose = {
          val nft = ftoks(ftoks.matching(ft.right), -1)
          nft.noBreak ||
          style.formatInfix(p) && !nft.left.is[Token.Comment]
        }
        checkOpen && checkClose
      case _ => true
    }

  private def okToRemoveBlock(
      b: Term.Block
  )(implicit style: ScalafmtConfig): Boolean = {
    b.parent.exists {

      case p: Case =>
        settings.generalExpressions && {
          (p.body eq b) || shouldRemoveSingleStatBlock(b)
        }

      case t: Term.ArgClause if t.parent.exists(_.is[Term.Apply]) =>
        // Example: as.map { _.toString }
        // Leave this alone for now.
        // In future there should be an option to surround such expressions with parens instead of braces
        isSeqMulti(t.values) && okToRemoveBlockWithinApply(b)

      case d: Defn.Def =>
        def disqualifiedByUnit =
          !settings.includeUnitMethods && d.decltpe.exists {
            case Type.Name("Unit") => true
            case _ => false
          }
        checkBlockAsBody(b, d.body, noParams(d.paramClauseGroup)) &&
        !isProcedureSyntax(d) &&
        !disqualifiedByUnit

      case d: Defn.Var => d.rhs.exists(checkBlockAsBody(b, _, noParams = true))
      case d: Defn.Val => checkBlockAsBody(b, d.rhs, noParams = true)
      case d: Defn.Type =>
        checkBlockAsBody(b, d.body, noParams = d.tparamClause.values.isEmpty)
      case d: Defn.Macro =>
        checkBlockAsBody(b, d.body, noParams(d.paramClauseGroup))
      case d: Defn.GivenAlias =>
        checkBlockAsBody(b, d.body, noParams(d.paramClauseGroup))

      case p: Term.Function if isFunctionWithBraces(p) =>
        okToRemoveAroundFunctionBody(b, true)

      case _: Term.If =>
        settings.ifElseExpressions && shouldRemoveSingleStatBlock(b)

      case Term.Block(List(`b`)) => true

      case _: Term.QuotedMacroExpr | _: Term.SplicedMacroExpr => false

      case _ =>
        settings.generalExpressions && shouldRemoveSingleStatBlock(b)
    }
  }

  private def checkBlockAsBody(b: Term.Block, rhs: Tree, noParams: => Boolean)(
      implicit style: ScalafmtConfig
  ): Boolean =
    rhs.eq(b) && getSingleStatIfLineSpanOk(b).exists(innerOk(b)) &&
      isDefnBodiesEnabled(noParams)

  private def isDefnBodiesEnabled(
      noParams: => Boolean
  )(implicit style: ScalafmtConfig): Boolean =
    settings.defnBodies match {
      case RedundantBracesSettings.DefnBodies.all => true
      case RedundantBracesSettings.DefnBodies.none => false
      case RedundantBracesSettings.DefnBodies.noParams => noParams
    }

  private def noParams(group: Member.ParamClauseGroup): Boolean =
    group.tparamClause.values.isEmpty && group.paramClauses.isEmpty

  private def noParams(group: Option[Member.ParamClauseGroup]): Boolean =
    group.forall(noParams)

  private def innerOk(b: Term.Block)(s: Stat): Boolean =
    s match {
      case t: Term.NewAnonymous =>
        // can't allow: new A with B .foo
        // can allow if: no ".foo", no "with B", or has braces
        !b.parent.exists(_.is[Term.Select]) ||
        t.templ.inits.lengthCompare(1) <= 0 ||
        t.templ.stats.nonEmpty || t.tokens.last.is[Token.RightBrace]
      case tree => tree.is[Term] && tree.isNot[Term.Function]
    }

  private def okToRemoveBlockWithinApply(
      b: Term.Block
  )(implicit style: ScalafmtConfig): Boolean =
    getSingleStatIfLineSpanOk(b).exists {
      case f: Term.Function if needParensAroundParams(f) => false
      case f: Term.Function if f.body.is[Term.Block] =>
        val fb = f.body
        // don't rewrite block if the inner block will be rewritten, too
        // sometimes a function body block doesn't have braces
        fb.tokens.headOption.exists(_.is[Token.LeftBrace]) &&
        !okToRemoveAroundFunctionBody(fb, true)
      case _: Term.Assign => false // f({ a = b }) is not the same as f(a = b)
      case _ => true
    }

  /** Some blocks look redundant but aren't */
  private def shouldRemoveSingleStatBlock(
      b: Term.Block
  )(implicit style: ScalafmtConfig): Boolean =
    getSingleStatIfLineSpanOk(b).exists { stat =>
      @tailrec
      def checkParent(tree: Tree): Boolean = tree match {
        case t: Term.ArgClause =>
          t.parent match {
            case Some(p) => checkParent(p)
            case _ => true
          }
        case _: Term.Try | _: Term.TryWithHandler =>
          // "try (x).y" or "try { x }.y" isn't supported until scala 2.13
          // inside exists, return true if rewrite is OK
          !stat.tokens.headOption.exists {
            case x: Token.LeftParen =>
              ftoks.matchingOpt(x) match {
                case Some(y) if y ne stat.tokens.last =>
                  redundantParensFunc.exists { parensRule =>
                    parensRule.onToken(ftoks(x, -1), style).exists {
                      _.how eq ReplacementType.Remove
                    }
                  }
                case _ => true
              }
            case x: Token.LeftBrace =>
              ftoks.matchingOpt(x) match {
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
        case _ if isPrefixExpr(stat) => false

        case parentIf: Term.If if stat.is[Term.If] =>
          // if (a) { if (b) c } else d
          //   ↑ cannot be replaced by ↓
          // if (a) if (b) c else d
          //   which would be equivalent to
          // if (a) { if (b) c else d }
          (parentIf.thenp eq b) && !ifWithoutElse(parentIf) &&
          existsIfWithoutElse(stat.asInstanceOf[Term.If])

        case p: Term.ApplyInfix =>
          stat match {
            case t: Term.ApplyInfix =>
              val useRight = isSingleElement(p.argClause.values, b)
              SyntacticGroupOps.groupNeedsParenthesis(
                TreeSyntacticGroup(p),
                TreeSyntacticGroup(t),
                if (useRight) Side.Right else Side.Left
              )
            case _ => true // don't allow other non-infix
          }

        case p: Term.Match => p.expr eq b
        case p: Type.Match => p.tpe eq b

        case parent =>
          SyntacticGroupOps.groupNeedsParenthesis(
            TreeSyntacticGroup(parent),
            TreeSyntacticGroup(stat),
            Side.Left
          )
      }

      innerOk(b)(stat) && !b.parent.exists(checkParent)
    }

  @inline
  private def okToRemoveAroundFunctionBody(b: Term, s: Seq[Tree])(implicit
      style: ScalafmtConfig
  ): Boolean =
    okToRemoveAroundFunctionBody(b, isSeqSingle(s))

  private def okToRemoveAroundFunctionBody(
      b: Term,
      okIfMultipleStats: => Boolean
  )(implicit style: ScalafmtConfig): Boolean =
    isDefnBodiesEnabled(noParams = false) && (getTreeSingleStat(b) match {
      case Some(_: Term.PartialFunction) => false
      case Some(_: Term.Block) => true
      case Some(s) => getTreeLineSpan(s) <= settings.maxBreaks
      case _ => okIfMultipleStats
    })

  private def getBlockNestedPartialFunction(
      tree: Tree
  ): Option[Term.PartialFunction] = tree match {
    case x: Term.PartialFunction => Some(x)
    case x: Term.Block => getBlockNestedPartialFunction(x)
    case _ => None
  }

  @tailrec
  private def getBlockNestedPartialFunction(
      tree: Term.Block
  ): Option[Term.PartialFunction] = getBlockSingleStat(tree) match {
    case Some(x: Term.PartialFunction) => Some(x)
    case Some(x: Term.Block) => getBlockNestedPartialFunction(x)
    case _ => None
  }

  private def getSingleStatIfLineSpanOk(b: Term.Block)(implicit
      style: ScalafmtConfig
  ): Option[Stat] =
    getBlockSingleStat(b).filter(getTreeLineSpan(_) <= settings.maxBreaks)

  // special case for Select which might contain a space instead of dot
  private def isPrefixExpr(expr: Tree): Boolean =
    RewriteCtx.isSimpleExprOr(expr) { case t: Term.Select =>
      ftoks(t.name.tokens.head, -1).left.is[Token.Dot]
    }

}
