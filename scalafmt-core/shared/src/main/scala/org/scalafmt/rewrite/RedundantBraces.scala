package org.scalafmt.rewrite

import org.scalafmt.config.RedundantBracesSettings
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal._
import org.scalafmt.util.TreeOps._

import scala.meta._
import scala.meta.tokens.Token

import scala.annotation.tailrec

object RedundantBraces extends Rewrite with FormatTokensRewrite.RuleFactory {

  import FormatTokensRewrite._

  override def enabled(implicit style: ScalafmtConfig): Boolean = true

  override def create(implicit ftoks: FormatTokens): Rule = new RedundantBraces

  private def needParensAroundParams(f: Term.FunctionTerm): Boolean =
    /* either we have parens or no type; multiple params or
     * no params guarantee parens, so we look for type and
     * parens only for a single param */
    f.paramClause match {
      case pc @ Term.ParamClause(param :: Nil, _) => param.decltpe.nonEmpty &&
        !pc.tokens.head.is[Token.LeftParen]
      case _ => false
    }

  private def canRewriteBlockWithParens(b: Term.Block)(implicit
      ftoks: FormatTokens,
  ): Boolean = getBlockSingleStat(b).exists(canRewriteStatWithParens)

  private def canRewriteStatWithParens(
      t: Stat,
  )(implicit ftoks: FormatTokens): Boolean = t match {
    case f: Term.FunctionTerm => canRewriteFuncWithParens(f)
    case _: Term.Assign => false // disallowed in 2.13
    case _: Defn => false
    case _: Term.PartialFunction => false
    case Term.Block(s :: Nil) if !ftoks.isEnclosedInMatching(t) =>
      canRewriteStatWithParens(s)
    case _ => true
  }

  /* guard for statements requiring a wrapper block
   * "foo { x => y; z }" can't become "foo(x => y; z)" */
  @tailrec
  private def canRewriteFuncWithParens(
      f: Term.FunctionTerm,
      nested: Boolean = false,
  ): Boolean = !needParensAroundParams(f) &&
    (getTreeSingleStat(f.body) match {
      case Some(t: Term.FunctionTerm) => canRewriteFuncWithParens(t, true)
      case Some(_: Defn) => false
      case x => nested || x.isDefined
    })

  private def checkApply(t: Tree): Boolean = t.parent match {
    case Some(p @ Term.ArgClause(`t` :: Nil, _)) => isParentAnApply(p)
    case _ => false
  }

  private[scalafmt] def canRewriteWithParensOnRightBrace(rb: FormatToken)(
      implicit ftoks: FormatTokens,
  ): Boolean = !ftoks.prevNonCommentBefore(rb).left.is[Token.Semicolon] &&
    (rb.meta.leftOwner match { // look for "foo { bar }"
      case b: Term.Block => checkApply(b) && canRewriteBlockWithParens(b) &&
        b.parent.exists(ftoks.getLast(_) eq rb)
      case f: Term.FunctionTerm => checkApply(f) && canRewriteFuncWithParens(f)
      case t @ SingleArgInBraces(_, arg, _) => isParentAnApply(t) &&
        canRewriteStatWithParens(arg)
      case _ => false
    })

  private[scalafmt] def noSplitForParensOnRightBrace(
      rb: => FormatToken,
  )(implicit ftoks: FormatTokens, style: ScalafmtConfig): Option[FormatToken] =
    if (!style.spaces.inParentheses && style.rewrite.trailingCommas.isOptional)
      Some(rb).filter(canRewriteWithParensOnRightBrace)
    else None

}

/** Removes/adds curly braces where desired.
  */
class RedundantBraces(implicit val ftoks: FormatTokens)
    extends FormatTokensRewrite.Rule {

  import FormatTokensRewrite._
  import RedundantBraces._

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    RedundantBraces.enabled

  override def onToken(implicit
      ft: FormatToken,
      session: Session,
      style: ScalafmtConfig,
  ): Option[Replacement] = Option {
    ft.right match {
      case _: Token.LeftBrace => onLeftBrace
      case _: Token.LeftParen => onLeftParen
      case _ => null
    }
  }

  override def onRight(left: Replacement, hasFormatOff: Boolean)(implicit
      ft: FormatToken,
      session: Session,
      style: ScalafmtConfig,
  ): Option[(Replacement, Replacement)] = Option {
    ft.right match {
      case _: Token.RightBrace => onRightBrace(left)
      case _: Token.RightParen => onRightParen(left, hasFormatOff)
      case _ => null
    }
  }

  private def onLeftParen(implicit
      ft: FormatToken,
      style: ScalafmtConfig,
  ): Replacement = {
    val rt = ft.right
    val rtOwner = ft.meta.rightOwner
    def lpFunction = okToReplaceFunctionInSingleArgApply(rtOwner).map {
      case (`rt`, f) => f.body match {
          case b: Term.Block => ftoks.getHead(b) match {
              case FormatToken(_: Token.LeftBrace, _, lbm) =>
                replaceToken("{", claim = lbm.idx - 1 :: Nil) {
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
      case ta @ Term.ArgClause(arg :: Nil, _) if !ta.parent.is[Init] =>
        getOpeningParen(ta).map { lp =>
          if (lp.ne(rt) || getBlockNestedPartialFunction(arg).isEmpty) null
          else ftoks.nextNonCommentAfter(ft) match {
            case FormatToken(_, _: Token.LeftBrace, lbm) =>
              removeToken(claim = lbm.idx :: Nil)
            case _ => null
          }
        }
      case _ => None
    }

    lpFunction.orElse(lpPartialFunction).orNull
  }

  private def onRightParen(left: Replacement, hasFormatOff: Boolean)(implicit
      ft: FormatToken,
      session: Session,
      style: ScalafmtConfig,
  ): (Replacement, Replacement) = left.how match {
    case ReplacementType.Remove =>
      val resOpt = getRightBraceBeforeRightParen(false).map { rb =>
        ft.meta.rightOwner match {
          case ac: Term.ArgClause => ftoks.matchingOpt(rb.left).map(ftoks.prev)
              .foreach { lb =>
                session.rule[RemoveScala3OptionalBraces].foreach { r =>
                  session.getClaimed(lb.meta.idx).foreach { case (leftIdx, _) =>
                    val repl = r.onLeftForArgClause(ac)(lb, left.style)
                    if (null ne repl) {
                      implicit val ft: FormatToken = ftoks.prev(rb)
                      repl.onRightAndClaim(hasFormatOff, leftIdx)
                    }
                  }
                }
              }
          case _ =>
        }
        (left, removeToken)
      }
      resOpt.orNull

    case ReplacementType.Replace if {
          val lft = left.ft
          val ro = ft.meta.rightOwner
          (lft.meta.rightOwner eq ro) && lft.right.is[Token.LeftBrace]
        } =>
      val pftOpt = getRightBraceBeforeRightParen(true)
      def replaceIfAfterRightBrace = pftOpt.map { pft =>
        val rb = pft.left
        // move right to the end of the function
        val rType = new ReplacementType.RemoveAndResurrect(ftoks.prev(pft))
        left -> replaceToken("}", rtype = rType) {
          // create a shifted token so that any child tree wouldn't own it
          new Token.RightBrace(rb.input, rb.dialect, rb.start + 1)
        }
      }
      (ft.meta.rightOwner match {
        case ac: Term.ArgClause => session.rule[RemoveScala3OptionalBraces]
            .flatMap { r =>
              val repl = r.onLeftForArgClause(ac)(left.ft, left.style)
              if (repl eq null) None else repl.onRight(hasFormatOff)
            }
        case _ => None
      }).getOrElse {
        replaceIfAfterRightBrace.orNull // don't know how to Replace
      }
    case _ => null
  }

  private def getRightBraceBeforeRightParen(shouldBeRemoved: Boolean)(implicit
      ft: FormatToken,
      session: Session,
      style: ScalafmtConfig,
  ): Option[FormatToken] = {
    val pft = ftoks.prevNonComment(ft)
    val ok = pft.left match {
      case _: Token.Comma => // looks like trailing comma
        val pft2 = ftoks.prevNonCommentBefore(pft)
        pft2.left.is[Token.RightBrace] &&
        session.isRemovedOnLeft(pft2, true) == shouldBeRemoved &&
        session.isRemovedOnLeftOpt(pft).getOrElse {
          val crt = ftoks.prev(pft)
          val crepl = Replacement(this, crt, ReplacementType.Remove, style)
          session.claim(crepl)(crt)
          true
        }
      case _: Token.RightBrace => !session.isRemovedOnLeft(pft, !shouldBeRemoved)
      case _ => false
    }
    if (ok) Some(pft) else None
  }

  private def onLeftBrace(implicit
      ft: FormatToken,
      session: Session,
      style: ScalafmtConfig,
  ): Replacement = onLeftBrace(ft.meta.rightOwner)

  private def onLeftBrace(owner: Tree)(implicit
      ft: FormatToken,
      session: Session,
      style: ScalafmtConfig,
  ): Replacement = {
    def handleInterpolation =
      if (
        style.rewrite.redundantBraces.stringInterpolation &&
        processInterpolation
      ) removeToken
      else null

    owner match {
      case t: Term.FunctionTerm if t.tokens.last.is[Token.RightBrace] =>
        if (!okToRemoveFunctionInApplyOrInit(t)) null else removeToken
      case t: Term.PartialFunction => t.parent match {
          case Some(SingleArgInBraces.OrBlock(lft, `t`, _))
              if lft.left ne ft.right =>
            val ok = ftoks.findTokenWith(lft, ftoks.prev) { xft =>
              if (!xft.left.is[Token.LeftBrace]) Some(false)
              else if (session.isRemovedOnLeft(xft, ok = true)) None
              else Some(true)
            }.contains(true)
            if (ok) removeToken else null
          case _ => null
        }
      case t: Term.Block => t.parent match {
          case Some(f: Term.FunctionTerm)
              if okToReplaceFunctionInSingleArgApply(f) => removeToken
          case Some(_: Term.Interpolate) => handleInterpolation
          case Some(_: Term.Xml) => null
          case Some(_: Term.Annotate) => null
          case Some(_: Term.Return) if ftoks.next(ft).right.is[Token.Comment] =>
            null
          case Some(p: Case) =>
            val ok = settings.generalExpressions &&
              ((p.body eq t) || shouldRemoveSingleStatBlock(t))
            if (ok) removeToken else null
          case _ => if (processBlock(t)) removeToken else null
        }
      case _: Term.Interpolate => handleInterpolation
      case Importer(_, List(x))
          if !(x.is[Importee.Rename] || x.is[Importee.Unimport]) ||
            style.dialect.allowAsForImportRename &&
            (ConvertToNewScala3Syntax.enabled ||
              !x.tokens.exists(_.is[Token.RightArrow])) => removeToken
      case t: Ctor.Block
          if t.stats.isEmpty && isDefnBodiesEnabled(noParams = false) =>
        val prevIsEquals = ftoks.prevNonComment(ft).left.is[Token.Equals]
        if (prevIsEquals) removeToken
        else replaceTokenBy("=", t.parent) { x =>
          new Token.Equals(x.input, x.dialect, x.start)
        }
      case _ => null
    }
  }

  private def onRightBrace(left: Replacement)(implicit
      ft: FormatToken,
      session: Session,
      style: ScalafmtConfig,
  ): (Replacement, Replacement) = {
    @tailrec
    def okComment(xft: FormatToken): Boolean =
      ftoks.prevNotTrailingComment(xft) match {
        case Right(x) =>
          if ((x eq xft) && x.hasBreak)
            !ftoks.isAttachedCommentThenBreak(ftoks.next(ft))
          else !session.isRemovedOnLeft(x, true) || okComment(ftoks.prev(x))
        case _ => false
      }
    val ok = ft.meta.rightOwner match {
      case _: Term.Block => !braceSeparatesTwoXmlTokens &&
        (style.dialect.allowSignificantIndentation ||
          okComment(ft) && !elseAfterRightBraceThenpOnLeft)
      case _ => true
    }
    if (ok) (left, removeToken) else null
  }

  private def settings(implicit
      style: ScalafmtConfig,
  ): RedundantBracesSettings = style.rewrite.redundantBraces

  private def processInterpolation(implicit ft: FormatToken): Boolean = {
    def isIdentifierAtStart(value: String) = value.headOption
      .exists(x => Character.isLetterOrDigit(x) || x == '_')

    def isLiteralIdentifier(arg: Term.Name): Boolean = {
      val syntax = arg.toString()
      syntax.nonEmpty && syntax.head == '`' && syntax.last == '`'
    }

    /** we need to keep braces
      *   - for interpolated literal identifiers: {{{s"string ${`type`}"}}}
      *   - and identifiers starting with '_': {{{s"string %{_id}"}}}, otherwise
      *     formatting will result in compilation error (see
      *     https://github.com/scalameta/scalafmt/issues/1420)
      */
    def shouldTermBeEscaped(arg: Term.Name): Boolean = arg.value.head == '_' ||
      isLiteralIdentifier(arg)

    val ft2 = ftoks(ft, 2) // should point to "name}"
    ft2.right.is[Token.RightBrace] &&
    (ft2.meta.leftOwner match {
      case t: Term.Name => !shouldTermBeEscaped(t)
      case _ => false
    }) &&
    (ftoks(ft2, 2).right match { // skip splice end, to get interpolation part
      case Token.Interpolation.Part(value) => !isIdentifierAtStart(value)
      case _ => false
    })
  }

  private def okToReplaceFunctionInSingleArgApply(f: Term.FunctionTerm)(implicit
      style: ScalafmtConfig,
  ): Boolean = f.parent.flatMap(okToReplaceFunctionInSingleArgApply)
    .exists(_._2 eq f)

  private def getOpeningParen(t: Term.ArgClause): Option[Token.LeftParen] =
    ftoks.getHead(t).left match {
      case lp: Token.LeftParen => Some(lp)
      case _ => None
    }

  // single-arg apply of a lambda
  // a(b => { c; d }) change to a { b => c; d }
  private def okToReplaceFunctionInSingleArgApply(tree: Tree)(implicit
      style: ScalafmtConfig,
  ): Option[(Token.LeftParen, Term.FunctionTerm)] = tree match {
    case ta @ Term.ArgClause((func: Term.FunctionTerm) :: Nil, _) if {
          val body = func.body
          (body.is[Term.Block] || func.tokens.last.ne(body.tokens.last)) &&
          isParentAnApply(ta) && okToRemoveAroundFunctionBody(body, true)
        } => getOpeningParen(ta).map((_, func))
    case _ => None
  }

  // multi-arg apply of single-stat lambdas
  // a(b => { c }, d => { e }) change to a(b => c, d => e)
  // a single-stat lambda with braces can be converted to one without braces,
  // but the reverse conversion isn't always possible
  private def okToRemoveFunctionInApplyOrInit(
      t: Term.FunctionTerm,
  )(implicit style: ScalafmtConfig): Boolean = t.parent match {
    case Some(p: Term.ArgClause) => p.parent match {
        case Some(_: Init) => okToRemoveAroundFunctionBody(t.body, false)
        case Some(_: Term.Apply) => getOpeningParen(p).isDefined &&
          okToRemoveAroundFunctionBody(t.body, p.values)
        case _ => false
      }
    case _ => false
  }

  private def processBlock(b: Term.Block)(implicit
      ft: FormatToken,
      session: Session,
      style: ScalafmtConfig,
  ): Boolean = b.stats.nonEmpty && b.tokens.headOption.contains(ft.right) &&
    okToRemoveBlock(b) && !braceSeparatesTwoXmlTokens &&
    (b.parent match {
      case Some(p: Term.ArgClause) => p.parent.exists(checkValidInfixParent)
      case Some(p) => checkValidInfixParent(p)
      case _ => true
    })

  private def checkValidInfixParent(
      p: Tree,
  )(implicit ft: FormatToken, style: ScalafmtConfig): Boolean = p match {
    case _: Member.Infix =>
      /* for infix, we will preserve the block unless the closing brace
       * follows a non-whitespace character on the same line as we don't
       * break lines around infix expressions.
       * we shouldn't join with the previous line (which might also end
       * in a comment), and if we keep the break before the right brace
       * we are removing, that will likely invalidate the expression. */
      def checkOpen = {
        val nft = ftoks.next(ft)
        nft.noBreak || style.formatInfix(p) && !nft.right.is[Token.Comment]
      }
      def checkClose = {
        val nft = ftoks.prev(ftoks.matching(ft.right))
        nft.noBreak || style.formatInfix(p) && !nft.left.is[Token.Comment]
      }
      checkOpen && checkClose
    case _ => true
  }

  private def okToRemoveBlock(
      b: Term.Block,
  )(implicit style: ScalafmtConfig, session: Session): Boolean = b.parent
    .exists {
      case t: Term.ArgClause if isParentAnApply(t) =>
        // Example: as.map { _.toString }
        // Leave this alone for now.
        // In future there should be an option to surround such expressions with parens instead of braces
        if (isSeqMulti(t.values)) okToRemoveBlockWithinApply(b)
        else (t.pos.start != b.pos.start) && ftoks.isEnclosedInBraces(t)

      case d: Defn.Def =>
        def disqualifiedByUnit = !settings.includeUnitMethods &&
          d.decltpe.exists {
            case Type.Name("Unit") => true
            case _ => false
          }
        checkBlockAsBody(b, d.body, noParams = d.paramClauseGroups.isEmpty) &&
        !isProcedureSyntax(d) && !disqualifiedByUnit

      case d: Defn.Var => checkBlockAsBody(b, d.body, noParams = true)
      case d: Defn.Val => checkBlockAsBody(b, d.rhs, noParams = true)
      case d: Defn.Type =>
        checkBlockAsBody(b, d.body, noParams = d.tparamClause.isEmpty)
      case d: Defn.Macro =>
        checkBlockAsBody(b, d.body, noParams = d.paramClauseGroups.isEmpty)
      case d: Defn.GivenAlias =>
        checkBlockAsBody(b, d.body, noParams = d.paramClauseGroup.isEmpty)

      case p: Term.FunctionTerm if isFunctionWithBraces(p) =>
        okToRemoveAroundFunctionBody(b, true)

      case _: Term.If => settings.ifElseExpressions &&
        shouldRemoveSingleStatBlock(b)

      case Term.Block(List(`b`)) => true

      case _: Term.QuotedMacroExpr | _: Term.SplicedMacroExpr => false

      case _ => settings.generalExpressions && shouldRemoveSingleStatBlock(b)
    }

  private def checkBlockAsBody(b: Term.Block, rhs: Tree, noParams: => Boolean)(
      implicit style: ScalafmtConfig,
  ): Boolean = rhs.eq(b) && getSingleStatIfLineSpanOk(b).exists(innerOk(b)) &&
    isDefnBodiesEnabled(noParams)

  private def isDefnBodiesEnabled(
      noParams: => Boolean,
  )(implicit style: ScalafmtConfig): Boolean = settings.defnBodies match {
    case RedundantBracesSettings.DefnBodies.all => true
    case RedundantBracesSettings.DefnBodies.none => false
    case RedundantBracesSettings.DefnBodies.noParams => noParams
  }

  private def innerOk(b: Term.Block)(s: Stat): Boolean = s match {
    case _: Term.FunctionTerm | _: Term.Xml => false
    case t: Term.NewAnonymous =>
      // can't allow: new A with B .foo
      // can allow if: no ".foo", no "with B", or has braces
      !b.parent.is[Term.Select] || t.templ.inits.lengthCompare(1) <= 0 ||
      t.templ.body.stats.nonEmpty || t.tokens.last.is[Token.RightBrace]
    case _: Term => true
    case _ => false
  }

  private def okToRemoveBlockWithinApply(b: Term.Block)(implicit
      style: ScalafmtConfig,
  ): Boolean = getSingleStatIfLineSpanOk(b).exists {
    case f: Term.FunctionTerm => !needParensAroundParams(f) && {
        val fb = f.body
        !fb.is[Term.Block] ||
        // don't rewrite block if the inner block will be rewritten, too
        // sometimes a function body block doesn't have braces
        fb.tokens.headOption.is[Token.LeftBrace] &&
        !okToRemoveAroundFunctionBody(fb, true)
      }
    case _: Term.Assign => false // f({ a = b }) is not the same as f(a = b)
    case _ => true
  }

  /** Some blocks look redundant but aren't */
  private def shouldRemoveSingleStatBlock(b: Term.Block)(implicit
      style: ScalafmtConfig,
      session: Session,
  ): Boolean = getSingleStatIfLineSpanOk(b).exists { stat =>
    @tailrec
    def checkParent(tree: Tree): Boolean = tree match {
      case t: Term.ArgClause => t.parent match {
          case Some(p) => checkParent(p)
          case _ => true
        }
      case _: Term.TryClause =>
        // "try (x).y" or "try { x }.y" isn't supported until scala 2.13
        // same is true with "try (a, b)" and "try ()"
        // return true if rewrite is not OK
        // inside exists, return true if rewrite is OK
        !stat.tokens.headOption.exists {
          case x: Token.LeftParen => ftoks.matchingOpt(x) match {
              case Some(y) if y.left ne stat.tokens.last =>
                session.rule[RedundantParens].exists {
                  _.onToken(ftoks(x, -1), session, style).exists(_.isRemove)
                }
              case Some(_) if !style.dialect.allowTryWithAnyExpr =>
                !stat.isAny[Term.Tuple, Lit.Unit]
              case _ => true
            }
          case x: Token.LeftBrace => ftoks.matchingOpt(x) match {
              case Some(y) if y.left ne stat.tokens.last =>
                findFirstTreeBetween(stat, x, y.left).exists {
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

      case p: Term.ApplyInfix => stat match {
          case t: Term.ApplyInfix =>
            val useRight = hasSingleElement(p.argClause, b)
            SyntacticGroupOps.groupNeedsParenthesis(
              TreeSyntacticGroup(p),
              TreeSyntacticGroup(t),
              if (useRight) Side.Right else Side.Left,
            )
          case _ => true // don't allow other non-infix
        }

      case p: Term.Match => p.expr eq b
      case p: Type.Match => p.tpe eq b

      case p: Term.ForClause if p.body eq b =>
        @tailrec
        def iter(t: Tree): Boolean = t match {
          case _: Term.Do => true
          case Term.Block(x :: Nil) => iter(x)
          case _ => false
        }
        iter(stat)

      case parent => SyntacticGroupOps.groupNeedsParenthesis(
          TreeSyntacticGroup(parent),
          TreeSyntacticGroup(stat),
          Side.Left,
        )
    }

    innerOk(b)(stat) && !b.parent.exists(checkParent)
  }

  @inline
  private def okToRemoveAroundFunctionBody(b: Term, s: Seq[Tree])(implicit
      style: ScalafmtConfig,
  ): Boolean = okToRemoveAroundFunctionBody(b, isSeqSingle(s))

  private def okToRemoveAroundFunctionBody(
      b: Term,
      okIfMultipleStats: => Boolean,
  )(implicit style: ScalafmtConfig): Boolean = isDefnBodiesEnabled(noParams =
    false,
  ) &&
    (getTreeSingleStat(b) match {
      case Some(_: Term.PartialFunction) => false
      case Some(_: Term.Block) => true
      case Some(s) => okLineSpan(s)
      case _ => okIfMultipleStats
    })

  private def getBlockNestedPartialFunction(
      tree: Tree,
  ): Option[Term.PartialFunction] = tree match {
    case x: Term.PartialFunction => Some(x)
    case x: Term.Block => getBlockNestedPartialFunction(x)
    case _ => None
  }

  @tailrec
  private def getBlockNestedPartialFunction(
      tree: Term.Block,
  ): Option[Term.PartialFunction] = getBlockSingleStat(tree) match {
    case Some(x: Term.PartialFunction) => Some(x)
    case Some(x: Term.Block) => getBlockNestedPartialFunction(x)
    case _ => None
  }

  private def getSingleStatIfLineSpanOk(b: Term.Block)(implicit
      style: ScalafmtConfig,
  ): Option[Stat] = getBlockSingleStat(b).filter(okLineSpan(_))

  private def okLineSpan(tree: Tree)(implicit style: ScalafmtConfig): Boolean =
    getTreeLineSpan(tree) <= settings.maxBreaks

  // special case for Select which might contain a space instead of dot
  private def isPrefixExpr(expr: Tree): Boolean = RewriteCtx
    .isSimpleExprOr(expr) { case t: Term.Select =>
      ftoks(t.name.tokens.head, -1).left.is[Token.Dot]
    }

  private def braceSeparatesTwoXmlTokens(implicit ft: FormatToken): Boolean = ft
    .left.is[Token.Xml.End] && ftoks.next(ft).right.is[Token.Xml.Start]

  private def elseAfterRightBraceThenpOnLeft(implicit
      ft: FormatToken,
      ftoks: FormatTokens,
      session: Session,
  ): Boolean = ftoks.nextNonCommentAfter(ft).right.is[Token.KwElse] && {
    val pft = ftoks.findToken(ft, ftoks.prev) { xft =>
      xft.left match {
        case _: Token.Comment => false
        case _: Token.RightBrace => !session.isRemovedOnLeft(xft, ok = true)
        case _ => true
      }
    }
    val rbOwner = ft.rightOwner
    findTreeWithParent(pft.leftOwner) { p =>
      if (p eq rbOwner) Some(false)
      else p.parent match {
        case None => Some(false)
        case Some(pp: Term.If) if pp.thenp eq p => Some(true)
        case _ => None
      }
    }.isDefined
  }

}
