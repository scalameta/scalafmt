package org.scalafmt.rewrite

import org.scalafmt.config._
import org.scalafmt.internal._
import org.scalafmt.util.TreeOps._

import scala.meta._
import scala.meta.classifiers.Classifier
import scala.meta.internal.prettyprinters.{TreeSyntacticGroup => TSG}
import scala.meta.internal.tokens.Chars
import scala.meta.tokens.{Token => T}

import scala.annotation.tailrec

object RedundantBraces extends Rewrite with FormatTokensRewrite.RuleFactory {

  import FormatTokensRewrite._

  override def enabled(implicit style: ScalafmtConfig): Boolean = true

  def usedIn(rewrite: RewriteSettings): Boolean = rewrite.rules.contains(this)
  def used(implicit style: ScalafmtConfig): Boolean = usedIn(style.rewrite)

  override def create(implicit ftoks: FormatTokens): Rule = new RedundantBraces

  override def priority: Int = RemoveScala3OptionalBraces.priority + 1

  private def settings(implicit
      style: ScalafmtConfig,
  ): RedundantBracesSettings = style.rewrite.redundantBraces

  private def needParensAroundParams(f: Term.FunctionLike): Boolean =
    /* either we have parens or no type; multiple params or
     * no params guarantee parens, so we look for type and
     * parens only for a single param */
    f.paramClause match {
      case pc @ Term.ParamClause(param :: Nil, _) => param.decltpe.nonEmpty &&
        !pc.tokens.head.is[T.LeftParen]
      case _ => false
    }

  def canRewriteStatWithParens(
      stat: Tree,
  )(implicit ftoks: FormatTokens): Boolean = {
    @tailrec
    def stripTopBlock(tree: Tree): Option[Tree] = tree match {
      case Term.Block(s :: Nil)
          if (tree ne stat) || !ftoks.tokenAfter(s).right.is[T.Semicolon] =>
        stripTopBlock(s)
      case _: Term.Block => None
      /* guard for statements requiring a wrapper block
       * "foo { x => y; z }" can't become "foo(x => y; z)"
       * "foo { x1 => x2 => y; z }" can't become "foo(x1 => x2 => y; z)"
       */
      case t: Term.FunctionLike =>
        if (needParensAroundParams(t)) None else stripTopBlock(t.body)
      case _ => Some(tree)
    }
    @tailrec
    def iter(trees: List[Tree]): Boolean = trees match {
      case head :: rest => head match {
          case _: Term.Repeated => iter(rest)
          case _: Term.PartialFunction | _: Defn | _: Term.Assign => false
          case b @ Term.Block(s :: ss) =>
            if (ss.isEmpty) iter(s :: rest)
            else ftoks.isEnclosedInBraces(b) && iter(rest)
          case t: Term.If => iter(t.thenp :: t.elsep :: rest)
          case t: Term.FunctionLike if needParensAroundParams(t) => false
          case t: Tree.WithBody => iter(t.body :: rest)
          case t: Term.AnonymousFunction => iter(t.body :: rest)
          case _ => iter(rest)
        }
      case _ => true
    }
    stripTopBlock(stat).exists(t => iter(t :: Nil))
  }

  private def checkApply(t: Tree): Boolean = t.parent match {
    case Some(p @ Term.ArgClause(`t` :: Nil, _)) => isParentAnApply(p)
    case _ => false
  }

  private[scalafmt] def canRewriteWithParensOnRightBrace(rb: FT)(implicit
      ftoks: FormatTokens,
  ): Boolean = !ftoks.prevNonCommentBefore(rb).left.is[T.Semicolon] &&
    (rb.meta.leftOwner match { // look for "foo { bar }"
      case b: Term.Block => checkApply(b) && canRewriteStatWithParens(b) &&
        b.parent.exists(ftoks.getLast(_) eq rb)
      case f: Term.FunctionLike => checkApply(f) && canRewriteStatWithParens(f)
      case t @ Term.ArgClause(arg :: Nil, _) => isParentAnApply(t) &&
        ftoks.getDelimsIfEnclosed(t).exists(_._2 eq rb) &&
        canRewriteStatWithParens(arg)
      case _ => false
    })

  private[scalafmt] def isReplacedWithBrace(repl: Replacement): Boolean =
    (repl.how eq ReplacementType.Replace) && repl.ft.right.is[T.LeftBrace]

  private[scalafmt] def isLeftParenReplacedWithBraceOnLeft(pft: FT)(implicit
      session: Session,
  ): Boolean = session.claimedRuleOnLeft(pft).exists(isReplacedWithBrace)

  private def okLineSpan(tree: Tree)(implicit style: ScalafmtConfig): Boolean =
    getTreeLineSpan(tree) <= settings.maxBreaks

  private def isDefnBodiesEnabled(
      noParams: => Boolean,
  )(implicit style: ScalafmtConfig): Boolean = settings.defnBodies match {
    case RedundantBracesSettings.DefnBodies.all => true
    case RedundantBracesSettings.DefnBodies.none => false
    case RedundantBracesSettings.DefnBodies.noParams => noParams
  }

  private def okToRemoveAroundFunctionBody(
      b: Term,
      okIfMultipleStats: => Boolean,
  )(implicit style: ScalafmtConfig): Boolean =
    isDefnBodiesEnabled(noParams = false) &&
      (getTreeSingleStat(b) match {
        case Some(_: Term.PartialFunction) => false
        case Some(_: Term.Block) => true
        case Some(s) => okLineSpan(s)
        case _ => okIfMultipleStats
      })

  @inline
  private def okToRemoveAroundFunctionBody(b: Term, s: Seq[Tree])(implicit
      style: ScalafmtConfig,
  ): Boolean = okToRemoveAroundFunctionBody(b, isSeqSingle(s))

  private def getSingleStatIfLineSpanOk(b: Term.Block)(implicit
      style: ScalafmtConfig,
  ): Option[Stat] = getBlockSingleStat(b).filter(okLineSpan(_))

  @tailrec
  private[rewrite] def okCommentBeforeClose(
      xft: FT,
  )(implicit ftoks: FormatTokens, session: Session): Boolean =
    ftoks.prevNotTrailingComment(xft) match {
      case Right(x) => (x eq xft) || !session.isRemovedOnLeft(x, ok = true) || {
          val pft = ftoks.prev(x)
          pft.noBreak && okCommentBeforeClose(pft)
        }
      case _ => false
    }

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
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Option[Replacement] = Option(ft.right match {
    case _: T.LeftBrace => onLeftBrace
    case _: T.LeftParen => onLeftParen
    case _ => null
  })

  override def onRight(left: Replacement, hasFormatOff: Boolean)(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Option[(Replacement, Replacement)] = {
    def okBeforeBrace = {
      val pft = ftoks.prevNonComment(ft)
      def isRemoved = session.isRemovedOnLeft(pft, ok = true)
      pft.left match {
        case _: T.Comma => RewriteTrailingCommas.enabled || isRemoved
        case _: T.Semicolon => isRemoved
        case _ => true
      }
    }
    Option(ft.right match {
      case _: T.RightBrace if okBeforeBrace => onRightBrace(left)
      case _: T.RightParen => onRightParen(left, hasFormatOff)
      case _ => null
    })
  }

  private def onLeftParen(implicit ft: FT, style: ScalafmtConfig): Replacement = {
    val rt = ft.right

    def replaceWithBrace(claim: List[Int] = Nil, owner: Option[Tree] = None) = {
      val lb = new T.LeftBrace(rt.input, rt.dialect, rt.start)
      replaceToken("{", owner = owner, claim = claim)(lb)
    }

    ft.rightOwner match {
      case ta @ Term.ArgClause(arg :: Nil, None)
          if !ta.parent.is[Init] && getOpeningParen(ta).contains(rt) =>
        val lp = ftoks.next(ft)
        def useDelim = settings.oneStatApply.changeDelim(lp, ftoks.getLast(ta))
        def shouldReplaceWithBrace(s: Stat): Boolean = s match {
          case x: Term.ApplyInfix
              if !style.dialect.allowInfixOperatorAfterNL &&
                RedundantParens.breaksBeforeOp(x) => false
          case _ => useDelim eq T.LeftBrace
        }
        def replaceWithNextBrace(claimToo: List[Int] = Nil) =
          ftoks.nextNonComment(lp) match {
            case FT(lt, _: T.LeftBrace, m) =>
              val claim = m.idx :: claimToo
              if (lt eq rt) removeToken(claim = claim)
              else replaceWithBrace(owner = Some(m.rightOwner), claim = claim)
            case _ => replaceWithBrace(owner = Some(arg), claim = claimToo)
          }
        def rewriteFunction(f: Term.FunctionLike)(shouldMoveBrace: => Boolean) =
          getBlockToReplaceAsFuncBodyInSingleArgApply(ta, f).fold(
            if (!shouldMoveBrace) null
            else if (f eq arg) replaceWithBrace()
            else replaceWithNextBrace(),
          ) { case (xb, xlb) =>
            val shouldMoveNextBrace = !canRewriteStatWithParens(xb) ||
              !okLineSpan(xb) || (useDelim eq T.LeftBrace)
            if (!shouldMoveNextBrace) null
            else {
              val claim = xlb.idx - 1 :: Nil
              if (f eq arg) replaceWithBrace(claim = claim)
              else replaceWithNextBrace(claimToo = claim)
            }
          }
        arg match {
          // single-arg apply of a partial function or optionally any arg
          // a({ case b => c; d }) change to a { case b => c; d }
          case _: Term.PartialFunction => replaceWithNextBrace()
          case b: Term.Block =>
            // options:
            // 1. if oneStatApply wants braces, move brace
            // 2. if oneStatApply wants parens and the block can be stripped, do nothing here
            // 3. otherwise, do nothing if the block can and will be stripped, else move brace
            def blockCantBeStripped = ftoks.prevNonCommentBefore(ftoks.getLast(b))
              .left.is[T.Semicolon]
            def blockWontBeStripped = !okLineSpan(b)
            def shouldMoveBrace(s: Stat): Boolean =
              !canRewriteStatWithParens(s) ||
                (useDelim match {
                  case T.LeftBrace => true
                  case T.LeftParen => blockCantBeStripped
                  case _ => blockCantBeStripped || blockWontBeStripped
                })
            getTreeSingleExpr(b) match {
              case Some(f: Term.FunctionLike) =>
                rewriteFunction(f)(shouldMoveBrace(f))
              case Some(s: Term) if !shouldMoveBrace(s) => null
              case _ => replaceWithNextBrace()
            }
          case f: Term.FunctionLike =>
            rewriteFunction(f)(shouldReplaceWithBrace(f.body))
          case _: Term.Assign | _: Term.Repeated | _: Term.Ascribe => null
          case _ if ta.parent.is[Term.ApplyInfix] => null
          case t => if (shouldReplaceWithBrace(t)) replaceWithBrace() else null
        }
      case _ => null
    }
  }

  private def onRightParen(left: Replacement, hasFormatOff: Boolean)(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): (Replacement, Replacement) = left.how match {
    case ReplacementType.Remove =>
      getRightBraceBeforeRightParen(shouldBeRemoved = false).map { rb =>
        ft.rightOwner match {
          case ac: Term.ArgClause => ftoks.matchingOptLeft(rb).foreach(lbr =>
              session.rule[RemoveScala3OptionalBraces].foreach { r =>
                val lb = ftoks.prev(lbr)
                session.getClaimed(lb.idx).foreach { case (leftIdx, lbRepl) =>
                  val orepl = if (lbRepl ne null) lbRepl else left
                  val repl = r.onLeftForArgClause(lb, orepl)(ac)
                  if (null ne repl) {
                    implicit val ft: FT = ftoks.prev(rb)
                    repl.advanceSpan(ft) // instead of removing from session
                    repl.onRightAndClaim(hasFormatOff, leftIdx)
                  }
                }
              },
            )
          case _ =>
        }
        (left, removeToken)
      }.orNull

    case ReplacementType.Replace if left.ft.right.is[T.LeftBrace] =>
      // this also removes any trailing commas, so can't be conditionally invoked
      val pftOpt = getRightBraceBeforeRightParen(shouldBeRemoved = true)
      def replaceWithBrace(rb: T, rtype: ReplacementType, startOff: Int = 0) = {
        val rbo = Some(left.ft.rightOwner)
        left -> replaceToken("}", owner = rbo, rtype = rtype)(
          new T.RightBrace(rb.input, rb.dialect, rb.start + startOff),
        )
      }
      def replaceIfAfterRightBrace = pftOpt.map { pft =>
        val rb = pft.left
        // move right to the end of the function
        val rType = new ReplacementType.RemoveAndResurrect(pft.idx - 1)
        // create a shifted token so that any child tree wouldn't own it
        replaceWithBrace(rb, rType, startOff = 1)
      }
      (ft.rightOwner match {
        case ac: Term.ArgClause => session.rule[RemoveScala3OptionalBraces]
            .flatMap { r =>
              val repl = r.onLeftForArgClause(left.ft, left)(ac)
              if (repl eq null) None else repl.onRight(hasFormatOff)
            }.orElse(
              if (left.claim.nonEmpty) None
              else Some(replaceWithBrace(ft.right, ReplacementType.Replace)),
            )
        case _ => None
      }).getOrElse(
        replaceIfAfterRightBrace.orNull, // don't know how to Replace
      )
    case _ => null
  }

  private def getRightBraceBeforeRightParen(
      shouldBeRemoved: Boolean,
  )(implicit ft: FT, session: Session, style: ScalafmtConfig): Option[FT] = {
    @inline
    def isRemovedOnLeft(xft: FT): Boolean = session
      .isRemovedOnLeft(xft, ok = true)
    @tailrec
    def shouldNotBeRemoved(xft: FT): Boolean = !isRemovedOnLeft(xft) || {
      val pxft = ftoks.prevNonCommentBefore(xft)
      pxft.left.is[T.RightBrace] && shouldNotBeRemoved(pxft)
    }
    def checkBrace(xft: FT): Boolean = xft.left.is[T.RightBrace] &&
      (if (shouldBeRemoved) isRemovedOnLeft(xft) else shouldNotBeRemoved(xft))
    val pft = ftoks.prevNonComment(ft)
    val ok =
      if (pft.left.is[T.Comma]) // looks like trailing comma
        checkBrace(ftoks.prevNonCommentBefore(pft)) &&
        session.isRemovedOnLeftOpt(pft).getOrElse {
          val crt = ftoks.prev(pft)
          val crepl = Replacement(this, crt, ReplacementType.Remove, style)
          session.claim(crepl)(crt)
          true
        }
      else checkBrace(pft)
    if (ok) Some(pft) else None
  }

  private def onLeftBrace(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Replacement = onLeftBrace(ft.meta.rightOwner)

  private def onLeftBrace(
      owner: Tree,
  )(implicit ft: FT, session: Session, style: ScalafmtConfig): Replacement = {
    def handleInterpolation =
      if (settings.stringInterpolation && processInterpolation) removeToken
      else null

    owner match {
      case t: Term.FunctionLike if t.tokens.last.is[T.RightBrace] =>
        if (!okToRemoveFunctionInApplyOrInit(t)) null else removeToken
      case t: Term.PartialFunction =>
        val pft = ftoks.prevNonComment(ft)
        pft.left match {
          case _: T.LeftParen if isLeftParenReplacedWithBraceOnLeft(pft) =>
            removeToken
          case _: T.LeftBrace
              if owner.parent.contains(pft.leftOwner) && okLineSpan(t) &&
                findTreeWithParentEx(owner) {
                  case p: Term.Block => Some(p)
                  case _: Term.ArgClause => None
                  case _ => Some(null)
                }.isEmpty => removeToken
          case _ => null
        }
      case t: Term.Block => t.parent match {
          case Some(p: Term.ArgClause)
              if isParentAnApply(p) && canRewriteStatWithParens(t) =>
            val head = ftoks.getHead(p)
            def useParens = settings.oneStatApply
              .changeDelim(head, ftoks.getLast(p)) eq T.LeftParen
            if (head.left ne ft.right) { // it was a left paren
              val keepBrace = session.claimedRuleOnLeft(head)
                .exists(_.isRemove) || !useParens && !okLineSpan(t)
              if (keepBrace) null else removeToken
            } else // arg clause is this block
            if (useParens) replaceTokenBy("(", Some(p))(x =>
              new T.LeftParen(x.input, x.dialect, x.start),
            )
            else null
          case Some(f: Term.FunctionLike)
              if getBlockToReplaceAsFuncBodyIfInSingleArgApply(f).exists {
                case (_, xft) => xft.idx <= ft.idx + 1
              } => removeToken
          case Some(_: Term.Interpolate) => handleInterpolation
          case Some(_: Term.Xml) => null
          case Some(_: Term.Annotate) => null
          case Some(_: Term.Return) if ftoks.next(ft).right.is[T.Comment] =>
            null
          case Some(p: Case) =>
            val ok = settings.generalExpressions &&
              ((p.body eq t) || shouldRemoveSingleStatBlock(t))
            if (ok) removeToken else null
          case _ => if (processBlock(t)) removeToken else null
        }
      case _: Term.Interpolate => handleInterpolation
      case Importer(_, List(x))
          if !x.isAny[Importee.Rename, Importee.Unimport] ||
            style.dialect.allowAsForImportRename &&
            (ConvertToNewScala3Syntax.enabled ||
              !x.tokens.exists(_.is[T.RightArrow])) => removeToken
      case t: Ctor.Block
          if t.stats.isEmpty && isDefnBodiesEnabled(noParams = false) &&
            okLineSpan(t) =>
        val prevIsEquals = ftoks.prevNonComment(ft).left.is[T.Equals]
        if (prevIsEquals) removeToken
        else replaceTokenBy("=", t.parent)(x =>
          new T.Equals(x.input, x.dialect, x.start),
        )
      case _ => null
    }
  }

  private def onRightBrace(left: Replacement)(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): (Replacement, Replacement) = {
    def okSemicolon(t: Tree): Boolean = !braceSeparatesTwoXmlTokens &&
      (ftoks.prevNonComment(ft) match {
        case FT(_: T.Semicolon, _, m) =>
          val plo = m.leftOwner
          (plo eq t) || !plo.parent.contains(t)
        case _ => true
      })
    ft.rightOwner match {
      case t: Tree.Block => left.how match {
          case ReplacementType.Remove
              if okSemicolon(t) &&
                ((t.parent match {
                  case Some(_: Term.Block) => true
                  case Some(_: Term.ArgClause) =>
                    val pft = ftoks.prevNonComment(left.ft)
                    pft.left match {
                      case _: T.LeftParen =>
                        isLeftParenReplacedWithBraceOnLeft(pft)
                      case _: T.LeftBrace => true
                      case _ => false
                    }
                  case _ => style.dialect.allowSignificantIndentation
                }) ||
                  okCommentBeforeClose(ft) &&
                  !elseAfterRightBraceThenpOnLeft) => (left, removeToken)
          case ReplacementType.Replace => left.ft.right match {
              case _: T.LeftParen => left -> replaceTokenBy(")", t.parent)(x =>
                  new T.RightParen(x.input, x.dialect, x.start),
                )
              case _: T.Equals if okSemicolon(t) || okCommentBeforeClose(ft) =>
                (left, removeToken)
              case _ => null
            }
          case _ => null
        }
      case _ => (left, removeToken)
    }
  }

  private def processInterpolation(implicit ft: FT): Boolean = {
    def isIdentifierAtStart(value: String) = value.headOption
      .exists(x => Character.isLetterOrDigit(x) || x == '_')

    /** we need to keep braces
      *   - for interpolated literal identifiers: {{{s"string ${`type`}"}}}
      *   - and identifiers starting with '_': {{{s"string %{_id}"}}}, otherwise
      *     formatting will result in compilation error (see
      *     https://github.com/scalameta/scalafmt/issues/1420)
      */
    def canRemoveAroundName(name: String): Boolean = name.headOption.forall {
      case '_' | '$' => false
      case '`' => name.length <= 1 || name.last != '`'
      case _ => true
    }

    val ft2 = ftoks(ft, 2) // should point to "name}"
    ft2.right.is[T.RightBrace] &&
    (ft2.meta.leftOwner match {
      case t: Term.Name => canRemoveAroundName(t.text)
      case _ => false
    }) &&
    (ftoks(ft2, 2).right match { // skip splice end, to get interpolation part
      case T.Interpolation.Part(value) => !isIdentifierAtStart(value)
      case _ => false
    })
  }

  private def getOpeningParen(t: Term.ArgClause): Option[T.LeftParen] =
    ftoks.getHead(t).left match {
      case lp: T.LeftParen => Some(lp)
      case _ => None
    }

  // single-arg apply of a lambda
  // a(b => { c; d }) change to a { b => c; d }

  private def getBlockToReplaceAsFuncBodyInSingleArgApply(
      ta: Term.ArgClause,
      func: Term.FunctionLike,
  )(implicit
      style: ScalafmtConfig,
      ftoks: FormatTokens,
  ): Option[(Term.Block, FT)] = {
    @tailrec
    def iter(body: Tree): Option[(Term.Block, FT)] = body match {
      case b: Term.Block => ftoks.getDelimsIfEnclosed(b) match {
          case Some((lb, _)) =>
            if (!lb.left.is[T.LeftBrace]) None
            else {
              val lbo = lb.leftOwner
              if (lbo eq b) Some((b, lb)) else iter(lbo)
            }
          case None => b.stats match {
              case (s: Term) :: Nil => iter(s)
              case _ => None
            }
        }
      case f: Term.FunctionLike if !needParensAroundParams(f) => iter(f.body)
      case _ => None
    }

    val ok = isDefnBodiesEnabled(noParams = false) && isParentAnApply(ta)
    if (ok) iter(func.body) else None
  }

  private def getBlockToReplaceAsFuncBodyIfInSingleArgApply(
      func: Term.FunctionLike,
  )(implicit
      style: ScalafmtConfig,
      ftoks: FormatTokens,
  ): Option[(Term.Block, FT)] = {
    @tailrec
    def iter(t: Tree, f: Term.FunctionLike): Option[(Term.Block, FT)] =
      t.parent match {
        case Some(p @ Term.ArgClause(_ :: Nil, _)) =>
          getBlockToReplaceAsFuncBodyInSingleArgApply(p, f)
        case Some(p: Term.Block) => iter(p, f)
        case Some(p: Term.FunctionLike) => iter(p, p)
        case _ => None
      }
    iter(func, func)
  }

  // multi-arg apply of single-stat lambdas
  // a(b => { c }, d => { e }) change to a(b => c, d => e)
  // a single-stat lambda with braces can be converted to one without braces,
  // but the reverse conversion isn't always possible
  private def okToRemoveFunctionInApplyOrInit(
      t: Term.FunctionLike,
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
      ft: FT,
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
  )(implicit ft: FT, style: ScalafmtConfig): Boolean = p match {
    case _: Member.Infix =>
      /* for infix, we will preserve the block unless the closing brace
       * is not followed by an operator of a further infix expression, or
       * follows a non-whitespace character on the same line as we don't
       * break lines around infix expressions.
       * we shouldn't join with the previous line (which might also end
       * in a comment), and if we keep the break before the right brace
       * we are removing, that will likely invalidate the expression. */
      val rft = ftoks.matchingRight(ft)
      def checkAfterRight(wasNonComment: => Boolean) = {
        val nrft = ftoks.nextNonComment(rft)
        !nrft.right.is[T.Ident] || !isInfixOp(nrft.rightOwner) ||
        wasNonComment && style.newlines.infix.sourceIgnoredAt(nrft)(p) ||
        findTreeWithParent(p) { // check if infix is in parens
          case pp: Member.ArgClause => ftoks.getClosingIfWithinParensOrBraces(pp)
              .map(_.isRight)
          case _: Member.Infix => None
          case _: Tree.Block => Some(false)
          case pp => Some(ftoks.isEnclosedWithinParens(pp))
        }.isDefined
      }
      val prft = ftoks.prev(rft)
      prft.left match {
        case _: T.Comma => RewriteTrailingCommas.enabled &&
          checkAfterRight(!ftoks.prev(prft).left.is[T.Comment])
        case _: T.Comment => prft.noBreak || checkAfterRight(false)
        case _ => prft.noBreak || checkAfterRight(true)
      }
    case _ => true
  }

  private def okToRemoveBlock(
      b: Term.Block,
  )(implicit ft: FT, style: ScalafmtConfig, session: Session): Boolean = b
    .parent.exists {
      case t: Term.ArgClause if isParentAnApply(t) =>
        // Example: as.map { _.toString }
        // Leave this alone for now.
        // In future there should be an option to surround such expressions with parens instead of braces
        if (isSeqMulti(t.values)) okToRemoveBlockWithinApply(b)
        else ftoks.getDelimsIfEnclosed(t).exists { case (ldelim, _) =>
          def isArgDelimRemoved = session.isRemovedOnLeft(ldelim, ok = true)
          ldelim.left match {
            case lb: T.LeftBrace => (ft.right ne lb) && !isArgDelimRemoved // arg clause has separate braces
            case _: T.LeftParen => session.claimedRuleOnLeft(ldelim) match {
                case Some(x) => isReplacedWithBrace(x) // this brace replaces paren
                case None => okLineSpan(b) && canRewriteStatWithParens(b)
              }
            case _ => false
          }
        }

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
        checkBlockAsBody(b, d.body, noParams = d.paramClauseGroups.isEmpty)

      case p: Term.FunctionLike if isFunctionWithBraces(p) =>
        okToRemoveAroundFunctionBody(b, okIfMultipleStats = true)

      case _: Term.If => settings.ifElseExpressions &&
        shouldRemoveSingleStatBlock(b)

      case Term.Block(List(`b`)) => true

      case _ if !settings.generalExpressions => false

      case p: Term.QuotedMacroExpr =>
        isValidMacroIdent[Term.SplicedMacroExpr](b, p)

      case p: Term.SplicedMacroExpr =>
        isValidMacroIdent[Term.QuotedMacroExpr](b, p)

      case _: Term.SplicedMacroPat => false

      case _ => shouldRemoveSingleStatBlock(b)
    }

  private def isValidMacroIdent[A <: Term.MacroLike](
      b: Term.Block,
      p: Term.MacroLike,
  )(implicit ft: FT, classifier: Classifier[Tree, A]): Boolean =
    !ftoks.next(ft).right.is[T.Comment] &&
      (getTreeSingleExpr(b) match {
        case Some(x: Term.Name) => Chars
            .isTypeMask(Chars.scalaLetterTypeMask)(x.text.codePointAt(0))
        case _ => false
      }) && existsParentOfType[A](p)

  private def checkBlockAsBody(b: Term.Block, rhs: Tree, noParams: => Boolean)(
      implicit style: ScalafmtConfig,
  ): Boolean = rhs.eq(b) && getSingleStatIfLineSpanOk(b).exists(innerOk(b)) &&
    isDefnBodiesEnabled(noParams)

  private def innerOk(b: Term.Block)(s: Stat): Boolean = s match {
    case _: Term.FunctionLike | _: Term.Xml => false
    case t: Term.NewAnonymous =>
      // can't allow: new A with B .foo
      // can allow if: no ".foo", no "with B", or has braces
      !b.parent.is[Term.Select] || t.templ.inits.lengthCompare(1) <= 0 ||
      t.templ.body.stats.nonEmpty || t.tokens.last.is[T.RightBrace]
    case _ => isTreeSingleExpr(s)
  }

  private def okToRemoveBlockWithinApply(b: Term.Block)(implicit
      style: ScalafmtConfig,
  ): Boolean = getSingleStatIfLineSpanOk(b).exists {
    case f: Term.FunctionLike => !needParensAroundParams(f) && {
        val fb = f.body
        !fb.is[Term.Block] ||
        // don't rewrite block if the inner block will be rewritten, too
        // sometimes a function body block doesn't have braces
        fb.tokens.headOption.is[T.LeftBrace] &&
        !okToRemoveAroundFunctionBody(fb, true)
      }
    case _: Term.Assign => false // f({ a = b }) is not the same as f(a = b)
    case _ => true
  }

  /** Some blocks look redundant but aren't */
  private def shouldRemoveSingleStatBlock(
      b: Term.Block,
  )(implicit ft: FT, style: ScalafmtConfig, session: Session): Boolean =
    getSingleStatIfLineSpanOk(b).exists { stat =>
      import style.dialect
      @tailrec
      def keepForParent(tree: Tree): Boolean = tree match {
        case t: Term.ArgClause => t.parent match {
            case Some(p) => keepForParent(p)
            case _ => true
          }
        case _: Term.TryClause =>
          def matching(tok: T) = ftoks.matchingOptLeft(ftoks(tok))
          // "try (x).y" or "try { x }.y" isn't supported until scala 2.13
          // same is true with "try (a, b)" and "try ()"
          // return true if rewrite is not OK
          // inside exists, return true if rewrite is OK
          !stat.tokens.headOption.exists {
            case x: T.LeftParen => matching(x) match {
                case Some(y) if y.left ne stat.tokens.last =>
                  session.rule[RedundantParens].exists(
                    _.onToken(ftoks(x, -1), session, style).exists(_.isRemove),
                  )
                case Some(_) if !style.dialect.allowTryWithAnyExpr =>
                  !stat.isAny[Term.Tuple, Lit.Unit]
                case _ => true
              }
            case x: T.LeftBrace => matching(x) match {
                case Some(y) if y.left ne stat.tokens.last =>
                  findFirstTreeBetween(stat, x, y.left).exists {
                    case z: Term.Block => okToRemoveBlock(z)
                    case _ => false
                  }
                case _ => true
              }
            case _ => true
          }

        case _: Case => !RewriteCtx.isPostfixExpr(stat)

        // can't do it for try until 2.13.3
        case _ if RewriteCtx.isPrefixExpr(stat) => false

        case parentIf: Term.If if stat.is[Term.If] =>
          // if (a) { if (b) c } else d
          //   ↑ cannot be replaced by ↓
          // if (a) if (b) c else d
          //   which would be equivalent to
          // if (a) { if (b) c else d }
          (parentIf.thenp eq b) && !ifWithoutElse(parentIf) &&
          existsIfWithoutElse(stat.asInstanceOf[Term.If])

        case p: Term.ApplyInfix => stat match {
            case t: Term.ApplyInfix => TSG.opNeedsParens(p.op, t.op, p.lhs eq b)
            case _ => true // don't allow other non-infix
          }

        case p: Term.MatchLike => p.expr eq b
        case p: Type.Match => p.tpe eq b

        case p: Term.ForClause if p.body eq b =>
          @tailrec
          def iter(t: Tree): Boolean = t match {
            case _: Term.Do => true
            case Term.Block(x :: Nil) => iter(x)
            case _ => false
          }
          iter(stat)

        case parent => TreeSyntacticGroup.groupNeedsParens(parent, stat)
      }

      innerOk(b)(stat) && !b.parent.exists(keepForParent)
    }

  private def braceSeparatesTwoXmlTokens(implicit ft: FT): Boolean = ft.left
    .is[T.Xml.End] && ftoks.next(ft).right.is[T.Xml.Start]

  private def elseAfterRightBraceThenpOnLeft(implicit
      ft: FT,
      ftoks: FormatTokens,
      session: Session,
  ): Boolean = ftoks.nextNonCommentAfter(ft).right.is[T.KwElse] && {
    val pft = ftoks.findToken(ft, ftoks.prev)(xft =>
      xft.left match {
        case _: T.Comment => false
        case _: T.RightBrace => !session.isRemovedOnLeft(xft, ok = true)
        case _ => true
      },
    )
    val rbOwner = ft.rightOwner
    findTreeWithParent(pft.leftOwner)(p =>
      if (p eq rbOwner) Some(false)
      else p.parent match {
        case None => Some(false)
        case Some(pp: Term.If) if pp.thenp eq p => Some(true)
        case _ => None
      },
    ).isDefined
  }

}
