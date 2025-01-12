package org.scalafmt.internal

import org.scalafmt.config.BinPack
import org.scalafmt.config.IndentOperator
import org.scalafmt.config.Indents
import org.scalafmt.config.Newlines
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.config.ScalafmtOptimizer
import org.scalafmt.config.TrailingCommas
import org.scalafmt.internal.Policy.NoPolicy
import org.scalafmt.rewrite.RedundantBraces
import org.scalafmt.util.InfixApp._
import org.scalafmt.util._

import org.scalameta.FileLine
import scala.meta.classifiers.Classifier
import scala.meta.internal.tokens.Chars.isOperatorPart
import scala.meta.tokens.{Token => T}
import scala.meta.{Token => _, _}

import scala.annotation.tailrec
import scala.collection.mutable

/** Helper functions for generating splits/policies for a given tree.
  */
class FormatOps(
    val topSourceTree: Tree,
    baseStyle: ScalafmtConfig,
    val filename: String = "",
) {
  import FormatOps._
  import PolicyOps._
  import TokenOps._
  import TreeOps._

  private[internal] val (initStyle, ownersMap) =
    getStyleAndOwners(topSourceTree, baseStyle)

  implicit val dialect: Dialect = initStyle.dialect
  implicit val (tokens: FormatTokens, styleMap: StyleMap) =
    FormatTokens(topSourceTree.tokens, ownersMap)(initStyle)
  import tokens._

  private[internal] implicit val soft: SoftKeywordClasses =
    new SoftKeywordClasses(dialect)

  val (forceConfigStyle, emptyQueueSpots) = getForceConfigStyle

  val optimizationEntities = OptimizationEntities(topSourceTree)

  @tailrec
  final def findFirst(start: FT, end: FT)(f: FT => Boolean): Option[FT] =
    if (start.idx >= end.idx) None
    else if (f(start)) Some(start)
    else {
      val next_ = next(start)
      if (next_ == start) None else findFirst(next_, end)(f)
    }

  def findFirstOnRight[A](start: FT, end: FT)(implicit
      classifier: Classifier[T, A],
  ): Option[FT] = findFirst(start, prev(end))(x => classifier(x.right))

  @tailrec
  final def getSlbEndOnLeft(start: FT)(implicit style: ScalafmtConfig): FT = {
    val nft = start.right match {
      case _: T.EOF => start
      case _: T.Comma | _: T.Semicolon | _: T.RightArrow | _: T.Equals |
          _: T.Interpolation.Start | _: T.Interpolation.SpliceEnd |
          _: T.Interpolation.End | _: T.Interpolation.SpliceStart |
          _: T.Interpolation.Part => null
      case _ if start.hasBlankLine => start
      case _
          if AsInfixOp(start.rightOwner)
            .orElse(AsInfixOp(prevNonComment(start).leftOwner))
            .exists(style.newlines.infix.keep) =>
        if (start.hasBreak) start else null
      case _: T.LeftParen if (start.rightOwner match {
            case _: Member.ArgClause =>
              !style.newlines.isBeforeOpenParenCallSite
            case t => !isJustBeforeTree(start)(t)
          }) => null
      case _: T.RightParen =>
        if (start.left.is[T.LeftParen]) null
        else {
          val owner = start.rightOwner
          val isDefnSite = isParamClauseSite(owner)
          implicit val clauseSiteFlags: ClauseSiteFlags =
            ClauseSiteFlags(owner, isDefnSite)
          val bpFlags = getBinpackSiteFlags(matchingRight(start), start, false)
          if (bpFlags.scalaJsStyle) scalaJsOptCloseOnRight(start, bpFlags)
          else if (
            !start.left.is[T.RightParen] ||
            !style.newlines.fold && clauseSiteFlags.dangleCloseDelim
          ) start
          else null
        }
      case _: T.LeftBrace if (start.left match {
            case _: T.CloseDelim =>
              (start.leftOwner match {
                case _: Member.SyntaxValuesClause => true
                case t: Term.Block => t.parent.is[Term.ArgClause]
                case _ => false
              }) &&
              (start.rightOwner match {
                case _: Term.ArgClause => true
                case t => t.parent.exists {
                    case p: Tree.WithBody => p.body eq start.rightOwner
                    case _: Term.ArgClause => true
                    case _ => false
                  }
              })
            case _: T.Interpolation.SpliceStart =>
              style.newlines.inInterpolation eq Newlines.InInterpolation.allow
            case _ => false
          }) => null
      case _: T.RightBracket if start.left.is[T.RightBracket] => null
      case _: T.LeftBracket => null
      case _: T.Dot => start.rightOwner match {
          case _: Type.Select => null
          case _: Term.Select
              if start.noBreak &&
                (style.newlines.getSelectChains eq Newlines.keep) => null
          case _ => start
        }
      case _: T.Ident => start.leftOwner match {
          case _: Type.Select => null
          case _: Term.Select
              if start.noBreak &&
                (style.newlines.getSelectChains eq Newlines.keep) => null
          case _ => start
        }
      case _: T.Colon if {
            !style.newlines.sometimesBeforeColonInMethodReturnType &&
            colonDeclType(start.rightOwner).isDefined
          } => tokens(start, 2) // can't break after colon either
      case _: T.Comment if start.noBreak =>
        val nft = nextNonCommentSameLineAfter(start)
        if (!start.left.is[T.LeftParen] || nft.hasBreakOrEOF) return nft // RETURN!!!
        start
      case _ => start
    }

    if (nft eq start) start
    else getSlbEndOnLeft(if (nft ne null) nft else next(start))
  }

  /** js.native is very special in Scala.js.
    *
    * Context: https://github.com/scalameta/scalafmt/issues/108
    */
  def isJsNative(body: Tree): Boolean =
    initStyle.newlines.neverBeforeJsNative &&
      (body match {
        case Term.Select(Term.Name("js"), Term.Name("native")) => true
        case _ => false
      })

  val StartsStatementRight = new FT.ExtractFromMeta[Tree](meta =>
    optimizationEntities.statementStarts.get(meta.idx + 1),
  )

  def parensTuple(ft: FT): TokenRanges = matchingOptLeft(ft)
    .fold(TokenRanges.empty)(other => TokenRanges(TokenRange(ft, other)))
  def parensTuple(tree: Tree): TokenRanges = parensTuple(getLast(tree))

  def insideBlock[A](start: FT, end: FT)(implicit
      classifier: Classifier[T, A],
  ): TokenRanges = insideBlock(start, end, x => classifier(x.left))

  def insideBlock(start: FT, end: FT, matches: FT => Boolean): TokenRanges =
    insideBlock(x => if (matches(x)) matchingOptLeft(x) else None)(start, end)

  def insideBracesBlock(start: FT, end: FT, parensToo: Boolean = false)(implicit
      style: ScalafmtConfig,
  ): TokenRanges = insideBlock(x => getEndOfBlock(x, parensToo))(start, end)

  def insideBlock(
      matches: FT => Option[FT],
  )(start: FT, end: FT): TokenRanges = {
    var result = TokenRanges.empty

    @tailrec
    def run(tok: FT): Unit = if (tok.idx < end.idx) {
      val nextTokOpt = matches(tok).flatMap(closeFt =>
        if (tok.left.start >= closeFt.left.end) None
        else {
          result = result.append(TokenRange(tok, closeFt))
          Some(closeFt)
        },
      )
      val nextTok = nextTokOpt.getOrElse(next(tok))
      if (nextTok ne tok) run(nextTok)
    }

    run(next(start))
    result
  }

  // invoked on opening paren, part of ParamClause
  @tailrec
  final def defnSiteOptimalToken(t: Tree): Option[FT] = t match {
    case _: Member.SyntaxValuesClause | _: Member.ParamClauseGroup |
        _: Type.ParamFunctionType => t.parent match {
        case Some(p) => defnSiteOptimalToken(p)
        case _ => None
      }
    case Tree.WithDeclTpe(tpe: Type.Refine) if tpe.body.nonEmpty =>
      getHeadOpt(tpe.body)
    case Tree.WithDeclTpeOpt(Some(tpe: Type.Refine)) if tpe.body.nonEmpty =>
      getHeadOpt(tpe.body)
    // macro body comes after KwMacro, not directly after Equals
    case t: Defn.Macro => tokenBeforeOpt(t.body).map(prevNonCommentBefore)
    case t: Tree.WithBody => tokenBeforeOpt(t.body)
    case t: Stat.WithTemplate => tokenBeforeOpt(t.templ)
    case t: Decl => getLastOpt(t)
    case _ => None
  }

  @inline
  def splitOneArgOneLine(close: FT, owner: Tree)(implicit
      style: ScalafmtConfig,
  ): Policy =
    if (style.newlines.configStyle.beforeComma) Policy
      .beforeLeft(close, prefix = "B[,]")(splitOneArgPerLineBeforeComma(owner))
    else Policy
      .beforeLeft(close, prefix = "A[,]")(splitOneArgPerLineAfterComma(owner))

  def splitOneArgPerLineBeforeComma(owner: Tree): Policy.Pf = {
    // TODO(olafur) clear queue between arguments, they are independent.
    case Decision(t @ FT(_, _: T.Comma, _), splits)
        if owner == t.meta.rightOwner && !next(t).right.is[T.Comment] =>
      splits.map(x => if (x.mod ne NoSplit) x else x.withMod(Newline))

    case Decision(t @ FT(_: T.Comma, right, _), splits)
        if owner == t.meta.leftOwner && !right.is[T.LeftBrace] &&
          // If comment is bound to comma, see unit/Comment.
          (!right.is[T.Comment] || t.hasBreak) =>
      val isNewline = right.is[T.Comment]
      splits.filter(_.isNL == isNewline)
  }

  def splitOneArgPerLineAfterComma(owner: Tree): Policy.Pf = {
    // Newline on every comma.
    case Decision(t @ FT(_: T.Comma, right, m), splits)
        if owner == m.leftOwner &&
          // TODO(olafur) what the right { decides to be single line?
          // If comment is bound to comma, see unit/Comment.
          (!right.is[T.Comment] || t.hasBreak) =>
      getOneArgPerLineSplitsAfterComma(right, splits)
  }

  def splitOneArgPerLineAfterCommaOnBreak(comma: FT): Policy =
    splitOneArgPerLineAfterCommaOnBreak(TokenRanges.empty)(comma)

  def splitOneArgPerLineAfterCommaOnBreak(exclude: TokenRanges)(
      comma: FT,
  ): Policy = Policy ? (comma.right.is[T.Comment] && comma.noBreak) ||
    delayedBreakPolicy(Policy.End < comma, exclude)(
      Policy.onlyFor(comma, prefix = "NL->A[,]")(
        getOneArgPerLineSplitsAfterComma(comma.right, _),
      ),
    )

  private def getOneArgPerLineSplitsAfterComma(r: T, s: Seq[Split]) =
    if (r.is[T.LeftBrace]) SplitTag.OneArgPerLine.activateOnly(s)
    else Decision.onlyNewlineSplits(s)

  def templateCurly(obj: Template.Body): Option[FT] = getHeadOpt(obj)
    .map(x => if (x.meta.leftOwner eq obj) x else prevNonCommentBefore(x))

  def templateCurlyOrLastNonTrivial(tpl: Template): FT = templateCurly(tpl.body)
    .getOrElse(getLastNonTrivial(tpl))

  def templateDerivesOrCurlyOrLastNonTrivial(
      template: Template,
  )(implicit ft: FT): FT = findTemplateGroupOnRight(_.getExpireToken)(template)
    .getOrElse(templateCurlyOrLastNonTrivial(template))

  private def findTreeInGroup[A](
      trees: Seq[Tree],
      func: TemplateSupertypeGroup => A,
  )(expireFunc: Seq[Tree] => FT)(implicit ft: FT): Option[A] = trees
    .find(_.pos.end >= ft.right.end)
    .map(x => func(TemplateSupertypeGroup(x, trees, expireFunc)))

  def findTemplateGroupOnRight[A](
      func: TemplateSupertypeGroup => A,
  )(template: Template)(implicit ft: FT): Option[A] = {
    @tailrec
    def iter(groups: Seq[Seq[Tree]]): Option[A] =
      if (isSeqSingle(groups))
        // for the last group, return '{' or ':'
        findTreeInGroup(groups.head, func)(x =>
          getHeadOpt(template.body).getOrElse(getLastNonTrivial(x.last)),
        )
      else {
        // for intermediate groups, return its last token
        val res = findTreeInGroup(groups.head, func)(tokenAfter)
        if (res.isDefined) res else iter(groups.tail)
      }
    getTemplateGroups(template).flatMap(iter)
  }

  def getBreakBeforeElsePolicy(beforeElse: FT): Policy = Policy
    .onlyFor(beforeElse, prefix = "ELSE")(
      Decision.onlyNewlinesWithFallback(_, Seq(Split(Newline, 0))),
    )

  def getBreakBeforeElsePolicy(term: Term.If): Policy = getElseToken(term)
    .flatMap { case (_, elsOpt) => elsOpt.map(getBreakBeforeElsePolicy) }

  def getBreaksBeforeElseChainPolicy(term: Term.If): Policy =
    getElseChain(term, Nil).foldLeft(Policy.noPolicy) { case (res, els) =>
      getBreakBeforeElsePolicy(els) ==> res
    }

  private final def getElseToken(term: Term.If): Option[(FT, Option[FT])] =
    tokenJustBeforeOpt(term.elsep).map { ftElsep =>
      val elsOpt = prevBeforeNonComment(ftElsep) match {
        case ft @ FT(_, _: T.KwElse, _) =>
          val ok = initStyle.newlines.alwaysBeforeElseAfterCurlyIf || ! {
            ft.leftOwner.is[Term.Block] && ft.left.is[T.RightBrace] &&
            !prevNonCommentSameLineBefore(ft).left.is[T.LeftBrace] &&
            matchingOptLeft(ft)
              .exists(lb => prev(lb).left.start < term.thenp.pos.start)
          }
          if (ok) Some(ft) else None
        case _ => None
      }
      (ftElsep, elsOpt)
    }

  @tailrec
  private final def getElseChain(term: Term.If, res: List[FT]): List[FT] =
    getElseToken(term) match {
      case Some((ftElsep, elsOpt)) =>
        val newRes = elsOpt.fold(res)(_ :: res)
        term.elsep match {
          case t: Term.If => getElseChain(t, newRes)
          case b @ Term.Block((t: Term.If) :: Nil)
              if !matchingOptRight(ftElsep).exists(_ eq getLast(b)) =>
            getElseChain(t, newRes)
          case _ => newRes
        }
      case _ => res
    }

  def insideInfixSplit(
      app: Member.Infix,
  )(implicit style: ScalafmtConfig, ft: FT): Seq[Split] = {
    val op = app.op.value
    if (app.is[Type] && style.spaces.neverAroundInfixTypes.contains(op))
      Seq(Split(NoSplit, 0))
    else {
      val isBeforeOp = ft.meta.leftOwner ne app.op
      // RETURNING!!!
      if (isBeforeOp && isFewerBracesLhs(app.lhs)) return Seq(Split(Newline, 0))
      def useSpaceBeforeArg = style.spaces.beforeInfixArgInParens(op) ||
        (app.arg match {
          case _: Lit.Unit => false
          case x: Member.ArgClause if x.values.lengthCompare(1) != 0 => false
          case x => !isEnclosedWithinParensOrBraces(x)
        })
      def useSpaceAroundOp = app.isAssignment || !isOperatorPart(op.head) ||
        op.length != 1 && !isOperatorPart(op.last) ||
        style.spaces.aroundSymbolicInfixOperators.forall(_.matches(op)) || {
          if (isBeforeOp) prevNonComment(ft).left match {
            case x: T.Ident => isOperatorPart(x.value.last)
            case _ => false
          }
          else nextNonComment(ft).right match {
            case x: T.Ident => isOperatorPart(x.value.head)
            case _ => false
          }
        }
      def spaceMod = Space(useSpaceAroundOp && (isBeforeOp || useSpaceBeforeArg))

      val afterInfix = style.newlines.infix.get(app)
      if (!afterInfix.isKeep)
        if (isBeforeOp) Seq(Split(spaceMod, 0))
        else {
          val (fullInfix, enclosedIn) = InfixSplits.findMaybeEnclosingInfix(app)
          val ok = enclosedIn.isDefined || fullInfix.parent.forall {
            case t: Defn.Val => t.rhs eq fullInfix
            case t: Defn.Var => t.body eq fullInfix
            case _ => true
          }
          if (ok) InfixSplits(app, ft, fullInfix)
            .getBeforeLhsOrRhs(afterInfix, spaceMod = spaceMod)
          else Seq(Split(spaceMod, 0))
        }
      else {
        // we don't modify line breaks generally around infix expressions
        // TODO: if that ever changes, modify how rewrite rules handle infix
        val (fullInfix, fullInfixEnclosedIn) = InfixSplits
          .findMaybeEnclosingInfix(app)
        val fullInfixEnclosedInParens = fullInfixEnclosedIn.exists(_.isRight)
        def okToBreak: Boolean = !isBeforeOp || fullInfixEnclosedInParens ||
          initStyle.dialect.allowInfixOperatorAfterNL ||
          (fullInfix.parent match {
            case Some(p: Case) => p.cond.contains(fullInfix)
            case _ => false
          }) || { // check if the break was in the original code
            val optokens = app.op.tokens
            val idx = optokens.rskipWideIf(_.is[T.Trivia], -1, Int.MinValue)
            optokens.getWideOpt(idx).contains(ft.left) // no rewritten tokens
          }
        val mod =
          if (ft.noBreak || !okToBreak) spaceMod
          else Newline2x(fullInfixEnclosedInParens && ft.hasBlankLine)
        def split(implicit fl: FileLine) = Split(mod, 0)
        if (isBeforeOp && isFewerBracesRhs(app.arg)) Seq(split)
        else Seq(InfixSplits.withNLIndent(split, app, fullInfix))
      }
    }
  }

  def getInfixSplitsBeforeLhs(
      lhsApp: Member.Infix,
      afterInfix: Newlines.Infix.Site,
      newStmtMod: Option[Modification] = None,
  )(implicit style: ScalafmtConfig, ft: FT): Seq[Split] = {
    val fullInfixTreeOpt = findTreeWithParentSimple(lhsApp, false)(isInfixApp)
    val fullInfix = fullInfixTreeOpt.flatMap(asInfixApp).getOrElse(lhsApp)
    val app = findLeftInfix(fullInfix)
    new InfixSplits(app, ft, fullInfix, app)
      .getBeforeLhsOrRhs(afterInfix, newStmtMod)
  }

  final def maybeGetInfixSplitsBeforeLhs(mod: => Option[Modification] = None)(
      nonInfixSplits: => Seq[Split],
  )(implicit style: ScalafmtConfig, ft: FT): Seq[Split] =
    asInfixApp(ft.meta.rightOwner).fold(nonInfixSplits) { ia =>
      val infixSite = style.newlines.infix.get(ia)
      if (infixSite.isKeep) nonInfixSplits
      else getInfixSplitsBeforeLhs(ia, infixSite, mod)
    }

  private[internal] object InfixSplits {

    def apply(app: Member.Infix, ft: FT)(implicit
        style: ScalafmtConfig,
    ): InfixSplits = apply(app, ft, findEnclosingInfix(app))

    def apply(app: Member.Infix, ft: FT, fullInfix: Member.Infix)(implicit
        style: ScalafmtConfig,
    ): InfixSplits = {
      val leftInfix = findLeftInfix(fullInfix)
      new InfixSplits(app, ft, fullInfix, leftInfix)
    }

    private def switch(splits: Seq[Split], triggers: T*): Seq[Split] = splits
      .map(x =>
        triggers.foldLeft(x) { case (y, trigger) => y.switch(trigger, false) },
      )

    @tailrec
    private def findMaybeEnclosingInfix(
        child: Member.Infix,
        childTree: Tree,
    ): (Member.Infix, Option[Either[FT, FT]]) = {
      val inParensOrBraces = getClosingIfWithinParensOrBraces(childTree)
      if (inParensOrBraces.isDefined) (child, inParensOrBraces)
      else childTree.parent match {
        case Some(p: Member.Infix) if !p.isAssignment =>
          findMaybeEnclosingInfix(p, p)
        case Some(p @ Member.ArgClause(_ :: Nil)) =>
          findMaybeEnclosingInfix(child, p)
        case Some(p @ Tree.Block(`childTree` :: Nil)) =>
          findMaybeEnclosingInfix(child, p)
        case _ => (child, None)
      }
    }

    private[FormatOps] def findMaybeEnclosingInfix(
        app: Member.Infix,
    ): (Member.Infix, Option[Either[FT, FT]]) = findMaybeEnclosingInfix(app, app)

    private[FormatOps] def findEnclosingInfix(app: Member.Infix): Member.Infix =
      findMaybeEnclosingInfix(app)._1

    def withNLIndent(
        split: Split,
    )(app: Member.Infix)(implicit ft: FT, style: ScalafmtConfig): Split =
      withNLIndent(split, app, findEnclosingInfix(app))

    def withNLIndent(
        split: Split,
        app: Member.Infix,
        fullInfix: => Member.Infix,
    )(implicit ft: FT, style: ScalafmtConfig): Split = {
      val noNL = !split.isNL && {
        val nextFt = nextNonCommentSameLine(ft)
        nextFt.eq(ft) || nextFt.noBreak
      }
      if (noNL) split else apply(app, ft, fullInfix).withNLIndent(split)
    }

  }

  private[internal] class InfixSplits(
      app: Member.Infix,
      ft: FT,
      fullInfix: Member.Infix,
      leftInfix: Member.Infix,
  )(implicit style: ScalafmtConfig) {
    private val isLeftInfix = leftInfix eq app
    private val isAfterOp = ft.meta.leftOwner eq app.op
    private val beforeLhs = !isAfterOp && ft.left.start < app.pos.start
    private val isFirstOp = beforeLhs || isLeftInfix
    private val fullExpire = getLastExceptParen(fullInfix)

    private val assignBodyExpire = {
      val prevFt = tokenBefore(fullInfix)
      val prevOwner = prevFt.meta.leftOwner
      prevFt.left match {
        case _: T.Equals => Some(getLast(prevOwner))
        case _: T.LeftParen | _: T.LeftBracket
            if fullInfix.parent.contains(prevOwner) && !(prevOwner match {
              case po: Member.ArgClause => po.parent.exists(isInfixApp)
              case po => isInfixApp(po)
            }) && isSeqSingle(getArgsOrNil(prevOwner)) => Some(getLast(fullInfix))
        case _ => None
      }
    }

    private val skipInfixIndent: Boolean = {
      @tailrec
      def getLastPat(t: Tree): Tree = t.parent match {
        case Some(p @ (_: Pat | _: Pat.ArgClause)) => getLastPat(p)
        case _ => t
      }
      def getChild = fullInfix match {
        case t: Pat => getLastPat(t)
        case t => t
      }
      @tailrec
      def isOldTopLevelWithParent(child: Tree)(p: Tree): Boolean = p match {
        case _: Term.If | _: Term.While | _: Source => true
        case Term.Block(_ :: rest) => rest.nonEmpty ||
          (p.parent match {
            case Some(pp) => p.tokens.head match { // check brace was not rewritten
                case head: T.LeftBrace => (tokens.before(head).left eq head) ||
                  isOldTopLevelWithParent(p)(pp)
                case _ => true
              }
            case None => true
          })
        case fun: Term.FunctionTerm => isBlockFunction(fun)
        case t: Case => t.pat.eq(child) || t.body.eq(child)
        case SingleArgInBraces(_, arg, _) => child eq arg
        case _ => false
      }
      def isOldTopLevel(child: Tree) = child.parent
        .exists(isOldTopLevelWithParent(child))
      def isAloneEnclosed(child: Tree) = child.parent.exists {
        case p: Case => p.pat eq child
        case p: Term.If => p.cond eq child
        case p: Term.While => p.expr eq child
        case p: Term.Do => p.expr eq child
        case p: Term.Block => hasSingleElement(p, child)
        case p: Term.FunctionTerm => isBlockFunction(p)
        case p @ Member.ArgClause(`child` :: Nil) => isEnclosedInMatching(p)
        case Member.Tuple(`child` :: Nil) => true
        case _ => false
      }
      def isAloneArgOrBody(child: Tree) = child.parent.exists {
        case t: Case => t.pat.eq(child) || t.body.eq(child)
        case _: Term.If | _: Term.While | _: Term.Do => true
        case _: Member.ArgClause => true
        case p: Term.Block => hasSingleElement(p, child)
        case _: Init | _: Term.Super | _: Member.Tuple => true
        case t: Tree.WithBody => t.body eq child
        case t: Term.Param => t.default.contains(child)
        case _ => false
      }
      val cfg = style.indent.infix
      def allowNoIndent = cfg.exemptScope match {
        case IndentOperator.Exempt.all => true
        case IndentOperator.Exempt.oldTopLevel => isOldTopLevel(getChild)
        case IndentOperator.Exempt.aloneEnclosed => isAloneEnclosed(getChild)
        case IndentOperator.Exempt.aloneArgOrBody => isAloneArgOrBody(getChild)
        case IndentOperator.Exempt.notAssign => isAfterAssignmentOp(false)
        case IndentOperator.Exempt.notWithinAssign => !app.isAssignment &&
          // fullInfix itself is never an assignment
          fullInfix.parent.exists {
            case _: Member.Infix => false
            case p: Member.ArgClause => !p.parent.is[Member.Infix]
            case _ => true
          }
      }
      if (beforeLhs) assignBodyExpire.isEmpty
      else app.is[Pat] || allowNoIndent && cfg.noindent(app.op.value)
    }

    private val fullIndent: Indent = assignBodyExpire match {
      case Some(x) if beforeLhs => Indent(style.indent.main, x, ExpiresOn.After)
      case None if isLeftInfix && isAfterAssignmentOp(true) =>
        Indent(style.indent.main, fullExpire, ExpiresOn.After)
      case _ =>
        val len = style.indent.getAfterInfixSite
        Indent(len, fullExpire, ExpiresOn.After)
    }

    val (nlIndent, nlPolicy) = {
      def policy(triggers: T*) = Policy ? triggers.isEmpty ||
        Policy.onLeft(fullExpire, prefix = "INF") {
          case Decision(t: FT, s)
              if isInfixOp(t.meta.leftOwner) || AsInfixOp(t.meta.rightOwner)
                .exists(style.newlines.infix.keep) =>
            InfixSplits.switch(s, triggers: _*)
        }

      val fullTok = getIndentTrigger(fullInfix)
      val noAssign = assignBodyExpire.isEmpty
      if (!noAssign && beforeLhs) (fullIndent, policy(fullTok))
      else if (skipInfixIndent)
        if (noAssign) (Indent.Empty, Policy.NoPolicy)
        else (Indent.before(fullIndent, fullTok), policy(fullTok))
      else {
        val opTok = getIndentTrigger(leftInfix.op)
        val ind = if (isFirstOp) fullIndent else Indent.before(fullIndent, opTok)
        if (noAssign) (ind, policy(opTok))
        else (Indent.Switch(fullIndent, fullTok, ind), policy(fullTok, opTok))
      }
    }

    @inline
    private def isAfterAssignmentOp(isAssignment: Boolean): Boolean =
      isAfterOp && app.isAssignment == isAssignment

    private def withNLIndent(split: Split): Split = split.withIndent(nlIndent)
      .andPolicy(nlPolicy)

    def getBeforeLhsOrRhs(
        afterInfix: Newlines.Infix.Site,
        newStmtMod: Option[Modification] = None,
        spaceMod: Modification = Space,
    )(implicit ft: FT): Seq[Split] = {
      val maxPrecedence =
        if (isAfterOp) infixSequenceMaxPrecedence(fullInfix) else 0 // 0 unused
      val breakPenalty = if (isAfterOp) maxPrecedence - app.precedence else 1

      val closeOpt = matchingOptRight(ft)
      val finalExpireCost = fullExpire -> 0
      val expires =
        if (closeOpt.isDefined) finalExpireCost :: Nil
        else {
          val res = mutable.Buffer.empty[Member.Infix]
          findNextInfixes(fullInfix, app.lhs, res)(
            if (!afterInfix.breakOnNested) _ => true
            else x => !isEnclosedWithinParensOrBraces(x.lhs),
          )
          val infixes = if (isAfterOp) res.toSeq.drop(1) else res.toSeq
          if (infixes.isEmpty) finalExpireCost :: Nil
          else {
            val out = new mutable.ListBuffer[(FT, Int)]
            var minCost = Int.MaxValue
            infixes.foreach { ia =>
              val cost = maxPrecedence - ia.precedence
              if (cost < minCost) {
                out += getMidInfixToken(ia) -> cost
                minCost = cost
              }
            }
            if (0 < minCost) out += finalExpireCost
            out.toList
          }
        }

      val infixTooLong = infixSequenceLength(fullInfix) >
        afterInfix.maxCountPerExprForSome
      val breakMany = infixTooLong || (afterInfix.style eq Newlines.Infix.many)
      val rightAsInfix = asInfixApp(ft.meta.rightOwner)

      def breakAfterComment(t: FT) = {
        val end = nextNonCommentSameLine(t)
        Policy ? end.right.isAny[T.LeftBrace, T.Comment] || {
          if (end eq t) decideNewlinesOnlyAfterToken(end)
          else decideNewlinesOnlyAfterClose(end)
        }
      }
      val nlMod = newStmtMod
        .getOrElse(Space.orNL(ft.noBreak && ft.right.is[T.Comment]))
      val delayedBreak = Policy ? nlMod.isNL || breakAfterComment(ft)

      val (singleLineExpire, singleLineIndent) = {
        val skip = skipInfixIndent
        if (isFirstOp) (fullExpire, if (skip) Indent.Empty else fullIndent)
        else {
          val expire = expires.head._1
          val indentLen = if (skip) 0 else style.indent.main
          val indent = Indent(indentLen, expire, ExpiresOn.After)
          (expire, indent)
        }
      }

      val singleLinePolicy = Policy ? (infixTooLong || !isFirstOp) ||
        getSingleLineInfixPolicy(fullExpire)
      val nlSinglelineSplit = Split(nlMod, 0)
        .onlyIf(singleLinePolicy.nonEmpty && !isAfterOp)
        .withIndent(singleLineIndent).withSingleLine(singleLineExpire)
        .andPolicy(singleLinePolicy).andPolicy(delayedBreak)
      val spaceSingleLine = Split(spaceMod, 0).onlyIf(newStmtMod.isEmpty)
        .withSingleLine(singleLineExpire).andPolicy(singleLinePolicy)
      val singleLineSplits = Seq(
        spaceSingleLine.onlyFor(SplitTag.InfixChainNoNL),
        spaceSingleLine.onlyIf(singleLinePolicy.nonEmpty),
        nlSinglelineSplit,
      )

      val otherSplits = closeOpt.fold {
        val nlSplit = Split(nlMod, 1 + breakPenalty)
        Seq(nlSplit.withIndent(nlIndent).withPolicy(nlPolicy & delayedBreak))
      } { closeFt =>
        val noSingleLine = newStmtMod.isDefined || breakMany ||
          rightAsInfix.exists(10 < infixSequenceLength(_))
        val nextOp =
          if (!afterInfix.breakOnNested) None
          else if (!isAfterOp) Some(app.op)
          else getInfixRhsAsInfix(app) match {
            case Some(ia) => Some(findLeftInfix(ia).op)
            case _ => findNextInfixInParent(app, fullInfix)
          }
        val endOfNextOp = nextOp.map(getLast)
        val breakAfterClose: Policy = endOfNextOp.map(breakAfterComment)

        val nlSplit = Split(nlMod, 0).andPolicy(breakAfterClose)
          .withIndent(nlIndent).withPolicy(nlPolicy)
        val singleLineSplit = Split(spaceMod, 0).notIf(noSingleLine)
          .withSingleLine(endOfNextOp.getOrElse(closeFt))
          .andPolicy(breakAfterClose).andPolicy(getSingleLineInfixPolicy(closeFt))
        Seq(singleLineSplit, nlSplit)
      }

      val spaceSplits: Seq[Split] =
        if (ft.right.is[T.Comment]) Seq.empty
        else if (closeOpt.isDefined) Seq.empty
        else {
          val nextFT = if (rightAsInfix.isDefined) next(ft) else ft
          expires.filter(_._2 <= breakPenalty).takeRight(3)
            .map { case (expire, cost) =>
              val exclude =
                if (breakMany) TokenRanges.empty
                else insideBracesBlock(nextFT, expire, true)
              val ignore = exclude.isEmpty && singleLinePolicy.nonEmpty &&
                (expire eq fullExpire)
              Split(ignore, cost)(ModExt(newStmtMod.getOrElse(spaceMod)))
                .withSingleLine(expire, exclude, noOptimal = cost != 0)
            }
        }

      singleLineSplits ++ spaceSplits ++ otherSplits
    }

  }

  def getSingleLineInfixPolicy(end: FT) = Policy
    .onLeft(end, prefix = "INFSLB", terminal = true) {
      case Decision(t: FT, s) if isInfixOp(t.meta.leftOwner) =>
        SplitTag.InfixChainNoNL.activateOnly(s)
    }

  def getMidInfixToken(app: Member.Infix): FT = {
    val opToken = getHead(app.op)
    val beforeOp = prev(opToken)
    val lhsLast = prevNonComment(beforeOp)
    if (beforeOp eq lhsLast) opToken else lhsLast
  }

  @tailrec
  private def findNextInfixes(
      fullTree: Tree,
      tree: Tree,
      res: mutable.Buffer[Member.Infix],
  )(pred: Member.Infix => Boolean): Boolean = (tree ne fullTree) &&
    (tree.parent match {
      case Some(ia: Member.Infix) =>
        val ok = (ia.lhs ne tree) || pred(ia) && {
          res += ia
          findNestedInfixes(res)(pred)(ia.arg)
        }
        ok && findNextInfixes(fullTree, ia, res)(pred)
      case Some(p: Member.ArgClause) => p.parent match {
          case Some(pp: Member.Infix) => findNextInfixes(fullTree, pp, res)(pred)
          case _ => true
        }
      case Some(p @ Tree.Block(`tree` :: Nil)) if !isEnclosedInBraces(p) =>
        findNextInfixes(fullTree, p, res)(pred)
      case _ => true
    })

  private def findNestedInfixes(
      res: mutable.Buffer[Member.Infix],
  )(pred: Member.Infix => Boolean)(tree: Tree): Boolean = tree match {
    case Member.ArgClause(arg :: Nil)
        if !isEnclosedWithinParensOrBraces(tree) =>
      findNestedInfixes(res)(pred)(arg match {
        case Tree.Block(x :: Nil) => x
        case x => x
      })
    case ia: Member.Infix if !isEnclosedWithinParens(tree) =>
      findNestedInfixes(res)(pred)(ia.lhs) && pred(ia) && {
        res += ia
        ia.singleArg match {
          case None => true
          case Some(arg) => findNestedInfixes(res)(pred)(arg)
        }
      }
    case Tree.Block(arg :: Nil) if !isEnclosedWithinParensOrBraces(tree) =>
      findNestedInfixes(res)(pred)(arg)
    case _ => true
  }

  @tailrec
  final def findLeftInfix(app: Member.Infix): Member.Infix = app.lhs match {
    case ia: Member.Infix if !isEnclosedWithinParens(ia) => findLeftInfix(ia)
    case b @ Tree.Block((ia: Member.Infix) :: Nil)
        if !isEnclosedWithinParensOrBraces(b) => findLeftInfix(ia)
    case _ => app
  }

  private def getInfixRhsAsInfix(app: Member.Infix): Option[Member.Infix] =
    app.singleArg match {
      case Some(t: Member.Infix) if !isEnclosedWithinParens(t) => Some(t)
      case _ => None // multiple parameters to infix are always enclosed
    }

  private def infixSequenceMaxPrecedence(app: Member.Infix): Int = {
    val queue = new mutable.Queue[Member.Infix]()
    queue += app
    var maxPrecedence = 0
    while (queue.nonEmpty) {
      val elem = queue.dequeue()
      val elemPrecedence = elem.precedence
      if (maxPrecedence < elemPrecedence) maxPrecedence = elemPrecedence
      queue ++= elem.nestedInfixApps.filter(x => !isEnclosedWithinParens(x))
    }
    maxPrecedence
  }

  def functionExpire(function: Term.FunctionTerm): (FT, ExpiresOn) =
    function.parent match {
      case Some(SingleArgInBraces.OrBlock(_, _, e)) => e -> ExpiresOn.Before
      case _ => getLastExceptParen(function) -> ExpiresOn.After
    }

  def mustForceConfigStyle(ft: FT)(implicit
      cfg: Newlines.ConfigStyleElement,
  ): Boolean = cfg.getForceIfOptimized && forceConfigStyle(ft.meta.idx)

  def preserveConfigStyle(ft: FT, breakBeforeClose: => Boolean)(implicit
      style: ScalafmtConfig,
      clauseSiteFlags: ClauseSiteFlags,
  ): Boolean = preferConfigStyle &&
    couldPreserveConfigStyle(ft, breakBeforeClose)

  def couldPreserveConfigStyle(ft: FT, breakBeforeClose: => Boolean)(implicit
      style: ScalafmtConfig,
      clauseSiteFlags: ClauseSiteFlags,
  ): Boolean = !style.newlines.sourceIgnored && {
    !dangleCloseDelim && !alignOpenDelim || ft.hasBreak ||
    (next(ft).hasBreak || style.newlines.forceAfterImplicitParamListModifier) &&
    opensConfigStyleImplicitParamList(ft)
  } && breakBeforeClose

  /** Works for `using` as well */
  def opensConfigStyleImplicitParamList(formatToken: FT)(implicit
      style: ScalafmtConfig,
  ): Boolean = soft.ImplicitOrUsing(formatToken.right) &&
    style.newlines.notBeforeImplicitParamListModifier &&
    hasImplicitParamList(formatToken.meta.rightOwner)

  def isSingleIdentifierAnnotation(tok: FT): Boolean = {
    val toMatch =
      if (tok.right.is[T.RightParen])
        // Hack to allow any annotations with arguments like @foo(1)
        tokens(matchingRight(tok), -2)
      else tok
    toMatch match {
      case FT(T.At(), _: T.Ident, _) => true
      case _ => false
    }
  }

  def typeTemplateSplits(template: Template, indentIfSecond: Int)(implicit
      fileLine: FileLine,
      ft: FT,
      style: ScalafmtConfig,
  ): Seq[Split] = {
    def getPolicy(expire: FT) = expire.left match {
      case lb: T.LeftBrace if template.body.selfOpt.isEmpty =>
        Policy.onRight(expire, "MLTMPL") {
          // Force template to be multiline.
          case d @ Decision(ftd @ FT(`lb`, right, _), _)
              if !right.is[T.RightBrace] && // corner case, body is {}
                !isAttachedCommentThenBreak(ftd) =>
            d.onlyNewlinesWithoutFallback
        }
      case _ => NoPolicy
    }
    findTemplateGroupOnRight { x =>
      // this method is called on a `with` or comma; hence, it can
      // only refer to second or subsequent init/derive in a group
      // we'll indent only the second, but not any subsequent ones
      val expire = x.getExpireToken
      val indent =
        if (!x.isSecond) Indent.Empty
        else Indent(indentIfSecond, expire, ExpiresOn.After)
      def nlSplit(cost: Int) = Split(Newline, cost).withPolicy(getPolicy(expire))
        .withIndent(indent)
      if (!style.binPack.keepParentConstructors)
        Seq(Split(Space, 0).withIndent(indent), nlSplit(1))
      else if (ft.hasBreak) Seq(nlSplit(0))
      else {
        val slbEnd = getLast(x.superType)
        val exclude = insideBlock[T.LeftParen](ft, slbEnd)
        Seq(
          Split(Space, 0).withIndent(indent)
            .withSingleLine(slbEnd, exclude = exclude, noSyntaxNL = true),
          nlSplit(1),
        )
      }
    }(template).getOrElse(
      Seq(Split(Space, 0)), // shouldn't happen
    )
  }

  def binPackParentConstructorSplits(
      isFirstCtor: Boolean,
      owners: => Set[Tree],
      rhs: => Option[Tree],
      lastFt: FT,
      indentLen: Int,
      extendsThenWith: => Boolean = false,
  )(implicit fileLine: FileLine, ft: FT, style: ScalafmtConfig): Seq[Split] = {
    val nlMod = Newline.withAlt(Space)
    def nlPolicy(ignore: Boolean) = Policy ? (ignore || owners.isEmpty) ||
      Policy.onRight(lastFt, prefix = "WITH") {
        case d @ Decision(FT(_, _: T.KwWith, m), _) if owners(m.rightOwner) =>
          d.onlyNewlinesWithoutFallback
        case d @ Decision(FT(_: T.Comma, _, m), _) if owners(m.leftOwner) =>
          d.onlyNewlinesWithoutFallback
      }
    val indent =
      if (!isFirstCtor) Indent.Empty
      else Indent(indentLen, lastFt, ExpiresOn.After)
    if (style.binPack.keepParentConstructors)
      if (ft.hasBreak) Seq(Split(nlMod, 0).withIndent(indent))
      else {
        val slbEnd = getSlbEndOnLeft(rhs.fold(lastFt)(getLast))
        Seq(
          Split(Space, 0).withIndent(indent).withSingleLine(
            slbEnd,
            exclude = insideBracesBlock(ft, slbEnd, true),
            noSyntaxNL = extendsThenWith,
          ),
          Split(nlMod, 1).withIndent(indent),
        )
      }
    else if (!isFirstCtor) Seq(Split(Space, 0), Split(Newline, 1))
    else if (style.binPack.parentConstructors eq BinPack.ParentCtors.ForceBreak)
      Seq(Split(nlMod, 0, policy = nlPolicy(false)).withIndent(indent))
    else {
      val parentCtors = style.binPack.parentConstructors
      val nlOnelineTag = parentCtors match {
        case BinPack.ParentCtors.source => Right(style.newlines.fold)
        case BinPack.ParentCtors.Oneline => Right(true)
        case BinPack.ParentCtors.OnelineIfPrimaryOneline =>
          Left(SplitTag.OnelineWithChain)
        case _ => Right(false)
      }
      val isAlways = parentCtors eq BinPack.ParentCtors.Always
      val exclude =
        if (isAlways) insideBracesBlock(ft, lastFt, true) else TokenRanges.empty
      val noSyntaxNL = extendsThenWith
      val pnlPolicy = PenalizeAllNewlines(lastFt, 1, noSyntaxNL = noSyntaxNL)
      val slbEnd = getSlbEndOnLeft(lastFt)
      Seq(
        Split(Space, 0)
          .withSingleLine(slbEnd, exclude = exclude, noSyntaxNL = noSyntaxNL)
          .orPolicy(pnlPolicy).withIndent(indent),
        Split(nlMod, 0).onlyIf(nlOnelineTag != Right(false))
          .preActivateFor(nlOnelineTag.left.toOption)
          .withSingleLineNoOptimal(lastFt, noSyntaxNL = noSyntaxNL)
          .withIndent(indent),
        Split(nlMod, 1, policy = nlPolicy(isAlways) & pnlPolicy)
          .withIndent(indent),
      )
    }
  }

  def getForceConfigStyle: (Set[Int], Set[Int]) = {
    val callSite = initStyle.runner.optimizer.callSite
    val defnSite = initStyle.runner.optimizer.defnSite
    if (callSite.isEnabled || defnSite.isEnabled) {
      val clearQueues = Set.newBuilder[Int]
      val forces = Set.newBuilder[Int]
      def process(clause: Member.SyntaxValuesClause, ftOpen: FT)(
          cfg: ScalafmtOptimizer.ClauseElement,
      ): Unit = if (cfg.isEnabled) {
        val openIdx = ftOpen.idx
        matchingOpt(openIdx).foreach { close =>
          val values = clause.values
          if (
            values.lengthCompare(cfg.minCount) >= 0 &&
            (cfg.minSpan == 0 || cfg.minSpan <= span(openIdx, close.idx))
          ) {
            forces += openIdx
            values.foreach(x => clearQueues += getHead(x).idx)
          }
        }
      }
      tokens.foreach {
        case ft @ FT(_: T.LeftParen | _: T.LeftBracket, _, m) =>
          m.leftOwner match {
            case t: Member.ArgClause if !t.parent.is[Member.Infix] =>
              process(t, ft)(callSite)
            case t: Member.ParamClause => process(t, ft)(defnSite)
            case t: Type.FuncParamClause => process(t, ft)(defnSite)
            case _ =>
          }
        case _ =>
      }
      (forces.result(), clearQueues.result())
    } else (Set.empty, Set.empty)
  }

  /** Implementation for `verticalMultiline`
    */
  def verticalMultiline()(implicit
      style: ScalafmtConfig,
      ft: FT,
  ): Seq[Split] = {
    val lpOwner = ft.meta.leftOwner

    val FT(open, r, _) = ft
    val nft = next(ft)
    val close = matchingLeft(ft)
    val beforeClose = prev(close)
    val indentParam = style.indent.getDefnSite(lpOwner)
    val indentSep = Indent((indentParam - 2).max(0), close, ExpiresOn.After)
    val isBracket = open.is[T.LeftBracket]

    def getClausesFromClauseGroup(tree: Member.ParamClauseGroup) = {
      val tparams = tree.tparamClause
      val params = tree.paramClauses
      if (tparams.isEmpty) params else tparams :: params
    }
    val allClauses = lpOwner match {
      case ParamClauseParent(p) => p match {
          case t: Tree.WithParamClauseGroups => t.paramClauseGroups
              .flatMap(getClausesFromClauseGroup)
          case t: Tree.WithParamClauseGroup => t.paramClauseGroup
              .map(getClausesFromClauseGroup).getOrElse(Nil)
          case t: Stat.WithCtor with Tree.WithTParamClause => t.tparamClause +:
              t.ctor.paramClauses
          case t: Tree.WithParamClauses => t.paramClauses
          case t: Stat.WithCtor => t.ctor.paramClauses
          case t: Tree.WithTParamClause => t.tparamClause :: Nil
          case _ => Nil
        }
      case _ => Nil
    }
    val allParenOwners = allClauses.filter(_.pos.start > open.start)
    val allOwners = lpOwner +: allParenOwners

    // find the last param on the defn so that we can apply our `policy`
    // till the end.
    val lastParens = allParenOwners.map(getLast)
    val lastParen = lastParens.lastOption.getOrElse(close)

    val shouldNotDangle =
      !style.danglingParentheses.atVerticalMultilineSite(lpOwner)

    // Since classes and defs aren't the same (see below), we need to
    // create two (2) OneArgOneLineSplit when dealing with classes. One
    // deals with the type params and the other with the value params.
    val oneLinePerArg = allOwners.zip(close +: lastParens)
      .map { case (owner, end) => splitOneArgOneLine(end, owner) }
      .reduceRight(_ ==> _)

    // DESNOTE(2017-03-28, pjrt) Classes and defs aren't the same.
    // For defs, type params and value param have the same `owners`. However
    // this is not the case for classes. Type params have the class itself
    // as the owner, but value params have the Ctor as the owner, so a
    // simple check isn't enough. Instead we check against the owner of the
    // `lastParen` as well, which will be the same as the value param's
    // owner.

    val beforeLastParen =
      if (!shouldNotDangle) null
      else Some(prev(lastParen)).filterNot(isLeftCommentThenBreak).orNull
    val paramGroupSplitter = Policy.onLeft(lastParen, prefix = "VML") {
      // If this is a class, then don't dangle the last paren unless the line ends with a comment
      case Decision(`beforeLastParen`, _) => Seq(Split(NoSplit, 0))
      // Indent separators `)(` and `](` by `indentSep`
      case Decision(`beforeClose`, _) =>
        Seq(Split(Newline, 0).withIndent(indentSep))
      case Decision(FT(LeftParenOrBracket(), _, m), ss)
          if allParenOwners.contains(m.leftOwner) =>
        ss.filter(!_.isActiveFor(SplitTag.VerticalMultilineSingleLine))
      case Decision(ftd @ FT(soft.ImplicitOrUsing(), _, m), _)
          if style.newlines.forceAfterImplicitParamListModifier &&
            !isRightCommentThenBreak(ftd) &&
            hasImplicitParamList(m.leftOwner) => Seq(Split(Newline, 0))
    }

    // Our policy is a combination of OneArgLineSplit and a custom splitter
    // for parameter groups.
    val policy = oneLinePerArg | paramGroupSplitter

    val firstIndent =
      if (r.is[T.RightParen]) indentSep // An empty param group
      else Indent(indentParam, close, ExpiresOn.Before)

    def aboveArityThreshold = {
      val threshold = style.verticalMultiline.arityThreshold
      allOwners.exists {
        case Member.ParamClause(v) => v.lengthCompare(threshold) >= 0
        case _ => false
      }
    }

    val space = Space(style.spaces.inParentheses)
    val slbEnd =
      if (style.newlines.sometimesBeforeColonInMethodReturnType) lastParen
      else if (lastParen.right.is[T.Colon]) next(lastParen)
      else lastParen
    val slbSplit = Split(space, 0).withSingleLine(slbEnd)
      .preActivateFor(SplitTag.VerticalMultilineSingleLine)

    if (isBracket) {
      val noSlbPolicy = Policy.onLeft(lastParen, prefix = "VML!SLB") {
        case Decision(FT(LeftParenOrBracket(), _, m), ss)
            if allParenOwners.contains(m.leftOwner) =>
          ss.filter(!_.isActiveFor(SplitTag.VerticalMultilineSingleLine))
      }
      val noSplit =
        if (allParenOwners.isEmpty) Split(space, 0).withSingleLine(close)
        else {
          val lpNext = getHead(allParenOwners.head)
          val slbPolicy = Policy ? isRightCommentThenBreak(lpNext) ||
            decideNewlinesOnlyAfterToken(lpNext)
          // If we can fit the type params, make it so
          Split(space, 0).withSingleLine(lpNext).orPolicy(slbPolicy)
        }
      val nlSplit = Split(Newline, 1, policy = policy).withIndent(firstIndent)
      Seq(slbSplit, noSplit.andPolicy(noSlbPolicy), nlSplit)
    } else {
      val rightIsImplicit = soft.ImplicitOrUsing(r)
      val implicitNL = rightIsImplicit &&
        style.newlines.forceBeforeImplicitParamListModifier
      val implicitParams =
        if (rightIsImplicit) getImplicitParamList(ft.meta.rightOwner)
          .fold(Nil: List[Tree])(_.values)
        else Nil
      val noSlb = implicitNL || aboveArityThreshold ||
        ft.hasBreak && !style.newlines.sourceIgnored &&
        style.newlines.configStyle.getDefnSite(isBracket).prefer ||
        implicitParams.nonEmpty &&
        style.newlines.forceAfterImplicitParamListModifier
      val nlNoAlt = implicitNL ||
        !rightIsImplicit && style.verticalMultiline.newlineAfterOpenParen
      val nlMod = Newline.withAltIf(!nlNoAlt)(slbSplit.modExt)
      val spaceImplicit = !implicitNL && implicitParams.lengthCompare(1) > 0 &&
        style.newlines.notBeforeImplicitParamListModifier
      Seq(
        // If we can fit all in one block, make it so
        slbSplit.notIf(noSlb),
        Split(space, 0, policy = policy).onlyIf(spaceImplicit).andPolicy(
          decideNewlinesOnlyAfterClose(nft),
          isRightCommentThenBreak(nft),
        ).withIndent(firstIndent),
        // Otherwise split vertically
        Split(nlMod, 1, policy = policy).withIndent(firstIndent),
      )
    }

  }

  // Returns leading comment, if there exists one, otherwise formatToken
  @inline
  final def leadingComment(tree: Tree): FT = leadingComment(tokenJustBefore(tree))
  @tailrec
  final def leadingComment(ft: FT): FT =
    if (ft.hasBlankLine || !ft.left.is[T.Comment]) ft
    else {
      val pft = prevNonCommentSameLineBefore(ft)
      if (pft.noBreak) ft else leadingComment(pft)
    }

  // Returns trailing comment, if there exists one, otherwise formatToken
  @inline
  final def trailingComment(ft: FT, end: Int): FT = {
    @inline
    def isDone(x: FT) = x.hasBlankLine || x.right.end >= end
    @tailrec
    def iter(x: FT): FT = {
      val nft = nextNonCommentSameLineAfter(x)
      if (isDone(nft)) nft
      else if (!nft.right.is[T.Comment]) ft // original
      else iter(nft)
    }
    if (!ft.right.is[T.Comment] || isDone(ft)) ft else iter(ft)
  }

  def getSpaceAndNewlineAfterCurlyLambda(newlines: Int)(implicit
      style: ScalafmtConfig,
  ): (Boolean, NewlineT) = style.newlines.afterCurlyLambdaParams match {
    case Newlines.AfterCurlyLambdaParams.squash => (true, Newline)
    case Newlines.AfterCurlyLambdaParams.never =>
      (style.newlines.okSpaceForSource(newlines), Newline)
    case Newlines.AfterCurlyLambdaParams.always => (false, Newline2x)
    case Newlines.AfterCurlyLambdaParams.preserve =>
      val blanks = newlines >= 2
      (style.newlines.okSpaceForSource(newlines, !blanks), Newline2x(blanks))
  }

  def getNoSplitAfterOpening(
      ft: FT,
      commentNL: Modification,
      spaceOk: Boolean = true,
  )(implicit style: ScalafmtConfig): Modification = ft.right match {
    case _: T.Comment =>
      val isDetachedSlc = ft.hasBreak && isBreakAfterRight(ft)
      if (isDetachedSlc || ft.rightHasNewline) commentNL else Space
    case _: T.LeftParen if ft.meta.rightOwner eq ft.meta.leftOwner => NoSplit
    case _ => Space(spaceOk && style.spaces.inParentheses)
  }

  def getNoSplitBeforeClosing(
      ft: FT,
      commentNL: Modification,
      spaceOk: Boolean = true,
  )(implicit style: ScalafmtConfig): Modification = ft.left match {
    case _: T.Comment =>
      val isDetachedSlc = ft.hasBreak && prev(ft).hasBreak
      if (isDetachedSlc || ft.leftHasNewline) commentNL else Space
    case _: T.RightParen if ft.meta.rightOwner eq ft.meta.leftOwner => NoSplit
    case _ => Space(spaceOk && style.spaces.inParentheses)
  }

  // look for arrow before body, if any, else after params
  def getFuncArrow(term: Term.FunctionTerm): Option[FT] = tokens
    .tokenBeforeOpt(term.body)
    .orElse(tokenAfterOpt(term.paramClause).map(getArrowAfter[T.FunctionArrow]))

  // look for arrow before body, if any, else after cond/pat
  def getCaseArrow(term: Case): FT = tokenBeforeOpt(term.body)
    .getOrElse(getArrowAfter[T.RightArrow](tokenAfter(
      term.cond.getOrElse(term.pat),
    )))

  // look for arrow before body, if any, else after cond/pat
  def getCaseArrow(term: TypeCase): FT = next(tokenAfter(term.pat))

  private def getArrowAfter[A](ft: FT)(implicit f: Classifier[T, A]): FT = {
    val maybeArrow = next(ft)
    if (f(maybeArrow.left)) maybeArrow else nextAfterNonComment(maybeArrow)
  }

  @tailrec
  final def findPrevSelectAndApply(
      tree: Tree,
      enclosed: Boolean,
      applyTree: Option[Member.Apply] = None,
  ): (Option[SelectLike], Option[Member.Apply]) = {
    @inline
    def isEnclosed: Boolean = enclosed && isEnclosedWithinParens(tree)
    tree match {
      case GetSelectLike(t) if !isEnclosed => (Some(t), applyTree)
      case t: Member.Apply if !isEnclosed =>
        findPrevSelectAndApply(t.fun, enclosed, applyTree.orElse(Some(t)))
      case t: Term.AnonymousFunction if !enclosed =>
        findPrevSelectAndApply(t.body, false, applyTree)
      case Term.Block(t :: Nil) if !isEnclosedInBraces(tree) =>
        findPrevSelectAndApply(t, false, applyTree)
      case _ => (None, applyTree)
    }
  }

  def findPrevSelect(
      tree: SelectLike,
      enclosed: Boolean = true,
  ): Option[SelectLike] = findPrevSelectAndApply(tree.qual, enclosed)._1

  @tailrec
  final def findFirstSelect(
      tree: Tree,
      enclosed: Boolean,
      select: Option[SelectLike] = None,
  ): Option[SelectLike] = findPrevSelectAndApply(tree, enclosed) match {
    case (x @ Some(prevSelect), _) =>
      findFirstSelect(prevSelect.qual, enclosed, x)
    case _ => select
  }

  @tailrec
  private def findLastApplyAndNextSelectEnclosed(
      tree: Tree,
      select: Option[SelectLike] = None,
      prevApply: Option[Tree] = None,
  ): (Tree, Option[SelectLike]) =
    if (isEnclosedWithinParens(tree)) (prevApply.getOrElse(tree), select)
    else tree.parent match {
      case Some(GetSelectLike(p)) =>
        findLastApplyAndNextSelectEnclosed(p.tree, select.orElse(Some(p)))
      case Some(p: Member.Apply) if p.fun eq tree =>
        findLastApplyAndNextSelectEnclosed(p, select)
      case Some(p @ Term.Block(`tree` :: Nil)) if !isEnclosedInBraces(p) =>
        findLastApplyAndNextSelectEnclosed(p, select, prevApply.orElse(Some(tree)))
      case _ => (prevApply.getOrElse(tree), select)
    }

  @tailrec
  private def findLastApplyAndNextSelectPastEnclosed(
      tree: Tree,
      select: Option[SelectLike] = None,
      prevEnclosed: Option[Tree] = None,
      prevApply: Option[Tree] = None,
  ): (Tree, Option[SelectLike]) = tree.parent match {
    case Some(GetSelectLike(p)) =>
      findLastApplyAndNextSelectPastEnclosed(p.tree, select.orElse(Some(p)))
    case Some(p: Member.Apply) if p.fun eq tree =>
      prevEnclosed match {
        case Some(t) => (t, select)
        case _ =>
          val nextEnclosed = prevApply.orElse(Some(tree))
            .filter(isEnclosedWithinParens)
          findLastApplyAndNextSelectPastEnclosed(p, select, nextEnclosed)
      }
    case Some(p: Term.AnonymousFunction) =>
      findLastApplyAndNextSelectPastEnclosed(p, select, Some(p))
    case Some(p @ Term.Block(`tree` :: Nil)) if !isEnclosedInBraces(p) =>
      val nextApply = prevApply.orElse(Some(tree))
      findLastApplyAndNextSelectPastEnclosed(p, select, prevEnclosed, nextApply)
    case _ => (prevEnclosed.orElse(prevApply).getOrElse(tree), select)
  }

  final def findLastApplyAndNextSelect(
      tree: Tree,
      enclosed: Boolean,
  ): (Tree, Option[SelectLike]) =
    if (enclosed) findLastApplyAndNextSelectEnclosed(tree)
    else findLastApplyAndNextSelectPastEnclosed(tree)

  def canStartSelectChain(
      thisSelectLike: SelectLike,
      nextSelectLike: Option[SelectLike],
      lastApply: Tree,
  )(implicit style: ScalafmtConfig): Boolean = {
    val thisTree = thisSelectLike.tree
    val nextSelect = nextSelectLike.map(_.tree)
    val ok = thisTree.ne(lastApply) &&
      !cannotStartSelectChainOnExpr(thisSelectLike.qual)
    val cfg = style.newlines.selectChains
    def checkParent = thisTree.parent match {
      case `nextSelect` => cfg.classicCanStartWithoutApply
      case Some(p: Term.Apply) if getHead(p.argClause).left.is[T.LeftBrace] =>
        cfg.classicCanStartWithBraceApply && !nextSelect.contains(lastApply) // exclude short curly
      case Some(p: Member.Apply) => p.fun eq thisTree
      case _ => false
    }
    ok &&
    (thisTree match {
      case _: Term.SelectMatch => // like select and apply in one
        !tokenAfter(thisSelectLike.nameFt).right.is[T.LeftBrace] ||
        cfg.classicCanStartWithBraceApply &&
        nextSelect.isDefined && !nextSelect.contains(lastApply)
      case _ => checkParent
    })
  }

  /** Checks if an earlier select started the chain */
  @tailrec
  final def inSelectChain(
      prevSelect: Option[SelectLike],
      thisSelect: SelectLike,
      lastApply: Tree,
  )(implicit style: ScalafmtConfig): Boolean = prevSelect match {
    case None => false
    case Some(p) if canStartSelectChain(p, Some(thisSelect), lastApply) => true
    case Some(p) =>
      val prevPrevSelect = findPrevSelect(p, style.newlines.encloseSelectChains)
      inSelectChain(prevPrevSelect, p, lastApply)
  }

  @tailrec
  final def findXmlLastLineIndent(ft: FT): Int = ft.left match {
    case _: T.Xml.Start => 0
    case t: T.Xml.Part => TokenOps.getXmlLastLineIndent(t) match {
        case Some(x) => x
        case None => findXmlLastLineIndent(prev(ft))
      }
    case _: T.Xml.SpliceEnd => findXmlLastLineIndent(prev(matchingLeft(ft)))
    case _ => findXmlLastLineIndent(prev(ft))
  }

  private def withIndentOnXmlToken(
      ft: => Option[FT],
      xmlEnd: => FT,
      splits: Seq[Split],
  )(implicit style: ScalafmtConfig): Seq[Split] =
    if (style.xmlLiterals.assumeFormatted) {
      val end = xmlEnd
      val len = findXmlLastLineIndent(prev(ft.getOrElse(end)))
      val indent = Indent(Length.Num(len, reset = true), end, ExpiresOn.After)
      splits.map(_.withIndent(indent))
    } else splits

  def withIndentOnXmlStart(xmlEnd: => FT, splits: Seq[Split])(implicit
      style: ScalafmtConfig,
  ): Seq[Split] = withIndentOnXmlToken(None, xmlEnd, splits)

  def withIndentOnXmlSpliceStart(ft: FT, splits: Seq[Split])(implicit
      style: ScalafmtConfig,
  ): Seq[Split] = ft.left match {
    case _: T.Xml.SpliceStart =>
      withIndentOnXmlToken(Some(ft), matchingLeft(ft), splits)
    case _ => splits
  }

  object CtrlBodySplits {

    private object CallSite {

      private val penalizeOpenNL: Policy.Pf = { case Decision(_, s) =>
        s.penalizeNL(1)
      }

      @tailrec
      private def getNestedOpens(
          tree: Member.ArgClause,
          res: List[FT],
      ): List[FT] = {
        def newres = getHead(tree) :: res
        tree.values match {
          case Nil => res
          case arg :: Nil if isEnclosedWithinParens(tree) =>
            getBlockStat(arg) match {
              case t: Member.Apply => getNestedOpens(t.argClause, newres)
              case _ => newres
            }
          case _ => newres
        }
      }

      private def getNestedOpensPolicy(
          opens: List[FT],
          policy: Policy,
      ): Policy = opens.foldLeft(policy) { case (res, x) =>
        val xft = nextNonComment(x)
        val endPos = xft.right match {
          case _: T.LeftBrace => Policy.End > xft
          case _ => Policy.End >= xft
        }
        val onOpen = Policy("NSTOPEN")(penalizeOpenNL) <== endPos
        Policy.End <= x ==> onOpen ==> res
      }

      @tailrec
      def getFoldedPolicy(
          body: Tree,
          policy: Policy = Policy.NoPolicy,
      ): Policy = body match {
        case t: Member.Apply if t.fun ne body =>
          val opens = getNestedOpens(t.argClause, Nil)
          getFoldedPolicy(t.fun, getNestedOpensPolicy(opens, policy))
        case t: Init =>
          val opens = t.argClauses.foldLeft(List.empty[FT]) { case (res, x) =>
            getNestedOpens(x, res)
          }
          getNestedOpensPolicy(opens, policy)
        case t: Term.Select => getFoldedPolicy(t.qual, policy)
        case _ => policy
      }

    }

    def foldedNonEmptyNonComment(
        body: Tree,
        nlSplitFunc: Int => Split,
        isKeep: Boolean,
        spaceIndents: Seq[Indent] = Seq.empty,
    )(implicit style: ScalafmtConfig, ft: FT): Seq[Split] = {
      val btokens = body.tokens
      val blast = getLastNonTrivial(btokens, body)
      val expire = nextNonCommentSameLine(blast)
      def penalize(penalty: Int) = Policy ? (penalty > 0) &&
        new PolicyOps.PenalizeAllNewlines(penalty) <= blast
      def getNlSplit(penalty: Int, nlCost: Int = 1)(implicit
          fileLine: FileLine,
      ): Split = nlSplitFunc(nlCost).andPolicy(penalize(penalty))
        .forThisLine(nextLine)
      def getSplits(spaceSplit: Split, nlCost: Int = 1) = (
        spaceSplit.withIndents(spaceIndents),
        getNlSplit(1, nlCost)(spaceSplit.fileLine),
      )
      def getSlb(end: FT, excl: TokenRanges)(implicit fileLine: FileLine) =
        SingleLineBlock(end, exclude = excl, noSyntaxNL = true)
      def getSlbSplit(
          end: FT,
          exclude: TokenRanges = TokenRanges.empty,
          policy: Policy = Policy.NoPolicy,
      )(implicit fileLine: FileLine) = Split(Space, 0)
        .withPolicy(policy | getSlb(end, exclude)).withOptimalToken(
          end,
          killOnFail = exclude.isEmpty,
          ignore = blast.idx > end.idx,
        )
      def getSpaceSplit(penalty: Int, policy: Policy = Policy.NoPolicy)(implicit
          fileLine: FileLine,
      ) = {
        val spacePolicy = policy | penalize(penalty)
        val miniSlbEnd = getSlbEndOnLeft(next(ft))
        val slbLite = style.newlines.keep &&
          (body.parent match {
            case Some(p: Term.Assign) => !p.parent.is[Term.ArgClause] ||
              style.binPack.callSite == BinPack.Site.Never
            case _ => true
          })
        val opt = if (style.newlines.keep) miniSlbEnd else blast
        Split(Space, 0).withSingleLineNoOptimal(miniSlbEnd)
          .andPolicy(spacePolicy)
          .withOptimalToken(opt, killOnFail = slbLite, recurseOnly = slbLite)
      }
      def getPolicySplits(penalty: Int, policy: Policy, nlCost: Int = 1)(
          implicit fileLine: FileLine,
      ) = getSplits(getSpaceSplit(penalty, policy), nlCost)
      def getSlbSplits(
          end: FT = expire,
          exclude: TokenRanges = TokenRanges.empty,
          policy: Policy = Policy.NoPolicy,
      )(implicit fileLine: FileLine) = (
        getSlbSplit(end, exclude, policy),
        getNlSplit(if (policy.isEmpty) 0 else 1),
      )
      def hasStateColumn = spaceIndents.exists(_.hasStateColumn)
      val adjustedBody = getBlockStat(body)
      val (spaceSplit, nlSplit) = adjustedBody match {
        case t: Term.If if isKeep || ifWithoutElse(t) || hasStateColumn =>
          val thenBeg = getHead(t.thenp)
          val thenHasLB = thenBeg.left.is[T.LeftBrace]
          val end = if (thenHasLB) thenBeg else prevNonCommentBefore(thenBeg)
          getSplits(getSlbSplit(end))
        case _: Term.If => getSlbSplits()
        case _: Term.TryClause =>
          if (hasStateColumn) getSplits(getSpaceSplit(1)) else getSlbSplits()
        case _: Term.Block | _: Term.MatchLike | _: Type.Match |
            _: Term.NewAnonymous => getSplits(getSpaceSplit(1))
        case t: Term.ForYield => getDelimsIfEnclosed(t.enumsBlock) match {
            case Some((forEnumHead, forEnumLast)) =>
              val exclude = TokenRanges(TokenRange(forEnumHead, forEnumLast))
              val afterYield = (t.body match {
                case b: Term.Block => getHeadAndLastIfEnclosed(b)
                    .map { case (forBodyHead, forBodyLastOpt) =>
                      if (forBodyLastOpt.isDefined) forBodyHead
                      else prevNonCommentBefore(forBodyHead)
                    }
                case b => tokenBeforeOpt(b)
              }).getOrElse(getLast(t))
              val end = getSlbEndOnLeft(afterYield)
              getSlbSplits(end, exclude, penalize(1))
            case None => getSlbSplits()
          }
        case ia: Member.Infix =>
          val lia = findLeftInfix(ia)
          val callPolicy = CallSite.getFoldedPolicy(lia.lhs)
          // lia is enclosed in parens if and only if lia == ia (== body)
          if (callPolicy.nonEmpty) getPolicySplits(0, callPolicy)
          else if (isBodyEnclosedAsBlock(ia))
            if (isKeep) getPolicySplits(0, Policy.NoPolicy)
            else getSplits(getSlbSplit(getLast(lia.lhs)))
          else getSplits(getSlbSplit(getLast(lia.op)))
        case b =>
          val callPolicy = CallSite.getFoldedPolicy(b)
          val nlCost = b match {
            case Member.Apply(_, ac)
                if styleMap.at(getHead(ac)).binPack.callSiteFor(ac).isOneline =>
              3
            case _ => 1
          }
          getPolicySplits(if (callPolicy.nonEmpty) 0 else 1, callPolicy, nlCost)
      }

      Seq(spaceSplit, nlSplit)
    }

    private def foldedNonComment(
        body: Tree,
        nlSplitFunc: Int => Split,
        isKeep: Boolean,
        spaceIndents: Seq[Indent],
    )(implicit style: ScalafmtConfig, ft: FT): Seq[Split] =
      if (tokens.isEmpty(body)) Seq(Split(Space, 0))
      else foldedNonEmptyNonComment(body, nlSplitFunc, isKeep, spaceIndents)

    private def unfoldedSpaceNonEmptyNonComment(body: Tree, slbOnly: Boolean)(
        implicit style: ScalafmtConfig,
    ): Split = {
      val expire = nextNonCommentSameLine(getLastNonTrivial(body))
      def slbSplit(end: FT)(implicit fileLine: FileLine) = Split(Space, 0)
        .withSingleLine(end, noSyntaxNL = true)
      getBlockStat(body) match {
        // we force newlines in for/yield
        case _: Term.ForYield => Split.ignored
        // we force newlines in try/catch/finally
        case _: Term.TryClause => Split.ignored
        // don't tuck curried apply
        case t: Term.Apply if t.fun.is[Term.Apply] => slbSplit(expire)
        case t =>
          val endOpt = if (slbOnly) None else getEndOfFirstCall(t)
          slbSplit(endOpt.getOrElse(expire))
      }
    }

    private def unfoldedNonComment(
        body: Tree,
        nlSplitFunc: Int => Split,
        spaceIndents: Seq[Indent],
        slbOnly: Boolean,
    )(implicit style: ScalafmtConfig): Seq[Split] =
      if (tokens.isEmpty(body)) Seq(Split(Space, 0).withIndents(spaceIndents))
      else {
        val spaceSplit = unfoldedSpaceNonEmptyNonComment(body, slbOnly)
        Seq(spaceSplit.withIndents(spaceIndents), nlSplitFunc(1).forThisLine)
      }

    def checkComment(
        nlSplitFunc: Int => Split,
    )(splitsFunc: FT => Seq[Split])(implicit ft: FT): Seq[Split] =
      if (!ft.right.is[T.Comment] && !ft.hasBlankLine) splitsFunc(ft)
      else if (ft.hasBreak) Seq(nlSplitFunc(0).forThisLine)
      else {
        val nextFt = nextNonCommentSameLineAfter(ft)
        val splits =
          if (nextFt.noBreak) splitsFunc(nextFt)
          else {
            val split = nlSplitFunc(0).forThisLine
            Seq(if (rhsIsCommentedOut(nextFt)) split.withNoIndent else split)
          }
        val policy = Policy.onlyFor(nextFt, "CBCMT")(_ => splits)
        Seq(Split(Space, 0, policy = policy))
      }

    def folded(
        body: Tree,
        isKeep: Boolean,
        spaceIndents: Seq[Indent] = Seq.empty,
    )(
        nlSplitFunc: Int => Split,
    )(implicit style: ScalafmtConfig, ft: FT): Seq[Split] = checkComment(
      nlSplitFunc,
    )(_ => foldedNonComment(body, nlSplitFunc, isKeep, spaceIndents))

    def slbOnly(body: Tree, spaceIndents: Seq[Indent] = Seq.empty)(
        nlSplitFunc: Int => Split,
    )(implicit style: ScalafmtConfig, ft: FT): Seq[Split] = checkComment(
      nlSplitFunc,
    )(_ => unfoldedNonComment(body, nlSplitFunc, spaceIndents, true))

    def get(body: Tree, spaceIndents: Seq[Indent] = Seq.empty)(
        classicNoBreakFunc: => Split,
    )(nlSplitFunc: Int => Split)(implicit
        style: ScalafmtConfig,
        ft: FT,
    ): Seq[Split] = checkComment(nlSplitFunc) { x =>
      def getFolded(isKeep: Boolean) =
        foldedNonComment(body, nlSplitFunc, isKeep = isKeep, spaceIndents)
      def getFoldedKeepNLOnly = getFolded(true).filter(_.isNL)
      style.newlines.getBeforeMultiline match {
        case Newlines.fold => getFolded(false)
        case Newlines.unfold =>
          unfoldedNonComment(body, nlSplitFunc, spaceIndents, false)
        case Newlines.classic if x.noBreak =>
          Option(classicNoBreakFunc).fold(getFolded(true))(func =>
            func.forThisLine +: getFoldedKeepNLOnly,
          )
        case Newlines.keep if x.noBreak => getFolded(true)
        case _ => getFoldedKeepNLOnly // keep/classic with break
      }
    }

    def getWithIndent(
        body: Tree,
        endFt: => FT,
        spaceIndents: Seq[Indent] = Seq.empty,
    )(classicNoBreakFunc: => Split)(nlSplitFunc: Int => Split)(implicit
        style: ScalafmtConfig,
        ft: FT,
    ): Seq[Split] = get(body, spaceIndents)(classicNoBreakFunc)(x =>
      withIndent(nlSplitFunc(x), body, endFt),
    )

    def withIndent(nlSplit: Split, endFt: FT)(implicit
        ft: FT,
        style: ScalafmtConfig,
    ): Split = withNLPolicy(endFt) {
      val nft = nextNonComment(ft)
      val rpOpt = if (nft.right.is[T.LeftParen]) matchingOptRight(nft) else None
      val expire = nextNonCommentSameLine(
        rpOpt.fold(endFt)(rp => if (rp.left.end >= endFt.left.end) rp else endFt),
      )
      nlSplit.withIndent(style.indent.main, expire, ExpiresOn.After)
    }

    def withIndent(nlSplit: Split, body: Tree, endFt: => FT)(implicit
        ft: FT,
        style: ScalafmtConfig,
    ): Split = asInfixApp(body)
      .fold(withIndent(nlSplit, endFt))(InfixSplits.withNLIndent(nlSplit))

    @tailrec
    def getBlockStat(t: Tree): Tree = t match {
      case b: Term.Block => getSingleStatExceptEndMarker(b.stats) match {
          case Some(s) if !isEnclosedInBraces(b) => getBlockStat(s)
          case _ => t
        }
      case _ => t
    }

  }

  def withNLPolicy(endFt: FT)(nlSplit: Split): Split = {
    val nft = nextNonCommentSameLine(endFt)
    nlSplit.andPolicy(nft.right match {
      case _: T.Keyword => decideNewlinesOnlyBeforeToken(next(nft))
      case x: T.Semicolon =>
        val semi = nextNonCommentSameLineAfter(nft)
        if (semi.noBreak || (semi.left eq x) && !semi.right.is[T.Comment])
          decideNewlinesOnlyAfterToken(semi)
        else NoPolicy
      case _ => NoPolicy
    })
  }

  // Redundant () delims around case statements
  def getClosingIfCaseBodyEnclosedAsBlock(postArrowFt: FT, caseStat: CaseTree)(
      implicit beforeMultiline: Newlines.SourceHints,
  ): Option[FT] = {
    val body = caseStat.body
    val ok = body.eq(postArrowFt.meta.rightOwner) &&
      (beforeMultiline.ignoreSourceSplit || postArrowFt.noBreak)
    if (ok) getClosingIfBodyEnclosedAsBlock(body) else None
  }

  // Redundant () delims around body
  def getClosingIfBodyEnclosedAsBlock(body: Tree): Option[FT] = body match {
    case _: Lit.Unit | _: Term.Tuple => None
    case t: Term.ApplyInfix if {
          val op = t.op.value
          op == "->" || op == "→"
        } => None
    case _ => getClosingIfWithinParens(body)
  }

  def isBodyEnclosedAsBlock(body: Tree): Boolean =
    getClosingIfBodyEnclosedAsBlock(body).isDefined

  object GetSelectLike {
    val OnRight =
      new FT.ExtractFromMeta(m => onRightOpt(m.rightOwner, tokens(m.idx)))

    private def get(
        ro: Tree,
    )(onMatch: Term.SelectMatch => Option[FT]): Option[SelectLike] = ro match {
      case x: Term.Select => Some(SelectLike(x))
      case x: Term.SelectMatch => onMatch(x).map(ft => SelectLike(x, ft))
      case _ => None
    }

    private[FormatOps] def onRightOpt(ro: Tree, ft: => FT): Option[SelectLike] =
      get(ro)(_ =>
        nextNonCommentAfter(ft) match {
          case xft @ FT(_, _: T.KwMatch, _) => Some(next(xft))
          case _ => None
        },
      )

    def onRightOpt(ft: FT): Option[SelectLike] =
      onRightOpt(ft.meta.rightOwner, ft)

    def unapply(tree: Tree): Option[SelectLike] =
      get(tree)(x => Some(tokenBefore(x.casesBlock)))
  }

  def getSplitsForTypeBounds(
      noNLMod: => Modification,
      tbounds: Type.Bounds,
      bounds: => Seq[Type],
  )(implicit style: ScalafmtConfig, ft: FT): Seq[Split] = {
    val boundOpt = bounds.find(_.pos.start > ft.right.end)
    val expireOpt = boundOpt.map(getLastNonTrivial)
    getSplitsForTypeBounds(noNLMod, tbounds, expireOpt)
  }

  def getSplitsForTypeBounds(
      noNLMod: => Modification,
      tbounds: Type.Bounds,
      boundEndOpt: Option[FT],
  )(implicit style: ScalafmtConfig, ft: FT): Seq[Split] = {
    val typeEnd = getLastNonTrivial(tbounds)
    val boundEnd = boundEndOpt.getOrElse(typeEnd)
    def indent = Indent(style.indent.main, boundEnd, ExpiresOn.After)
    def unfoldPolicy = Policy.onLeft(typeEnd, prefix = "VB/CB") {
      case Decision(FT(_, _: T.Colon | _: T.Viewbound, m), s)
          if m.rightOwner eq tbounds => Decision.onlyNewlineSplits(s)
    }
    style.newlines.beforeTypeBounds match {
      case Newlines.classic => Seq(Split(noNLMod, 0))
      case Newlines.unfold => Seq(
          Split(noNLMod, 0).withSingleLine(typeEnd),
          Split(Newline, 1).withIndent(indent).withPolicy(unfoldPolicy),
        )
      case Newlines.keep if ft.hasBreak =>
        Seq(Split(Newline, 1).withIndent(indent))
      case _ => Seq(
          Split(noNLMod, 0).withSingleLine(boundEnd),
          Split(Newline, 1).withIndent(indent),
        )
    }
  }

  object OptionalBraces {

    private trait Factory {
      def create(
          nft: FT,
      )(implicit style: ScalafmtConfig, ft: FT): Option[OptionalBracesRegion]
    }

    // Optional braces in templates after `:|with`
    // Optional braces after any token that can start indentation:
    // )  =  =>  ?=>  <-  catch  do  else  finally  for
    // if  match  return  then  throw  try  while  yield
    def unapply(ftMeta: FT.Meta)(implicit
        style: ScalafmtConfig,
    ): Option[Seq[Split]] = get(tokens(ftMeta.idx)).flatMap(_.splits)

    def get(
        ft: FT,
    )(implicit style: ScalafmtConfig): Option[OptionalBracesRegion] =
      if (!style.dialect.allowSignificantIndentation) None
      else Option {
        ft.left match {
          case _: T.Colon => ColonEolImpl
          case _: T.KwWith => WithImpl
          case _: T.RightArrow => RightArrowImpl
          case _: T.RightParen => RightParenImpl
          case _: T.KwFor => ForImpl
          case _: T.KwWhile => WhileImpl
          case _: T.KwDo => DoImpl
          case _: T.Equals => EqualsImpl
          case _: T.KwTry => TryImpl
          case _: T.KwCatch => CatchImpl
          case _: T.KwFinally => FinallyImpl
          case _: T.KwMatch => MatchImpl
          case _: T.KwThen => ThenImpl
          case _: T.KwIf => IfImpl
          case _: T.KwElse => ElseImpl
          case _: T.KwReturn | _: T.ContextArrow | _: T.LeftArrow |
              _: T.KwThrow | _: T.KwYield => BlockImpl
          case _ => null
        }
      }.flatMap { impl =>
        implicit val ift: FT = ft
        val nft = nextNonComment(ft)
        impl.create(nft).filter(ob =>
          !nft.right.is[T.LeftBrace] || nft.meta.rightOwner.parent != ob.owner,
        )
      }

    def at(ft: FT)(implicit style: ScalafmtConfig): Boolean = get(ft).nonEmpty

    private def getSplits(
        tree: Tree,
        forceNL: Boolean,
        danglingKeyword: Boolean = true,
        indentOpt: Option[Int] = None,
        forceNLIfTrailingStandaloneComments: Boolean = true,
    )(implicit
        fileLine: FileLine,
        style: ScalafmtConfig,
        ft: FT,
    ): Seq[Split] = {
      val treeTokens = tree.tokens
      val end = getOnOrAfterLast(treeTokens, tree)
      val nonTrivialEnd = prevNonComment(end)
      val slbExpire = nextNonCommentSameLine(nonTrivialEnd)
      def head = getHead(treeTokens, tree)
      val close = (tree match {
        case _: Member.Tuple => None
        case Term.Block((_: Member.Tuple) :: Nil)
            if !head.left.is[T.LeftBrace] => None
        case _ => tokens.getClosingIfWithinParens(nonTrivialEnd)(head)
            .map(prevNonCommentSameLine)
      }).getOrElse(nextNonCommentSameLine(end))
      def nlPolicy(implicit fileLine: FileLine) = Policy ? danglingKeyword && {
        val couldBeTucked = close.right.is[T.CloseDelim] &&
          close.rightOwner.is[Member.SyntaxValuesClause]
        if (!couldBeTucked) decideNewlinesOnlyAfterClose(close)
        else decideNewlinesOnlyAfterToken(rank = 1, ifAny = true)(close)
      }
      val indentLen = indentOpt.getOrElse(style.indent.getSignificant)
      val indent = Indent(indentLen, close, ExpiresOn.After)
      def nlOnly = forceNLIfTrailingStandaloneComments &&
        slbExpire.right.is[T.Comment] && slbExpire.idx < close.idx
      if (ft.hasBlankLine)
        Seq(Split(Newline2x, 0).withIndent(indent).withPolicy(nlPolicy))
      else if (forceNL || nlOnly)
        Seq(Split(Newline, 0).withIndent(indent).withPolicy(nlPolicy))
      else Seq(
        Split(Space, 0).withSingleLine(slbExpire).withIndent(indent),
        Split(Newline, 1).withIndent(indent).withPolicy(nlPolicy),
      )
    }

    // https://dotty.epfl.ch/docs/reference/other-new-features/indentation.html#variant-indentation-marker-
    // TODO: amend for additional cases when the parser supports them
    private object ColonEolImpl extends Factory {
      def create(nft: FT)(implicit
          style: ScalafmtConfig,
          ft: FT,
      ): Option[OptionalBracesRegion] = {
        val lo = ft.meta.leftOwner
        def createImpl(ownerOpt: => Option[Tree], okRightBrace: => Boolean) =
          Some(new OptionalBracesRegion {
            def owner = ownerOpt
            def splits = Some(getSplits(lo, forceNL = true))
            def rightBrace = if (okRightBrace) treeLast(lo) else None
          })
        lo match {
          case t: Template.Body if getHeadOpt(t).contains(ft) =>
            createImpl(t.parent.parent, isSeqMulti(t.stats))
          case t: Pkg.Body if getHeadOpt(t).contains(ft) =>
            createImpl(t.parent, isSeqMulti(t.stats))
          case t: Stat.Block if getHeadOpt(t).contains(ft) =>
            createImpl(t.parent, t.parent.is[Type.Refine] || isSeqMulti(t.stats))
          case t: Term.ArgClause if getHead(t) eq ft => onArgClause(t, t.values)
          case t: Term => t.parent match {
              case Some(p: Term.ArgClause)
                  if hasSingleElement(p, t) && (getHead(p) eq ft) =>
                val stats = t match {
                  case b: Term.Block => b.stats
                  case _ => t :: Nil
                }
                onArgClause(p, stats)
              case _ => None
            }
          case _ => None
        }
      }

      private def onArgClause(ac: Term.ArgClause, args: List[Tree])(implicit
          style: ScalafmtConfig,
          ft: FT,
      ): Option[OptionalBracesRegion] = {
        def funcSplit(arg: Term.FunctionTerm)(implicit fl: FileLine) = {
          val end = getLast(arg)
          val opt = nextNonCommentSameLine(getFuncArrow(arg).getOrElse(end))
          Split(Space, 0).withSingleLine(opt)
            .andPolicy(decideNewlinesOnlyAfterToken(opt))
        }
        val indent = ac.parent match {
          case Some(p: Term.Apply) =>
            @tailrec
            def isSelect(ma: Member.Apply): Boolean = ma.fun match {
              case x: Member.Apply => isSelect(x)
              case x => x.is[Term.Select]
            }
            val ok =
              (style.getFewerBraces() match {
                case Indents.FewerBraces.never => true
                case Indents.FewerBraces.always => false
                case Indents.FewerBraces.beforeSelect =>
                  !p.parent.is[Term.Select]
              }) || isSelect(p)
            if (ok) None // select is taken care off elsewhere
            else Some(style.indent.main + style.indent.getSignificant)
          case _ => None
        }
        Some {
          new OptionalBracesRegion {
            def owner = ac.parent
            def splits = Some {
              args match {
                case (tf: Term.FunctionTerm) :: Nil
                    if !style.newlines.alwaysBeforeCurlyLambdaParams &&
                      // https://dotty.epfl.ch/docs/internals/syntax.html
                      (tf.paramClause match { // LambdaStart
                        case tpc @ Term.ParamClause(tp :: Nil, mod) =>
                          mod.isEmpty && tp.mods.isEmpty &&
                          tp.decltpe.isEmpty || isEnclosedWithinParens(tpc)
                        case _ => true // multiple params are always in parens
                      }) =>
                  getSplits(ac, forceNL = false, indentOpt = indent) match {
                    case s +: rs if !s.isNL => funcSplit(tf)(s.fileLine) +: rs
                    case ss => ss
                  }
                case _ => getSplits(ac, forceNL = true, indentOpt = indent)
              }
            }
            def rightBrace = treeLast(ac)
          }
        }
      }
    }

    private object WithImpl extends Factory {
      def create(nft: FT)(implicit
          style: ScalafmtConfig,
          ft: FT,
      ): Option[OptionalBracesRegion] = {
        val lo = ft.meta.leftOwner
        def createImpl(
            tb: Tree.Block,
            ownerOpt: => Option[Tree],
            okRightBrace: => Boolean,
        ) =
          if (
            nft.right.is[T.LeftBrace] && (nft.meta.rightOwner eq tb) ||
            tb.pos.start > nft.right.start
          ) None
          else Some(new OptionalBracesRegion {
            def owner = ownerOpt
            def splits = Some(getSplits(lo, forceNL = true))
            def rightBrace = if (okRightBrace) treeLast(lo) else None
          })
        lo match {
          case t: Template =>
            createImpl(t.body, t.parent, isSeqMulti(t.body.stats))
          case t: Type.Refine => createImpl(t.body, Some(t), true)
          case _ => None
        }
      }
    }

    private object BlockImpl extends Factory {
      def create(nft: FT)(implicit
          style: ScalafmtConfig,
          ft: FT,
      ): Option[OptionalBracesRegion] = {
        val leftOwner = ft.meta.leftOwner
        findTreeWithParentSimple(nft.meta.rightOwner)(_ eq leftOwner) match {
          case Some(t: Term.Block) => getBlockWithNonSingleTermStat(t)
              .flatMap(b => WithStats(nft, b.stats.headOption, t, t.parent))
          case _ => None
        }
      }
    }

    private object RightParenImpl extends Factory {
      def create(nft: FT)(implicit
          style: ScalafmtConfig,
          ft: FT,
      ): Option[OptionalBracesRegion] = {
        def createImpl(ownerTree: => Tree, blockTree: => Tree)(
            splitsRef: => Option[Seq[Split]],
        ) = Some(new OptionalBracesRegion {
          def owner = Some(ownerTree)
          def splits = splitsRef
          def rightBrace = blockLast(blockTree)
        })
        def createKwDo(ownerTree: => Tree, blockTree: => Tree) =
          if (nft.right.is[T.KwDo]) None
          else createImpl(ownerTree, blockTree)(
            if (isTreeSingleExpr(blockTree)) None
            else Some(getSplits(blockTree, true)),
          )
        ft.meta.leftOwner match {
          case ParamClauseParent(pp: Defn.ExtensionGroup)
              if !nft.right.is[T.LeftBrace] && (tokenBefore(pp.body) eq ft) =>
            createImpl(pp, pp.body)(Some(
              getSplits(pp.body, shouldBreakInOptionalBraces(ft, nft)),
            ))
          case t: Term.If if !nft.right.is[T.KwThen] && {
                !isTreeSingleExpr(t.thenp) || t.thenp.is[Tree.CasesBlock] ||
                getLastNotTrailingCommentOpt(t.thenp).exists(_.isLeft) ||
                !ifWithoutElse(t) &&
                (isElsePWithOptionalBraces(t) ||
                  existsBlockIfWithoutElse(t.thenp, false))
              } => createImpl(t, t.thenp)(Some(getSplitsForIf(nft, t)))
          case t: Term.EnumeratorsBlock => t.parent.flatMap {
              case p: Term.For => createKwDo(p, p.body)
              case _ => None
            }
          case t: Term.While => createKwDo(t, t.body)
          case _ => None
        }
      }
    }

    private object RightArrowImpl extends Factory {
      def create(
          nft: FT,
      )(implicit style: ScalafmtConfig, ft: FT): Option[OptionalBracesRegion] =
        ft.meta.leftOwner match {
          case t: Case => // unsupported except for right brace, or when ends in comment
            Some(new OptionalBracesRegion {
              def owner = None
              def splits =
                if (getLastNotTrailingCommentOpt(t).forall(_.isRight)) None
                else Some(Seq(Split(Newline2x(ft), 0)))
              def rightBrace = blockLast(t.body)
            })
          case t @ Tree.WithBody(b: Tree.CasesBlock)
              if nft.right.is[T.KwCase] =>
            Some(new OptionalBracesRegion {
              def owner = Some(t)
              def splits = Some(getSplits(b, forceNL = true))
              def rightBrace = treeLast(b)
            })
          case _ => BlockImpl.create(nft)
        }
    }

    private object ForImpl extends Factory {
      def create(
          nft: FT,
      )(implicit style: ScalafmtConfig, ft: FT): Option[OptionalBracesRegion] =
        ft.meta.leftOwner match {
          case t @ Tree.WithEnums(x) if isSeqMulti(x) =>
            WithStats(nft, x.headOption, x.last, Some(t), nlOnly = false)
          case _ => None
        }
    }

    private object WhileImpl extends Factory {
      def create(
          nft: FT,
      )(implicit style: ScalafmtConfig, ft: FT): Option[OptionalBracesRegion] =
        ft.meta.leftOwner match {
          case t: Term.While => t.expr match {
              case b: Term.Block
                  if isMultiStatBlock(b) &&
                    !matchingOptRight(nft).exists(_.left.end >= b.pos.end) =>
                Some(new OptionalBracesRegion {
                  def owner = Some(t)
                  def splits = Some {
                    val dangle = style.danglingParentheses.ctrlSite
                    val forceNL = !nft.right.is[T.LeftParen]
                    getSplits(b, forceNL = forceNL, danglingKeyword = dangle)
                  }
                  def rightBrace = blockLast(b)
                })
              case _ => None
            }
          case _ => None
        }
    }

    private object DoImpl extends Factory {
      def create(nft: FT)(implicit
          style: ScalafmtConfig,
          ft: FT,
      ): Option[OptionalBracesRegion] = {
        val lo = ft.meta.leftOwner
        def createImpl(body: Tree) = Some(new OptionalBracesRegion {
          def owner = Some(lo)
          def splits = Some(getSplitsMaybeBlock(nft, body))
          def rightBrace = blockLast(body)
        })
        lo match {
          case t: Tree.WithBody => createImpl(t.body)
          case _ => None
        }
      }
    }

    private object EqualsImpl extends Factory {
      def create(
          nft: FT,
      )(implicit style: ScalafmtConfig, ft: FT): Option[OptionalBracesRegion] =
        ft.meta.leftOwner match {
          case t: Ctor.Secondary =>
            if (t.body.stats.isEmpty) None
            else WithStats(nft, Some(t.body.init), t.body, t.parent)
          case t @ Tree.WithBody(b) => (b match {
              case x: Term.Block => getBlockWithNonSingleTermStat(x)
              case x: Tree.CasesBlock => Some(x)
              case _ => None
            }).fold(getMaybeFewerBracesSelectSplits(b))(x =>
              WithStats(nft, x.stats.headOption, b, Some(t)),
            )
          case _ => BlockImpl.create(nft)
        }
    }

    private object TryImpl extends Factory {
      def create(nft: FT)(implicit
          style: ScalafmtConfig,
          ft: FT,
      ): Option[OptionalBracesRegion] = {
        val lo = ft.meta.leftOwner
        def createImpl(expr: Term, finallyp: Option[Term], usesOB: => Boolean) =
          Some(new OptionalBracesRegion {
            def owner = Some(lo)
            def splits =
              if (!isTreeSingleExpr(expr)) Some(getSplits(expr, true))
              else if (finallyp.exists(isTreeUsingOptionalBraces) || usesOB)
                Some(getSplits(expr, shouldBreakInOptionalBraces(ft, nft)))
              else None

            def rightBrace = blockLast(expr)
          })
        ft.meta.leftOwner match {
          case t: Term.Try =>
            createImpl(t.expr, t.finallyp, isCatchUsingOptionalBraces(t))
          case t: Term.TryWithHandler => createImpl(t.expr, t.finallyp, false)
          case _ => None
        }
      }
    }

    private def isCatchUsingOptionalBraces(tree: Term.Try): Boolean = tree
      .catchp.headOption.exists(x => !tokenBefore(x).left.is[T.LeftBrace])

    private object CatchImpl extends Factory {
      def create(
          nft: FT,
      )(implicit style: ScalafmtConfig, ft: FT): Option[OptionalBracesRegion] =
        ft.meta.leftOwner match {
          case t: Term.Try => t.catchClause match {
              case Some(cb: Term.CasesBlock) =>
                val nlOnly = cb.cases match {
                  // to avoid next expression being interpreted as body
                  case head :: Nil => shouldBreakInOptionalBraces(ft, nft) ||
                    t.finallyp.isEmpty &&
                    (isEmptyTree(head.body) || getLastOpt(cb).exists { x =>
                      val xend = nextNonCommentSameLine(x)
                      xend.right match {
                        case _: T.Comment => !xend.hasBlankLine
                        case _ => false
                      }
                    })
                  case _ => true
                }
                Some(new OptionalBracesRegion {
                  def owner = Some(t)
                  def splits = Some(getSplits(
                    cb,
                    forceNL = nlOnly,
                    forceNLIfTrailingStandaloneComments = false,
                  ))
                  def rightBrace = treeLast(cb)
                })
              case _ => None
            }
          case _ => None
        }
    }

    private object FinallyImpl extends Factory {
      def create(nft: FT)(implicit
          style: ScalafmtConfig,
          ft: FT,
      ): Option[OptionalBracesRegion] = {
        val lo = ft.meta.leftOwner
        def createImpl(usingOB: => Boolean)(finallyExpr: Tree) = {
          val isMulti = !isTreeSingleExpr(finallyExpr)
          def usesOB = isMulti || usingOB
          def forceNL = isMulti || shouldBreakInOptionalBraces(ft, nft)
          new OptionalBracesRegion {
            def owner = Some(lo)
            def splits =
              if (usesOB) Some(getSplits(finallyExpr, forceNL)) else None
            def rightBrace = blockLast(finallyExpr)
          }
        }
        lo match {
          case t: Term.Try => t.finallyp.map(createImpl(
              isCatchUsingOptionalBraces(t) || isTreeUsingOptionalBraces(t.expr),
            ))
          case t: Term.TryWithHandler => t.finallyp
              .map(createImpl(isTreeUsingOptionalBraces(t.expr)))
          case _ => None
        }
      }
    }

    private object MatchImpl extends Factory {
      def create(
          nft: FT,
      )(implicit style: ScalafmtConfig, ft: FT): Option[OptionalBracesRegion] =
        ft.meta.leftOwner match {
          case t @ Tree.WithCasesBlock(x) =>
            val ind = style.indent.matchSite
            WithStats(nft, x.cases.headOption, t, Some(t), indentOpt = ind)
          case _ => None
        }
    }

    private object ThenImpl extends Factory {
      def create(
          nft: FT,
      )(implicit style: ScalafmtConfig, ft: FT): Option[OptionalBracesRegion] =
        ft.meta.leftOwner match {
          case t: Term.If => Some(new OptionalBracesRegion {
              def owner = Some(t)
              def splits = Some(getSplitsForIf(nft, t))
              def rightBrace = blockLast(t.thenp)
            })
          case _ => None
        }
    }

    private object IfImpl extends Factory {
      def create(
          nft: FT,
      )(implicit style: ScalafmtConfig, ft: FT): Option[OptionalBracesRegion] =
        ft.meta.leftOwner match {
          case t: Term.If => t.cond match {
              case b: Term.Block if (matchingOptRight(nft) match {
                    case Some(t) => t.left.end < b.pos.end
                    case None => isMultiStatBlock(b)
                  }) =>
                Some(new OptionalBracesRegion {
                  def owner = Some(t)
                  def splits = Some {
                    val dangle = style.danglingParentheses.ctrlSite
                    val forceNL = !nft.right.is[T.LeftParen]
                    getSplits(b, forceNL, dangle)
                  }
                  def rightBrace = blockLast(b)
                })
              case _ => None
            }
          case _ => None
        }
    }

    private object ElseImpl extends Factory {
      def create(
          nft: FT,
      )(implicit style: ScalafmtConfig, ft: FT): Option[OptionalBracesRegion] =
        ft.meta.leftOwner match {
          case t: Term.If => (getTreeSingleExpr(t.elsep) match {
              case Some(x: Term.If) =>
                val forceNL = isJustBeforeTree(nft)(x) && ft.hasBreak &&
                  ((ft ne nft) || style.newlines.keep)
                if (forceNL) Some(true) else None
              case Some(_: Tree.CasesBlock) => Some(true)
              case Some(_)
                  if !getLastNotTrailingCommentOpt(t).exists(_.isLeft) =>
                if (!isThenPWithOptionalBraces(t)) None
                else Some(shouldBreakInOptionalBraces(ft, nft))
              case _ => Some(true)
            }).map(forceNL =>
              new OptionalBracesRegion {
                def owner = Some(t)
                def splits = Some(getSplits(t.elsep, forceNL))
                def rightBrace = blockLast(t.elsep)
              },
            )
          case _ => None
        }
    }

    private def getSplitsMaybeBlock(
        nft: FT,
        tree: Tree,
        danglingKeyword: Boolean = true,
    )(implicit
        fileLine: FileLine,
        style: ScalafmtConfig,
        ft: FT,
    ): Seq[Split] = {
      val forceNL = !hasSingleTermStatIfBlock(tree) ||
        shouldBreakInOptionalBraces(ft, nft) || tree.is[Tree.CasesBlock]
      getSplits(tree, forceNL, danglingKeyword)
    }

    private def getMaybeFewerBracesSelectSplits(body: Tree)(implicit
        ft: FT,
        style: ScalafmtConfig,
    ) = findFirstSelect(body, enclosed = true).filter(_.qual match {
      case q @ (_: Term.ForClause | _: Term.ApplyInfix |
          _: Term.SelectPostfix) => !isEnclosedInMatching(q)
      case _ => false
    }).map { selectLike =>
      new OptionalBracesRegion {
        def splits: Option[Seq[Split]] = {
          val noFbIndent = style.getFewerBraces() == Indents.FewerBraces.never
          val indentLen =
            if (noFbIndent) style.indent.getSignificant
            else style.indent.main + style.indent.getSignificant
          val dot = prev(prevNonCommentBefore(selectLike.nameFt))
          val beforeDot = prevNonCommentSameLine(dot)
          val policy = Policy.End <= beforeDot ==>
            Policy.onRight(dot, "NL-NOIND-SELECT") {
              case Decision(`beforeDot`, ss) => ss.flatMap(s =>
                  if ((dot eq beforeDot) && !s.isNL) None
                  else Some(s.deActivateFor(SplitTag.SelectChainFirstNL)),
                )
              case Decision(_, ss) => ss
                  .map(_.deActivateFor(SplitTag.SelectChainFirstNL))
            }
          val nlSplit = Split(Newline2x(ft), 1, policy = policy)
            .withIndent(indentLen, beforeDot, ExpiresOn.After)
          Some(Seq(nlSplit))
        }
        def rightBrace: Option[FT] = treeLast(body)
        def owner: Option[Tree] = body.parent
      }
    }

    private class WithStats private (
        nft: FT,
        body: Tree,
        val owner: Option[Tree],
        nlOnly: Boolean,
        indentOpt: Option[Int],
    )(implicit fileLine: FileLine, style: ScalafmtConfig, ft: FT)
        extends OptionalBracesRegion {
      def splits: Option[Seq[Split]] = {
        val forceNL = nlOnly || shouldBreakInOptionalBraces(ft, nft)
        Some(getSplits(body, forceNL = forceNL, indentOpt = indentOpt))
      }
      def rightBrace = treeLast(body)
    }

    private object WithStats {
      def apply(
          nft: FT,
          head: Option[Tree],
          body: => Tree,
          owner: => Option[Tree],
          nlOnly: Boolean = true,
          indentOpt: Option[Int] = None,
      )(implicit
          fileLine: FileLine,
          style: ScalafmtConfig,
          ft: FT,
      ): Option[WithStats] = head.flatMap(head =>
        if (!isJustBeforeTree(nft)(head)) None
        else Some(new WithStats(nft, body, owner, nlOnly, indentOpt)),
      )
    }

    private def getSplitsForIf(nft: FT, t: Term.If)(implicit
        fileLine: FileLine,
        style: ScalafmtConfig,
        ft: FT,
    ): Seq[Split] = {
      def nestedIf(x: Term.If) = {
        val forceNL = shouldBreakInOptionalBraces(ft, nft) ||
          !ifWithoutElse(t) && existsIfWithoutElse(x)
        getSplits(t.thenp, forceNL)
      }
      t.thenp match {
        case x: Term.If => nestedIf(x)
        case Term.Block((x: Term.If) :: Nil) => nestedIf(x)
        case x => getSplitsMaybeBlock(nft, x, false)
      }
    }

    private def isThenPWithOptionalBraces(tree: Term.If): Boolean = {
      val thenp = tree.thenp
      val before = tokenJustBefore(thenp)
      prevNonComment(before).left match {
        case _: T.KwThen => true
        case _: T.LeftBrace => false
        case _ => !isTreeSingleExpr(thenp) &&
          (!before.right.is[T.LeftBrace] || matchingOptRight(before)
            .exists(_.left.end < thenp.pos.end))
      }
    }

    @tailrec
    private def isElsePWithOptionalBraces(tree: Term.If): Boolean = {
      val elsep = tree.elsep
      !getHead(elsep).left.is[T.LeftBrace] &&
      (elsep match {
        case t: Term.If => isThenPWithOptionalBraces(t) ||
          !ifWithoutElse(t) && isElsePWithOptionalBraces(t)
        case Term.Block((t: Term.If) :: Nil) => isThenPWithOptionalBraces(t) ||
          !ifWithoutElse(t) && isElsePWithOptionalBraces(t)
        case t => !isTreeSingleExpr(t)
      })
    }

    private def shouldBreakInOptionalBraces(ft: FT, nft: FT)(implicit
        style: ScalafmtConfig,
    ): Boolean = style.newlines.source match {
      case Newlines.unfold => true
      case Newlines.fold => false
      case Newlines.keep => ft.hasBreak
      case _ => (ft ne nft) && ft.hasBreak
    }

    private def isTreeUsingOptionalBraces(tree: Tree): Boolean =
      !isTreeSingleExpr(tree) && !tokenBefore(tree).left.is[T.LeftBrace]

    @inline
    private def treeLast(tree: Tree): Option[FT] = getLastOpt(tree)
    @inline
    private def blockLast(tree: Tree): Option[FT] =
      if (isTreeMultiStatBlock(tree)) treeLast(tree) else None
    @inline
    private def blockLast(tree: Term.Block): Option[FT] =
      if (isMultiStatBlock(tree)) treeLast(tree) else None

    def indentAndBreakBeforeCtrl[A](tree: Tree, split: Split)(implicit
        style: ScalafmtConfig,
        classifier: Classifier[T, A],
    ): Option[Split] = Some {
      if (!style.dialect.allowSignificantIndentation) return None

      val treeTokens = tree.tokens
      val head = treeTokens.head
      val hft = after(head)
      if (hft.left.eq(head) && tree.is[Term.Block] && !split.isNL) return None

      val beg = getOnOrBeforeOwned(hft, tree)
      val end = getLastNonTrivial(treeTokens, tree)
      val kw = next(getClosingIfWithinParens(end)(beg).fold(end)(next))
      if (!kw.left.is[A]) return None

      val indent = style.indent.ctrlSite.getOrElse(style.indent.getSignificant)
      def policy =
        if (split.isNL) decideNewlinesOnlyBeforeClose(kw)
        else decideNewlinesOnlyBeforeCloseOnBreak(kw)
      split.withIndent(indent, kw, ExpiresOn.Before)
        .andPolicy(policy, !style.danglingParentheses.ctrlSite)
    }

  }

  object MissingBraces {

    type Ranges = Seq[(Tree, Tree)]
    type Result = Option[(Tree, Ranges)]

    private trait Factory {
      def getBlocks(ft: FT, nft: FT, all: Boolean): Result
    }

    def getBlocks(ft: FT, all: Boolean): Result = {
      val nft = nextNonComment(ft)
      if (nft.right.is[T.LeftBrace]) None
      else {
        val impl = ft.left match {
          case _: T.RightArrow => RightArrowImpl
          case _: T.RightParen => RightParenImpl
          case _: T.RightBrace => RightBraceImpl
          case _: T.KwDo => DoImpl
          case _: T.Equals => EqualsImpl
          case _: T.KwTry => TryImpl
          case _: T.KwCatch => CatchImpl
          case _: T.KwFinally => FinallyImpl
          case _: T.KwElse => ElseImpl
          case _: T.KwYield => YieldImpl
          case _ => null
        }
        Option(impl).flatMap(_.getBlocks(ft, nft, all))
      }
    }

    private def seq(all: Boolean, t: Tree): Ranges =
      if (all) Seq(t -> t) else Nil

    private def seq(all: Boolean, t: Option[Tree]): Ranges = t.map(seq(all, _))
      .getOrElse(Nil)

    private def seq(all: Boolean, t: Seq[Tree]): Ranges =
      if (all && t.nonEmpty) Seq(t.head -> t.last) else Nil

    private object BlockImpl extends Factory {
      def getBlocks(ft: FT, nft: FT, all: Boolean): Result = {
        def ok(stat: Tree): Boolean = isJustBeforeTree(nft)(stat)
        val leftOwner = ft.meta.leftOwner
        findTreeWithParentSimple(nft.meta.rightOwner)(_ eq leftOwner) match {
          case Some(t: Term.Block) =>
            if (t.stats.headOption.exists(ok)) Some((t, Nil)) else None
          case x => x.filter(ok).map((_, Nil))
        }
      }
    }

    private object RightArrowImpl extends Factory {
      def getBlocks(ft: FT, nft: FT, all: Boolean): Result =
        ft.meta.leftOwner match {
          case t: Term.FunctionTerm =>
            val skip = t.parent.exists(TreeOps.isExprWithParentInBraces(t))
            if (skip) None else Some((t.body, seq(all, t.paramClause.values)))
          case _ => None
        }
    }

    private object RightParenImpl extends Factory {
      def getBlocks(ft: FT, nft: FT, all: Boolean): Result =
        ft.meta.leftOwner match {
          case x: Term.If if !nft.right.is[T.KwThen] =>
            val hasElse = all && !ifWithoutElse(x)
            Some((x.thenp, seq(hasElse, x.elsep) ++ seq(all, x.cond)))
          case t: Term.EnumeratorsBlock
              if !nft.right.is[T.KwDo] && getLastOpt(t).contains(ft) =>
            t.parent match {
              case Some(p: Term.For) => Some((p.body, seq(all, t)))
              case _ => None
            }
          case t: Term.While if !nft.right.is[T.KwDo] =>
            Some((t.body, seq(all, t.cond)))
          case _ => None
        }
    }

    private object RightBraceImpl extends Factory {
      def getBlocks(ft: FT, nft: FT, all: Boolean): Result =
        ft.meta.leftOwner match {
          case t: Term.EnumeratorsBlock
              if !nft.right.is[T.KwDo] && getLastOpt(t).contains(ft) =>
            t.parent match {
              case Some(p: Term.For) => Some((p.body, seq(all, t)))
              case _ => None
            }
          case _ => None
        }
    }

    private object DoImpl extends Factory {
      def getBlocks(ft: FT, nft: FT, all: Boolean): Result =
        ft.meta.leftOwner match {
          case t: Term.Do => Some((t.body, seq(all, t.expr)))
          case _ => None
        }
    }

    private object EqualsImpl extends Factory {
      def getBlocks(ft: FT, nft: FT, all: Boolean): Result =
        ft.meta.leftOwner match {
          case t: Ctor.Secondary => Some((t, seq(all, t.body)))
          case t: Tree.WithBody => Some((t.body, Nil))
          case _ => BlockImpl.getBlocks(ft, nft, all)
        }
    }

    private object TryImpl extends Factory {
      def getBlocks(ft: FT, nft: FT, all: Boolean): Result =
        ft.meta.leftOwner match {
          case t: Term.TryClause =>
            Some((t.expr, seq(all, t.catchClause) ++ seq(all, t.finallyp)))
          case _ => None
        }
    }

    private object CatchImpl extends Factory {
      def getBlocks(ft: FT, nft: FT, all: Boolean): Result =
        ft.meta.leftOwner match {
          case t: Term.TryClause => t.catchClause
              .map(x => (x, seq(all, t.expr) ++ seq(all, t.finallyp)))
          case _ => None
        }
    }

    private object FinallyImpl extends Factory {
      def getBlocks(ft: FT, nft: FT, all: Boolean): Result =
        ft.meta.leftOwner match {
          case t: Term.TryClause => t.finallyp
              .map(x => (x, seq(all, t.expr) ++ seq(all, t.catchClause)))
          case _ => None
        }
    }

    private object ElseImpl extends Factory {
      def getBlocks(ft: FT, nft: FT, all: Boolean): Result =
        ft.meta.leftOwner match {
          case x: Term.If if !x.elsep.is[Term.If] =>
            Some((x.elsep, seq(all, x.thenp) ++ seq(all, x.cond)))
          case _ => None
        }
    }

    private object YieldImpl extends Factory {
      def getBlocks(ft: FT, nft: FT, all: Boolean): Result =
        ft.meta.leftOwner match {
          case t: Term.ForYield => Some((t.body, seq(all, t.enumsBlock)))
          case _ => None
        }
    }

  }

  def isBlockWithoutBraces(t: Term.Block): Boolean = t.tokens.head match {
    case lb: T.LeftBrace => lb ne tokens(lb).left
    case _ => true
  }

  def existsBlockIfWithoutElse(t: Term.If): Boolean =
    existsBlockIfWithoutElse(t.thenp, false) ||
      existsBlockIfWithoutElse(t.elsep, ifWithoutElse(t))

  def existsBlockIfWithoutElse(t: Tree, other: => Boolean): Boolean = t match {
    case x: Term.If => existsBlockIfWithoutElse(x)
    case b @ Term.Block((x: Term.If) :: Nil) => isBlockWithoutBraces(b) &&
      existsBlockIfWithoutElse(x)
    case _ => other
  }

  def getEndOfBlock(ft: FT, parensToo: => Boolean)(implicit
      style: ScalafmtConfig,
  ): Option[FT] = ft.left match {
    case _: T.LeftBrace => matchingOptLeft(ft)
    case _: T.LeftParen => if (parensToo) matchingOptLeft(ft) else None
    case _ => OptionalBraces.get(ft)
        .flatMap(_.rightBrace.map(x => nextNonCommentSameLine(x)))
  }

  def isCloseDelimForTrailingCommasMultiple(ft: FT): Boolean =
    ft.meta.rightOwner match {
      case x: Importer => x.importees.lengthCompare(1) > 0
      // take last arg when multiple
      case x => getArgsOrNil(x).view.drop(1).lastOption match {
          case None | Some(_: Term.Repeated) => false
          case Some(t: Term.Param) => !t.decltpe.is[Type.Repeated]
          case _ => true
        }
    }

  def rightIsCloseDelimToAddTrailingComma(left: T, ft: => FT)(implicit
      style: ScalafmtConfig,
  ): Boolean = style.getTrailingCommas match {
    case TrailingCommas.keep => left.is[T.Comma] &&
      TreeOps.rightIsCloseDelimForTrailingComma(left, ft)
    case TrailingCommas.always => TreeOps
        .rightIsCloseDelimForTrailingComma(left, ft)
    case TrailingCommas.multiple =>
      val ftEval = ft
      TreeOps.rightIsCloseDelimForTrailingComma(left, ftEval) &&
      isCloseDelimForTrailingCommasMultiple(ftEval)
    case _ => false
  }

  def getMustDangleForTrailingCommas(getCloseFt: => FT)(implicit
      style: ScalafmtConfig,
  ): Boolean = !style.rewrite.trailingCommas.allowFolding && {
    val closeFt = getCloseFt
    val beforeClose =
      if (!closeFt.left.is[T.Comment]) Some(closeFt.left)
      else {
        val tok = prevNonCommentSameLineBefore(closeFt).left
        if (tok.is[T.Comment]) None else Some(tok)
      }
    beforeClose.exists(rightIsCloseDelimToAddTrailingComma(_, closeFt))
  }

  def getBinpackDefnSiteFlags(ftAfterOpen: => FT, ftBeforeClose: FT)(implicit
      style: ScalafmtConfig,
      clauseSiteFlags: ClauseSiteFlags,
  ): BinpackSiteFlags =
    getBinpackSiteFlags(ftAfterOpen, ftBeforeClose, literalArgList = false)

  def getBinpackCallSiteFlags(ftAfterOpen: FT, ftBeforeClose: FT)(implicit
      style: ScalafmtConfig,
      clauseSiteFlags: ClauseSiteFlags,
  ): BinpackSiteFlags = {
    val literalArgList = styleMap.opensLiteralArgumentList(ftAfterOpen)
    getBinpackSiteFlags(ftAfterOpen, ftBeforeClose, literalArgList)
  }

  def getBinpackSiteFlags(defnSite: Boolean, ftAfterOpen: FT, ftBeforeClose: FT)(
      implicit
      style: ScalafmtConfig,
      clauseSiteFlags: ClauseSiteFlags,
  ): BinpackSiteFlags =
    if (defnSite) getBinpackDefnSiteFlags(ftAfterOpen, ftBeforeClose)
    else getBinpackCallSiteFlags(ftAfterOpen, ftBeforeClose)

  def getBinpackSiteFlags(
      getFtAfterOpen: => FT,
      ftBeforeClose: FT,
      literalArgList: Boolean,
  )(implicit
      style: ScalafmtConfig,
      clauseSiteFlags: ClauseSiteFlags,
  ): BinpackSiteFlags = {
    implicit val configStyle = clauseSiteFlags.configStyle
    val configStylePrefer = configStyle.prefer
    val shouldDangle = dangleCloseDelim
    val sourceIgnored = style.newlines.sourceIgnored
    val configStyleSource = configStylePrefer && !sourceIgnored
    val dangleForTrailingCommas = getMustDangleForTrailingCommas(ftBeforeClose)
    val scalaJsStyle = configStyleSource && !shouldDangle
    val closeBreak = dangleForTrailingCommas || ftBeforeClose.hasBreak

    def noNLPolicy(): Policy = {
      def close = next(ftBeforeClose)
      if (scalaJsStyle) Policy
        .onlyFor(ftBeforeClose, prefix = "tuckSJS")(Decision.noNewlineSplits)
      else style.newlines.source match {
        case Newlines.keep if closeBreak => decideNewlinesOnlyBeforeClose(close)
        case Newlines.fold
            if shouldDangle && !style.binPack.indentCallSiteOnce =>
          decideNewlinesOnlyBeforeCloseOnBreak(close)
        case _ => NoPolicy
      }
    }

    def nlOpenClose(): (Boolean, NlClosedOnOpen) = {
      val ftAfterOpen = getFtAfterOpen
      if (!literalArgList && mustForceConfigStyle(ftAfterOpen))
        (true, NlClosedOnOpen.Cfg)
      else {
        val openBreak = ftAfterOpen.hasBreak
        val nlOpenExcludingCfg = dangleForTrailingCommas ||
          (style.newlines.source match {
            case Newlines.classic => openBreak && shouldDangle && closeBreak
            case Newlines.keep => openBreak
            case _ => false
          }) || isRightCommentWithBreak(ftAfterOpen)
        val nlBothIncludingCfg = !sourceIgnored && {
          val nlBothExcludingCfg =
            if (!shouldDangle) scalaJsStyle && closeBreak
            else nlOpenExcludingCfg && !style.newlines.keepBreak(!closeBreak)
          nlBothExcludingCfg ||
          closeBreak && preserveConfigStyle(ftAfterOpen, true)
        }
        // close on open NL; doesn't cover case with no break after open
        val nlClose = nlBothIncludingCfg || dangleForTrailingCommas ||
          shouldDangle || style.newlines.keepBreak(closeBreak)
        if (!nlClose) (nlOpenExcludingCfg, NlClosedOnOpen.No)
        else {
          val cfg = !literalArgList && configStyleSource
          val dangle = if (cfg) NlClosedOnOpen.Cfg else NlClosedOnOpen.Yes
          (nlBothIncludingCfg || nlOpenExcludingCfg, dangle)
        }
      }
    }

    BinpackSiteFlags(
      literalArgList = literalArgList,
      nlOpenClose = nlOpenClose,
      noNLPolicy = style.newlines.source match {
        case Newlines.unfold => null
        case Newlines.fold if configStylePrefer => null
        case _ => noNLPolicy
      },
      scalaJsStyle = scalaJsStyle,
    )
  }

  @tailrec
  final def scalaJsOptCloseOnRight(
      ftBeforeClose: FT,
      bpFlags: BinpackSiteFlags,
  ): FT =
    if (bpFlags.scalaJsStyle) {
      val ftAfterClose = nextNonCommentAfter(ftBeforeClose)
      val continue = ftAfterClose != ftBeforeClose &&
        ftAfterClose.right.is[T.RightParen] && ftAfterClose.noBreak &&
        isArgClauseSite(ftAfterClose.meta.rightOwner)
      if (continue) {
        val open = matchingRight(ftAfterClose)
        implicit val style: ScalafmtConfig = styleMap.at(open)
        implicit val clauseSiteFlags: ClauseSiteFlags = ClauseSiteFlags
          .atCallSite(ftAfterClose.meta.rightOwner)
        val bpFlagsAfter = getBinpackCallSiteFlags(open, ftAfterClose)
        scalaJsOptCloseOnRight(ftAfterClose, bpFlagsAfter)
      } else ftBeforeClose
    } else ftBeforeClose

  object BinPackOneline {

    private def noRighDelim(xtok: T, xft: FT): Option[FT] = xtok match {
      case _: T.CloseDelim => None
      case _: T.Comma => Some(null) // trailing comma, has NL
      case _: T.Comment => if (xft.noBreak) None else Some(null)
      case _ => Some(xft)
    }

    @inline
    def getBeforeRightDelims(ft: FT): FT =
      findTokenWith(ft, prev)(xft => noRighDelim(xft.left, xft)).merge
    @inline
    def getAfterRightDelims(ft: FT): FT =
      findTokenWith(ft, next)(xft => noRighDelim(xft.right, xft)).merge

    private def policyOnRightDelim(
        ft: FT,
        exclude: TokenRanges,
    ): (Option[FT], Policy) = {
      val beforeDelims = getBeforeRightDelims(ft)
      if (beforeDelims eq null) return (None, NoPolicy)

      val afterDelims = getAfterRightDelims(ft)
      if (afterDelims eq null) return (None, NoPolicy)

      def closeBreakPolicy() = {
        @tailrec
        def iter(currft: FT): Option[Policy] = {
          val prevft = prevNonComment(currft)
          val breakBeforeClose = matchingOptLeft(prevft) match {
            case Some(open) =>
              val cfg = styleMap.at(open)
              val owner = prevft.meta.leftOwner
              def cfgStyle = cfg.newlines.configStyle.getCallSite(owner).prefer
              def dangle = cfg.danglingParentheses.atCallSite(owner)
              cfg.newlines.source match {
                case Newlines.unfold => true
                case Newlines.fold => cfgStyle ||
                  !cfg.binPack.indentCallSiteOnce
                case _ => !cfgStyle || dangle || prev(prevft).hasBreak
              }
            case _ => false
          }
          if (breakBeforeClose) Some(decideNewlinesOnlyBeforeClose(prevft))
          else if (prevft eq beforeDelims) None
          else iter(prev(prevft))
        }

        iter(afterDelims)
      }

      def policyWithDelay(policy: Policy) =
        // force break if multiline and if there's no other break
        delayedBreakPolicy(Policy.End >= beforeDelims, exclude)(
          Policy.RelayOnSplit { case (s, nextft) =>
            s.isNL && nextft.idx > beforeDelims.idx // don't need anymore
          }(policy)(NoPolicy),
        )

      (afterDelims.right match {
        case _: T.Dot => // check if Dot rule includes a break option
          afterDelims -> GetSelectLike.onRightOpt(afterDelims).map { x =>
            implicit val cfg = styleMap.at(afterDelims)
            cfg.newlines.getSelectChains match {
              case Newlines.classic =>
                val (expireTree, nextSelect) = findLastApplyAndNextSelect(
                  x.tree,
                  cfg.newlines.encloseSelectChains,
                )
                Right(canStartSelectChain(x, nextSelect, expireTree))
              case Newlines.keep => Left(Policy.onlyFor(afterDelims, "BP1L.NL")(
                  Decision.onlyNewlineSplits(_)
                    .map(_.preActivateFor(SplitTag.SelectChainBinPackNL)),
                ))
              case _ => Right(true)
            }
          }.getOrElse(Right(false))
        case x @ LeftParenOrBracket() =>
          val nft = nextNonCommentSameLineAfter(afterDelims)
          nft.right match {
            case _: T.Comment => null
            case _ => // check if break would cause cfg style but not otherwise
              val cfg = styleMap.at(x)
              val owner = afterDelims.meta.rightOwner
              val ok = cfg.newlines.sourceIgnored || ! {
                cfg.newlines.configStyle.getCallSite(owner).prefer &&
                cfg.danglingParentheses.atCallSite(owner)
              } || next(afterDelims).hasBreak
              nft -> Right(ok)
          }
        case _ => null
      }) match {
        case null => (None, NoPolicy)
        case (nft, policyOrOkToBreak) =>
          val policyOpt = policyOrOkToBreak match {
            case Left(policy) => Some(policy)
            case Right(okToBreak) if !okToBreak => closeBreakPolicy()
            case _ => Some(decideNewlinesOnlyBeforeToken(next(nft)))
          }
          (Some(nft), policyOpt.fold(Policy.noPolicy)(policyWithDelay))
      }
    }

    def getPolicy(isCallSite: Boolean, exclude: TokenRanges)(
        afterArg: FT,
    ): (Option[FT], Policy) = afterArg.right match {
      case _: T.Comma // check for trailing comma, which needs no breaks
          if !nextNonCommentAfter(afterArg).right.is[T.CloseDelim] =>
        (None, splitOneArgPerLineAfterCommaOnBreak(exclude)(next(afterArg)))
      case _: T.CloseDelim if isCallSite => policyOnRightDelim(afterArg, exclude)
      case _ => (None, NoPolicy)
    }
  }

  def indentedPackage(body: Pkg.Body): Boolean = getHeadOpt(body)
    .exists(_.meta.leftOwner eq body)
  @inline
  def indentedPackage(pkg: Pkg): Boolean = indentedPackage(pkg.body)

  def insideBracesBlockIfBracesToParens(
      rb: FT,
      mod: Modification,
      isWithinBraces: Boolean,
  )(implicit style: ScalafmtConfig, ft: FT): Option[TokenRanges] = {
    val ok = initStyle.rewrite.bracesToParensForOneLineApply &&
      style.rewrite.trailingCommas.isOptional && (mod eq Space) &&
      !(isWithinBraces && style.spaces.inParentheses) &&
      RedundantBraces.canRewriteWithParensOnRightBrace(rb)
    if (ok) {
      // the rule for `(...` excludes brace blocks
      // check if all of them can be converted to parens
      val nft = if (isWithinBraces) ft else next(ft)
      def getTokenRanges = {
        val tr = insideBracesBlock(nft, rb)
          .filter(x => !couldHaveBracesConvertedToParens(x.lt.leftOwner))
        if (tr.ranges.exists(!_.lt.left.is[T.LeftBrace])) null else Some(tr)
      }
      (nft.leftOwner match {
        case Term.Block(arg :: Nil) if style.newlines.fold => Some(arg)
        case Term.ArgClause(arg :: Nil, _) if style.newlines.fold => Some(arg)
        case _ => None
      }) match {
        case Some(_: Term.FunctionTerm) => Some(TokenRanges.empty)
        case Some(arg) if isTreeEndingInArgumentClause(arg) =>
          Some(parensTuple(arg))
        case _ => getTokenRanges
      }
    } else None
  }

  def getBracesToParensMod(
      rb: FT,
      mod: Modification = Space,
      isWithinBraces: Boolean = true,
  )(implicit
      style: ScalafmtConfig,
      ft: FT,
  ): (Modification, Option[TokenRanges]) = {
    val tr = insideBracesBlockIfBracesToParens(rb, mod, isWithinBraces)
    if ((tr eq null) || tr.isEmpty) (mod, tr)
    else (SpaceOrNoSplit(Policy.End < rb), tr)
  }

  @inline
  def getBracesToParensModOnly(
      rb: FT,
      mod: Modification = Space,
      isWithinBraces: Boolean = true,
  )(implicit style: ScalafmtConfig, ft: FT): Modification =
    getBracesToParensMod(rb, mod, isWithinBraces)._1

  @tailrec
  private def getSingleFunctionArg(
      values: List[Tree],
  ): Option[Term.FunctionTerm] = values match {
    case (t: Term.FunctionTerm) :: Nil => Some(t)
    case (t: Term.Block) :: Nil if !isEnclosedInBraces(t) =>
      getSingleFunctionArg(t.stats)
    case _ => None
  }

  val LambdaAtSingleArgCallSite = new FT.ExtractFromMeta(_.leftOwner match {
    case Term.ArgClause(v, None) => getSingleFunctionArg(v)
    case _ => None
  })

}

object FormatOps {
  class SelectLike(val tree: Term, val qual: Term, val nameFt: FT) {}

  object SelectLike {
    def apply(tree: Term.Select)(implicit ftoks: FormatTokens): SelectLike =
      new SelectLike(tree, tree.qual, ftoks.getHead(tree.name))
    def apply(tree: Term.SelectMatch, kw: FT): SelectLike =
      new SelectLike(tree, tree.expr, kw)
  }

  case class TemplateSupertypeGroup(
      superType: Tree,
      superTypeGroup: Seq[Tree],
      expireTokenFunc: Seq[Tree] => FT,
  ) {
    def getExpireToken = expireTokenFunc(superTypeGroup)
    def isSecond = superTypeGroup.drop(1).headOption.contains(superType)
  }

  def nextLine(implicit fl: FileLine): FileLine = {
    val line = fl.line
    new FileLine(fl.file, line.copy(value = line.value + 1))
  }

  abstract class OptionalBracesRegion {
    def owner: Option[Tree]
    def splits: Option[Seq[Split]]
    def rightBrace: Option[FT]
  }

  def getOpenParenAlignIndents(
      end: FT,
  )(implicit style: ScalafmtConfig): Seq[Indent] =
    if (style.align.closeParenSite) Seq(
      Indent(Length.StateColumn, end, ExpiresOn.After),
      Indent(1, end, ExpiresOn.Before),
      Indent(-1, end, ExpiresOn.After),
    )
    else Seq(Indent(Length.StateColumn, end, ExpiresOn.Before))

  private[internal] sealed trait NlClosedOnOpen
  private[internal] object NlClosedOnOpen {
    case object No extends NlClosedOnOpen
    case object Yes extends NlClosedOnOpen
    case object Cfg extends NlClosedOnOpen
  }

  private[internal] case class BinpackSiteFlags(
      literalArgList: Boolean,
      nlOpenClose: () => (Boolean, NlClosedOnOpen),
      noNLPolicy: () => Policy, // nullable
      scalaJsStyle: Boolean,
  )

  case class ClauseSiteFlags(
      configStyle: Newlines.ConfigStyleElement,
      alignOpenDelim: Boolean = false,
      dangleCloseDelim: Boolean = true,
  )

  object ClauseSiteFlags {
    def apply(owner: Tree, defnSite: Boolean)(implicit
        style: ScalafmtConfig,
    ): ClauseSiteFlags = if (defnSite) atDefnSite(owner) else atCallSite(owner)

    def atDefnSite(
        owner: Tree,
    )(implicit style: ScalafmtConfig): ClauseSiteFlags = ClauseSiteFlags(
      configStyle = style.newlines.configStyle.getDefnSite(owner),
      alignOpenDelim = style.align.atDefnSite(owner),
      dangleCloseDelim = style.danglingParentheses.atDefnSite(owner),
    )

    def atCallSite(
        owner: Tree,
    )(implicit style: ScalafmtConfig): ClauseSiteFlags = ClauseSiteFlags(
      configStyle = style.newlines.configStyle.getCallSite(owner),
      alignOpenDelim = style.align.atCallSite(owner),
      dangleCloseDelim = style.danglingParentheses.atCallSite(owner),
    )
  }

  def preferConfigStyle(implicit clauseSiteFlags: ClauseSiteFlags): Boolean =
    clauseSiteFlags.configStyle.prefer

  def dangleCloseDelim(implicit clauseSiteFlags: ClauseSiteFlags): Boolean =
    clauseSiteFlags.dangleCloseDelim

  def alignOpenDelim(implicit clauseSiteFlags: ClauseSiteFlags): Boolean =
    clauseSiteFlags.alignOpenDelim

  val ImplicitUsingOnLeft =
    new FT.ExtractFromMeta(meta => TreeOps.getImplicitParamList(meta.leftOwner))

  val WithTemplateOnLeft = new FT.ExtractFromMeta(_.leftOwner match {
    case lo: Stat.WithTemplate => Some(lo.templ)
    case _ => None
  })

  val TemplateOnRight = new FT.ExtractFromMeta(_.rightOwner match {
    case ro: Template => Some(ro)
    case _ => None
  })

  val EnumeratorAssignRhsOnLeft = new FT.ExtractFromMeta(_.leftOwner match {
    case x: Enumerator.Assign => Some(x.rhs)
    case _ => None
  })

  @tailrec
  private def getBlockWithNonSingleTermStat(t: Term.Block): Option[Term.Block] =
    t.stats match {
      case (x: Term.Block) :: Nil => getBlockWithNonSingleTermStat(x)
      case (_: Term) :: Nil => None
      case _ :: _ => Some(t)
      case _ => None
    }

}
