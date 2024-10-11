package org.scalafmt.internal

import org.scalafmt.config.BinPack
import org.scalafmt.config.IndentOperator
import org.scalafmt.config.Indents
import org.scalafmt.config.Newlines
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.config.ScalafmtOptimizer
import org.scalafmt.config.TrailingCommas
import org.scalafmt.internal.Length.Num
import org.scalafmt.internal.Policy.NoPolicy
import org.scalafmt.util.InfixApp._
import org.scalafmt.util._

import org.scalameta.FileLine
import scala.meta._
import scala.meta.classifiers.Classifier
import scala.meta.internal.tokens.Chars.isOperatorPart
import scala.meta.tokens.{Token => T}

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
    FormatTokens(topSourceTree.tokens, owners)(initStyle)
  import tokens._

  private[internal] implicit val soft: SoftKeywordClasses =
    new SoftKeywordClasses(dialect)

  val (forceConfigStyle, emptyQueueSpots) = getForceConfigStyle

  val optimizationEntities = OptimizationEntities(topSourceTree)

  @inline
  def owners(token: T): Tree = ownersMap(hash(token))

  @inline
  final def findFirst(start: FormatToken, end: T)(
      f: FormatToken => Boolean,
  ): Option[FormatToken] = findFirst(start, end.end)(f)

  @tailrec
  final def findFirst(start: FormatToken, end: Int)(
      f: FormatToken => Boolean,
  ): Option[FormatToken] =
    if (start.left.end >= end) None
    else if (f(start)) Some(start)
    else {
      val next_ = next(start)
      if (next_ == start) None else findFirst(next_, end)(f)
    }

  def findFirstOnRight[A](start: FormatToken, end: T)(implicit
      classifier: Classifier[T, A],
  ): Option[FormatToken] = findFirst(start, end.start)(x => classifier(x.right))

  @tailrec
  final def getSlbEndOnLeft(start: FormatToken, end: Int = Int.MaxValue)(
      implicit style: ScalafmtConfig,
  ): FormatToken = {
    val nft = start.right match {
      case t if t.end >= end => start
      case _: T.EOF => start
      case _: T.Comma | _: T.Semicolon | _: T.RightArrow | _: T.Equals |
          _: T.Interpolation.Start | _: T.Interpolation.SpliceEnd |
          _: T.Interpolation.End | _: T.Interpolation.SpliceStart |
          _: T.Interpolation.Part => null
      case _ if start.hasBlankLine => start
      case _ if !style.newlines.formatInfix && {
            isInfixOp(start.rightOwner) ||
            isInfixOp(prevNonComment(start).leftOwner)
          } => if (start.hasBreak) start else null
      case _: T.LeftParen if (start.rightOwner match {
            case _: Member.ArgClause =>
              !style.newlines.isBeforeOpenParenCallSite
            case t => !isJustBeforeTree(start)(t)
          }) => null
      case t: T.RightParen =>
        if (start.left.is[T.LeftParen]) null
        else {
          val owner = start.rightOwner
          val isDefnSite = isParamClauseSite(owner)
          implicit val clauseSiteFlags: ClauseSiteFlags =
            ClauseSiteFlags(owner, isDefnSite)
          val bpFlags = getBinpackSiteFlags(matching(t), start, false)
          if (bpFlags.scalaJsStyle) scalaJsOptCloseOnRight(start, bpFlags)
          else if (
            !start.left.is[T.RightParen] ||
            !style.newlines.fold && clauseSiteFlags.dangleCloseDelim
          ) start
          else null
        }
      case _: T.LeftBrace
          if start.left.is[T.RightParen] &&
            start.leftOwner.is[Member.SyntaxValuesClause] &&
            start.rightOwner.parent.exists {
              case p: Tree.WithBody => p.body eq start.rightOwner
              case _ => false
            } => null
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

  final def endOfSingleLineBlock(start: FormatToken)(implicit
      style: ScalafmtConfig,
  ): T = getSlbEndOnLeft(start).left

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

  val StartsStatementRight = new ExtractFromMeta[Tree](meta =>
    optimizationEntities.statementStarts.get(meta.idx + 1),
  )

  def parensTuple(token: T): TokenRanges = matchingOpt(token)
    .fold(TokenRanges.empty)(other => TokenRanges(TokenRange(token, other.left)))

  def insideBlock[A](start: FormatToken, end: T)(implicit
      classifier: Classifier[T, A],
  ): TokenRanges = insideBlock(start, end, x => classifier(x.left))

  def insideBlock(
      start: FormatToken,
      end: T,
      matches: FormatToken => Boolean,
  ): TokenRanges = insideBlock { x =>
    if (matches(x)) matchingOpt(x.left) else None
  }(start, end)

  def insideBracesBlock(start: FormatToken, end: T, parensToo: Boolean = false)(
      implicit style: ScalafmtConfig,
  ): TokenRanges = insideBlock(x => getEndOfBlock(x, parensToo))(start, end)

  def insideBlock(
      matches: FormatToken => Option[FormatToken],
  )(start: FormatToken, end: T): TokenRanges = {
    var result = TokenRanges.empty

    @tailrec
    def run(tok: FormatToken): Unit = if (tok.left.start < end.start) {
      val nextTokOpt = matches(tok).flatMap { closeFt =>
        val open = tok.left
        val close = closeFt.left
        if (open.start >= close.end) None
        else {
          result = result.append(TokenRange(open, close))
          Some(closeFt)
        }
      }
      val nextTok = nextTokOpt.getOrElse(next(tok))
      if (nextTok ne tok) run(nextTok)
    }

    run(next(start))
    result
  }

  // invoked on closing paren, part of ParamClause
  @tailrec
  final def defnSiteLastToken(t: Tree): Option[FormatToken] = t match {
    case _: Term.ParamClause | _: Type.FuncParamClause | _: Type.FunctionType |
        _: Member.ParamClauseGroup => t.parent match {
        case Some(p) => defnSiteLastToken(p)
        case _ => None
      }
    case t: Defn.Macro => tokenBeforeOpt(t.body).map(prevNonCommentBefore)
    case t: Tree.WithBody => tokenBeforeOpt(t.body)
    case t: Stat.WithTemplate => tokenBeforeOpt(t.templ)
    case t: Decl => getLastOpt(t)
    case _ => None
  }

  @inline
  def splitOneArgOneLine(close: T, owner: Tree)(implicit
      fileLine: FileLine,
      style: ScalafmtConfig,
  ): Policy =
    if (style.poorMansTrailingCommasInConfigStyle) Policy
      .before(close, prefix = "B[,]")(splitOneArgPerLineBeforeComma(owner))
    else Policy
      .before(close, prefix = "A[,]")(splitOneArgPerLineAfterComma(owner))

  def splitOneArgPerLineBeforeComma(owner: Tree): Policy.Pf = {
    // TODO(olafur) clear queue between arguments, they are independent.
    case Decision(t @ FormatToken(_, _: T.Comma, _), splits)
        if owner == t.meta.rightOwner && !next(t).right.is[T.Comment] =>
      splits.map(x => if (x.mod ne NoSplit) x else x.withMod(Newline))

    case Decision(t @ FormatToken(_: T.Comma, right, _), splits)
        if owner == t.meta.leftOwner && !right.is[T.LeftBrace] &&
          // If comment is bound to comma, see unit/Comment.
          (!right.is[T.Comment] || t.hasBreak) =>
      val isNewline = right.is[T.Comment]
      splits.filter(_.isNL == isNewline)
  }

  def splitOneArgPerLineAfterComma(owner: Tree): Policy.Pf = {
    // Newline on every comma.
    case Decision(t @ FormatToken(_: T.Comma, right, m), splits)
        if owner == m.leftOwner &&
          // TODO(olafur) what the right { decides to be single line?
          // If comment is bound to comma, see unit/Comment.
          (!right.is[T.Comment] || t.hasBreak) =>
      getOneArgPerLineSplitsAfterComma(right, splits)
  }

  def splitOneArgPerLineAfterCommaOnBreak(comma: T)(implicit
      fileLine: FileLine,
  ): Policy = splitOneArgPerLineAfterCommaOnBreak(TokenRanges.empty)(comma)

  def splitOneArgPerLineAfterCommaOnBreak(
      exclude: TokenRanges,
  )(comma: T)(implicit fileLine: FileLine): Policy = {
    val policy = Policy.after(comma, prefix = "NL->A[,]") {
      case Decision(t @ FormatToken(`comma`, right, _), splits)
          if !right.is[T.Comment] || t.hasBreak =>
        getOneArgPerLineSplitsAfterComma(right, splits)
    }
    delayedBreakPolicy(Policy.End < comma, exclude)(policy)
  }

  private def getOneArgPerLineSplitsAfterComma(r: T, s: Seq[Split]) =
    if (r.is[T.LeftBrace]) SplitTag.OneArgPerLine.activateOnly(s)
    else Decision.onlyNewlineSplits(s)

  def templateCurly(obj: Template.Body): Option[FormatToken] = getHeadOpt(obj)
    .map(x => if (x.meta.leftOwner eq obj) x else prevNonCommentBefore(x))

  def templateCurlyOrLastNonTrivial(tpl: Template): FormatToken =
    templateCurly(tpl.body).getOrElse(getLastNonTrivial(tpl))

  def templateDerivesOrCurlyOrLastNonTrivial(template: Template)(implicit
      ft: FormatToken,
  ): FormatToken = findTemplateGroupOnRight(_.getExpireToken)(template)
    .getOrElse(templateCurlyOrLastNonTrivial(template))

  private def findTreeInGroup[A](
      trees: Seq[Tree],
      func: TemplateSupertypeGroup => A,
  )(expireFunc: Seq[Tree] => FormatToken)(implicit ft: FormatToken): Option[A] =
    trees.find(_.pos.end >= ft.right.end).map { x =>
      func(TemplateSupertypeGroup(x, trees, expireFunc))
    }

  def findTemplateGroupOnRight[A](
      func: TemplateSupertypeGroup => A,
  )(template: Template)(implicit ft: FormatToken): Option[A] = {
    @tailrec
    def iter(groups: Seq[Seq[Tree]]): Option[A] =
      if (isSeqSingle(groups))
        // for the last group, return '{' or ':'
        findTreeInGroup(groups.head, func) { x =>
          getHeadOpt(template.body).getOrElse(getLastNonTrivial(x.last))
        }
      else {
        // for intermediate groups, return its last token
        val res = findTreeInGroup(groups.head, func)(tokenAfter)
        if (res.isDefined) res else iter(groups.tail)
      }
    getTemplateGroups(template).flatMap(iter)
  }

  def getBreakBeforeElsePolicy(els: T): Policy = Policy
    .on(els, prefix = "ELSE") {
      case d @ Decision(FormatToken(_, `els`, _), _) => d
          .onlyNewlinesWithFallback(Split(Newline, 0))
    }

  def getBreakBeforeElsePolicy(term: Term.If): Policy = getElseToken(term)
    .flatMap { case (_, elsOpt) => elsOpt.map(getBreakBeforeElsePolicy) }

  def getBreaksBeforeElseChainPolicy(term: Term.If): Policy =
    getElseChain(term, Nil).foldLeft(Policy.noPolicy) { case (res, els) =>
      getBreakBeforeElsePolicy(els) ==> res
    }

  private final def getElseToken(
      term: Term.If,
  ): Option[(FormatToken, Option[T.KwElse])] = getHeadOpt(term.elsep)
    .map { ftElsep =>
      val beforeElsep = prevNonCommentBefore(ftElsep)
      val elsOpt = beforeElsep.left match {
        case els: T.KwElse
            if initStyle.newlines.alwaysBeforeElseAfterCurlyIf ||
              !prev(beforeElsep).left.is[T.RightBrace] => Some(els)
        case _ => None
      }
      (ftElsep, elsOpt)
    }

  @tailrec
  private final def getElseChain(term: Term.If, res: List[T]): List[T] =
    getElseToken(term) match {
      case Some((ftElsep, elsOpt)) =>
        val newRes = elsOpt.fold(res)(_ :: res)
        term.elsep match {
          case t: Term.If => getElseChain(t, newRes)
          case b @ Term.Block((t: Term.If) :: Nil)
              if !areMatching(ftElsep.left)(getLastToken(b)) =>
            getElseChain(t, newRes)
          case _ => newRes
        }
      case _ => res
    }

  def getOptimalTokenFor(token: T): T = getOptimalTokenFor(tokens(token))

  def getOptimalTokenFor(ft: FormatToken): T =
    if (isAttachedCommentThenBreak(ft)) ft.right else ft.left

  def insideInfixSplit(
      app: Member.Infix,
  )(implicit style: ScalafmtConfig, ft: FormatToken): Seq[Split] = {
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
          case x => !isEnclosedInParens(x)
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

      val afterInfix = style.breakAfterInfix(app)
      if (afterInfix ne Newlines.AfterInfix.keep)
        if (isBeforeOp) Seq(Split(spaceMod, 0))
        else {
          val fullInfix = InfixSplits.findEnclosingInfix(app)
          val ok = isEnclosedInParens(fullInfix) || fullInfix.parent.forall {
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
        val (fullInfix, isFullInfixEnclosed) = InfixSplits
          .findMaybeEnclosingInfix(app)
        def okToBreak: Boolean = !isBeforeOp || isFullInfixEnclosed ||
          initStyle.dialect.allowInfixOperatorAfterNL ||
          (fullInfix.parent match {
            case Some(p: Case) => p.cond.contains(fullInfix)
            case _ => false
          }) || { // check if the break was in the original code
            val atokens = app.tokens
            val ltidx = atokens.indexOf(ft.left)
            ltidx >= 0 && {
              val rtidx = atokens.skipIf(_.is[T.Trivia], ltidx + 1)
              rtidx >= 0 && (atokens(rtidx) eq ft.right) // no rewritten tokens
            }
          }
        val mod =
          if (ft.noBreak || !okToBreak) spaceMod
          else Newline2x(isFullInfixEnclosed && ft.hasBlankLine)
        val split = Split(mod, 0)
        if (isBeforeOp && isFewerBracesRhs(app.arg)) Seq(split)
        else Seq(InfixSplits.withNLIndent(split, app, fullInfix))
      }
    }
  }

  def getInfixSplitsBeforeLhs(
      lhsApp: Member.Infix,
      afterInfix: Newlines.AfterInfix,
      newStmtMod: Option[Modification] = None,
  )(implicit style: ScalafmtConfig, ft: FormatToken): Seq[Split] = {
    val fullInfixTreeOpt = findTreeWithParentSimple(lhsApp, false)(isInfixApp)
    val fullInfix = fullInfixTreeOpt.flatMap(asInfixApp).getOrElse(lhsApp)
    val app = findLeftInfix(fullInfix)
    new InfixSplits(app, ft, fullInfix, app)
      .getBeforeLhsOrRhs(afterInfix, newStmtMod)
  }

  final def maybeGetInfixSplitsBeforeLhs(mod: => Option[Modification] = None)(
      nonInfixSplits: => Seq[Split],
  )(implicit style: ScalafmtConfig, ft: FormatToken): Seq[Split] = {
    val tree = ft.meta.rightOwner
    val ai = style.breakAfterInfix(tree)
    val app = if (ai eq Newlines.AfterInfix.keep) None else asInfixApp(tree)
    app.fold(nonInfixSplits)(getInfixSplitsBeforeLhs(_, ai, mod))
  }

  private[internal] object InfixSplits {

    def apply(app: Member.Infix, ft: FormatToken)(implicit
        style: ScalafmtConfig,
    ): InfixSplits = apply(app, ft, findEnclosingInfix(app))

    def apply(app: Member.Infix, ft: FormatToken, fullInfix: Member.Infix)(
        implicit style: ScalafmtConfig,
    ): InfixSplits = {
      val leftInfix = findLeftInfix(fullInfix)
      new InfixSplits(app, ft, fullInfix, leftInfix)
    }

    private def switch(splits: Seq[Split], triggers: T*): Seq[Split] = splits
      .map { x =>
        triggers.foldLeft(x) { case (y, trigger) => y.switch(trigger, false) }
      }

    @tailrec
    private def findMaybeEnclosingInfix(
        child: Member.Infix,
        childTree: Tree,
    ): (Member.Infix, Boolean) =
      if (isEnclosedInParens(childTree)) (child, true)
      else childTree.parent match {
        case Some(p: Member.Infix) if !p.isAssignment =>
          findMaybeEnclosingInfix(p, p)
        case Some(p @ Member.ArgClause(_ :: Nil)) =>
          findMaybeEnclosingInfix(child, p)
        case _ => (child, false)
      }

    private[FormatOps] def findMaybeEnclosingInfix(
        app: Member.Infix,
    ): (Member.Infix, Boolean) = findMaybeEnclosingInfix(app, app)

    private[FormatOps] def findEnclosingInfix(app: Member.Infix): Member.Infix =
      findMaybeEnclosingInfix(app)._1

    def withNLIndent(split: Split)(
        app: Member.Infix,
    )(implicit ft: FormatToken, style: ScalafmtConfig): Split =
      withNLIndent(split, app, findEnclosingInfix(app))

    def withNLIndent(
        split: Split,
        app: Member.Infix,
        fullInfix: => Member.Infix,
    )(implicit ft: FormatToken, style: ScalafmtConfig): Split = {
      val noNL = !split.isNL && {
        val nextFt = nextNonCommentSameLine(ft)
        nextFt.eq(ft) || nextFt.noBreak
      }
      if (noNL) split else apply(app, ft, fullInfix).withNLIndent(split)
    }

  }

  private[internal] class InfixSplits(
      app: Member.Infix,
      ft: FormatToken,
      fullInfix: Member.Infix,
      leftInfix: Member.Infix,
  )(implicit style: ScalafmtConfig) {
    private val isLeftInfix = leftInfix eq app
    private val isAfterOp = ft.meta.leftOwner eq app.op
    private val beforeLhs = !isAfterOp && ft.left.start < app.pos.start
    private val isFirstOp = beforeLhs || isLeftInfix
    private val fullExpire = getLastEnclosedToken(fullInfix)

    private val assignBodyExpire = {
      val prevFt = tokenBefore(fullInfix)
      val prevOwner = prevFt.meta.leftOwner
      prevFt.left match {
        case _: T.Equals => Some(getLastToken(prevOwner))
        case _: T.LeftParen | _: T.LeftBracket
            if fullInfix.parent.contains(prevOwner) && !(prevOwner match {
              case po: Member.ArgClause => po.parent.exists(isInfixApp)
              case po => isInfixApp(po)
            }) && isSeqSingle(getArgsOrNil(prevOwner)) =>
          Some(getLastToken(fullInfix))
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
                case head: Token.LeftBrace =>
                  (tokens.before(head).left eq head) ||
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
      val allowNoIndent = style.indentOperator.getExemptScope match {
        case IndentOperator.Exempt.all => true
        case IndentOperator.Exempt.oldTopLevel => isOldTopLevel(getChild)
        case IndentOperator.Exempt.aloneEnclosed => isAloneEnclosed(getChild)
        case IndentOperator.Exempt.aloneArgOrBody => isAloneArgOrBody(getChild)
      }
      def isInfixTopLevelMatch(op: String, noindent: Boolean): Boolean =
        noindent == style.indentOperator.noindent(op) &&
          noindent == allowNoIndent
      if (style.verticalAlignMultilineOperators) isAfterAssignmentOp(false)
      else if (beforeLhs) assignBodyExpire.isEmpty
      else if (
        !app.singleArg.exists { x =>
          x.is[Term.Block] || x.is[Term.NewAnonymous]
        } && isInfixTopLevelMatch(ft.meta.left.text, false)
      ) false
      else if (isInfixTopLevelMatch(app.op.value, true)) true
      else if (app.is[Pat] && isChildOfCaseClause(app)) true
      else false
    }

    private val fullIndent: Indent = assignBodyExpire match {
      case Some(x) if beforeLhs =>
        Indent(Num(style.indent.main), x, ExpiresOn.After)
      case None if isLeftInfix && isAfterAssignmentOp(true) =>
        Indent(Num(style.indent.main), fullExpire, ExpiresOn.After)
      case _ =>
        val len = style.indent.getAfterInfixSite
        Indent(Num(len), fullExpire, ExpiresOn.After)
    }

    val (nlIndent, nlPolicy) = {
      def policy(triggers: T*)(implicit fileLine: FileLine) =
        Policy ? triggers.isEmpty || Policy.on(fullExpire, prefix = "INF") {
          case Decision(t: FormatToken, s)
              if isInfixOp(t.meta.leftOwner) ||
                isInfixOp(t.meta.rightOwner) &&
                !t.meta.rightOwner.parent.exists(style.formatInfix(_)) =>
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
        val ind =
          if (isFirstOp) fullIndent else Indent.before(fullIndent, opTok)
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
        afterInfix: Newlines.AfterInfix,
        newStmtMod: Option[Modification] = None,
        spaceMod: Modification = Space,
    )(implicit ft: FormatToken): Seq[Split] = {
      val maxPrecedence =
        if (isAfterOp) infixSequenceMaxPrecedence(fullInfix) else 0 // 0 unused
      val closeOpt = matchingOpt(ft.right)
      val expiresOpt =
        if (closeOpt.isDefined) None
        else {
          val res = mutable.Buffer.empty[Member.Infix]
          findNextInfixes(fullInfix, app.lhs, res)
          val infixes = if (isAfterOp) res.toSeq.tail else res.toSeq
          val filtered =
            if (!style.newlines.afterInfixBreakOnNested) infixes
            else infixes.takeWhile(x => !isEnclosedInParens(x.lhs))
          if (filtered.isEmpty) None
          else {
            val res = filtered.foldLeft(Seq.empty[(T, Int)]) { case (out, ia) =>
              val cost = maxPrecedence - ia.precedence
              if (out.nonEmpty && out.head._2 <= cost) out
              else (getMidInfixToken(ia) -> cost) +: out
            }
            Some(res)
          }
        }

      val breakPenalty = if (isAfterOp) maxPrecedence - app.precedence else 1
      val expires = expiresOpt.fold(Seq(fullExpire -> 0)) { x =>
        (if (x.head._2 == 0) x else (fullExpire -> 0) +: x).reverse
      }

      val infixTooLong = infixSequenceLength(fullInfix) >
        style.newlines.afterInfixMaxCountPerExprForSome
      val breakMany = infixTooLong || afterInfix == Newlines.AfterInfix.many
      val rightAsInfix = asInfixApp(ft.meta.rightOwner)

      def breakAfterComment(t: FormatToken) = {
        val end = nextNonCommentSameLine(t)
        Policy ? end.right.isAny[T.LeftBrace, T.Comment] || {
          if (end eq t) decideNewlinesOnlyAfterToken(end.left)
          else decideNewlinesOnlyAfterClose(end.left)
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
          val indent = Indent(Num(indentLen), expire, ExpiresOn.After)
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
          if (!style.newlines.afterInfixBreakOnNested) None
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
          .withSingleLine(endOfNextOp.getOrElse(closeFt).left)
          .andPolicy(breakAfterClose)
          .andPolicy(getSingleLineInfixPolicy(closeFt.left))
        Seq(singleLineSplit, nlSplit)
      }

      val spaceSplits: Seq[Split] =
        if (ft.right.is[T.Comment]) Seq.empty
        else if (closeOpt.isDefined) Seq.empty
        else {
          val nextFT = if (rightAsInfix.isDefined) next(ft) else ft
          expires.filter(_._2 <= breakPenalty).takeRight(3).map {
            case (expire, cost) =>
              val exclude =
                if (breakMany) TokenRanges.empty
                else insideBracesBlock(nextFT, expire, true)
              Split(ModExt(newStmtMod.getOrElse(spaceMod)), cost)
                .withSingleLine(expire, exclude, noOptimal = cost != 0)
          }
        }

      singleLineSplits ++ spaceSplits ++ otherSplits
    }

  }

  def getSingleLineInfixPolicy(end: T) = Policy.on(end, prefix = "INFSLB") {
    case Decision(t: FormatToken, s) if isInfixOp(t.meta.leftOwner) =>
      SplitTag.InfixChainNoNL.activateOnly(s)
  }

  def getMidInfixToken(app: Member.Infix): T = {
    val opToken = app.op.tokens.head
    val opFollowsComment = tokens(opToken, -1).left.is[T.Comment]
    if (opFollowsComment) getLastNonTrivialToken(app.lhs) else opToken
  }

  def getLastEnclosedToken(tree: Tree): T = getLastExceptParen(tree).left

  @tailrec
  private def findNextInfixes(
      fullTree: Tree,
      tree: Tree,
      res: mutable.Buffer[Member.Infix],
  ): Unit = if (tree ne fullTree) tree.parent match {
    case Some(ia: Member.Infix) =>
      if (ia.lhs eq tree) {
        res += ia
        findNestedInfixes(ia.arg, res)
      }
      findNextInfixes(fullTree, ia, res)
    case Some(p: Member.ArgClause) => p.parent match {
        case Some(pp: Member.Infix) => findNextInfixes(fullTree, pp, res)
        case _ =>
      }
    case _ =>
  }

  private def findNestedInfixes(
      tree: Tree,
      res: mutable.Buffer[Member.Infix],
  ): Unit = tree match {
    case Member.ArgClause(arg :: Nil) if !isEnclosedInParens(tree) =>
      findNestedInfixes(arg, res)
    case ia: Member.Infix if !isEnclosedInParens(tree) =>
      findNestedInfixes(ia.lhs, res)
      res += ia
      ia.singleArg.foreach(findNestedInfixes(_, res))
    case _ =>
  }

  @tailrec
  final def findLeftInfix(app: Member.Infix): Member.Infix = app.lhs match {
    case t: Member.Infix if !isEnclosedInParens(t) => findLeftInfix(t)
    case _ => app
  }

  private def getInfixRhsAsInfix(app: Member.Infix): Option[Member.Infix] =
    app.singleArg match {
      case Some(t: Member.Infix) if !isEnclosedInParens(t) => Some(t)
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
      queue ++= elem.nestedInfixApps.filter(x => !isEnclosedInParens(x))
    }
    maxPrecedence
  }

  def functionExpire(function: Term.FunctionTerm): (T, ExpiresOn) = function
    .parent match {
    case Some(SingleArgInBraces.OrBlock(_, _, e)) => e.left -> ExpiresOn.Before
    case _ => getLastExceptParen(function).left -> ExpiresOn.After
  }

  def mustForceConfigStyle(ft: FormatToken)(implicit
      cfg: Newlines.ConfigStyleElement,
  ): Boolean = cfg.getForceIfOptimized && forceConfigStyle(ft.meta.idx)

  def preserveConfigStyle(
      ft: FormatToken,
      breakBeforeClose: => Boolean,
  )(implicit style: ScalafmtConfig, clauseSiteFlags: ClauseSiteFlags): Boolean =
    preferConfigStyle && couldPreserveConfigStyle(ft, breakBeforeClose)

  def couldPreserveConfigStyle(ft: FormatToken, breakBeforeClose: => Boolean)(
      implicit
      style: ScalafmtConfig,
      clauseSiteFlags: ClauseSiteFlags,
  ): Boolean = !style.newlines.sourceIgnored && {
    !dangleCloseDelim && !alignOpenDelim || ft.hasBreak ||
    (next(ft).hasBreak || style.newlines.forceAfterImplicitParamListModifier) &&
    opensConfigStyleImplicitParamList(ft)
  } && breakBeforeClose

  /** Works for `using` as well */
  def opensConfigStyleImplicitParamList(formatToken: FormatToken)(implicit
      style: ScalafmtConfig,
  ): Boolean = soft.ImplicitOrUsing(formatToken.right) &&
    style.newlines.notBeforeImplicitParamListModifier &&
    hasImplicitParamList(formatToken.meta.rightOwner)

  def isSingleIdentifierAnnotation(tok: FormatToken): Boolean = {
    val toMatch =
      if (tok.right.is[T.RightParen])
        // Hack to allow any annotations with arguments like @foo(1)
        tokens(matching(tok.right), -2)
      else tok
    toMatch match {
      case FormatToken(T.At(), _: T.Ident, _) => true
      case _ => false
    }
  }

  def typeTemplateSplits(template: Template, indentIfSecond: Int)(implicit
      fileLine: FileLine,
      ft: FormatToken,
      style: ScalafmtConfig,
  ): Seq[Split] = {
    def getPolicy(expire: T) = expire match {
      case lb: T.LeftBrace if template.self.tokens.isEmpty =>
        Policy.after(lb, "MLTMPL") {
          // Force template to be multiline.
          case d @ Decision(ftd @ FormatToken(`lb`, right, _), _)
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
      val expire = x.getExpireToken.left
      val indent =
        if (!x.isSecond) Indent.Empty
        else Indent(Num(indentIfSecond), expire, ExpiresOn.After)
      def nlSplit(cost: Int) = Split(Newline, cost)
        .withPolicy(getPolicy(expire)).withIndent(indent)
      if (!style.binPack.keepParentConstructors)
        Seq(Split(Space, 0).withIndent(indent), nlSplit(1))
      else if (ft.hasBreak) Seq(nlSplit(0))
      else {
        val slbEnd = getLastToken(x.superType)
        val exclude = insideBlock[T.LeftParen](ft, slbEnd)
        Seq(
          Split(Space, 0).withIndent(indent)
            .withSingleLine(slbEnd, exclude = exclude, noSyntaxNL = true),
          nlSplit(1),
        )
      }
    }(template).getOrElse {
      Seq(Split(Space, 0)) // shouldn't happen
    }
  }

  def ctorWithChain(ownerSet: Set[Tree], lastToken: T)(implicit
      style: ScalafmtConfig,
  ): Policy = Policy ?
    ((style.binPack.parentConstructors eq BinPack.ParentCtors.Always) ||
      ownerSet.isEmpty) || Policy.after(lastToken, prefix = "WITH") {
      case d @ Decision(t @ FormatToken(_, _: T.KwWith, _), _)
          if ownerSet.contains(t.meta.rightOwner) =>
        d.onlyNewlinesWithoutFallback
      case d @ Decision(t @ FormatToken(T.Comma(), _, _), _)
          if ownerSet.contains(t.meta.leftOwner) =>
        d.onlyNewlinesWithoutFallback
    }

  def binPackParentConstructorSplits(
      isFirstCtor: Boolean,
      owners: => Set[Tree],
      rhs: => Option[Tree],
      lastFt: FormatToken,
      indentLen: Int,
      extendsThenWith: => Boolean = false,
  )(implicit
      fileLine: FileLine,
      ft: FormatToken,
      style: ScalafmtConfig,
  ): Seq[Split] = {
    val lastToken = lastFt.left
    val nlMod = NewlineT(alt = Some(Space))
    val indent =
      if (!isFirstCtor) Indent.Empty
      else Indent(Num(indentLen), lastToken, ExpiresOn.After)
    if (style.binPack.keepParentConstructors)
      if (ft.hasBreak) Seq(Split(nlMod, 0).withIndent(indent))
      else {
        val slbEnd = endOfSingleLineBlock(rhs.fold(lastFt)(getLast))
        Seq(
          Split(Space, 0).withIndent(indent).withSingleLine(
            slbEnd,
            exclude = insideBracesBlock(ft, slbEnd, true),
            noSyntaxNL = extendsThenWith,
          ),
          Split(nlMod, 1).withIndent(indent),
        )
      }
    else if (isFirstCtor) {
      val nlPolicy = ctorWithChain(owners, lastToken)
      val nlOnelineTag = style.binPack.parentConstructors match {
        case BinPack.ParentCtors.Oneline => Right(true)
        case BinPack.ParentCtors.OnelineIfPrimaryOneline =>
          Left(SplitTag.OnelineWithChain)
        case BinPack.ParentCtors.Always | BinPack.ParentCtors.Never =>
          Right(false)
        case _ => Right(style.newlines.fold)
      }
      val exclude = style.binPack.parentConstructors match {
        case BinPack.ParentCtors.Always => insideBracesBlock(ft, lastToken, true)
        case _ => TokenRanges.empty
      }
      val noSyntaxNL = extendsThenWith
      val pnlPolicy = PenalizeAllNewlines(lastToken, 1, noSyntaxNL = noSyntaxNL)
      val slbEnd = endOfSingleLineBlock(lastFt)
      Seq(
        Split(Space, 0)
          .withSingleLine(slbEnd, exclude = exclude, noSyntaxNL = noSyntaxNL)
          .orPolicy(pnlPolicy).withIndent(indent),
        Split(nlMod, 0).onlyIf(nlOnelineTag != Right(false))
          .preActivateFor(nlOnelineTag.left.toOption)
          .withSingleLineNoOptimal(lastToken, noSyntaxNL = noSyntaxNL)
          .withIndent(indent),
        Split(nlMod, 1).withPolicy(nlPolicy & pnlPolicy).withIndent(indent),
      )
    } else Seq(Split(Space, 0), Split(Newline, 1))
  }

  def getForceConfigStyle: (Set[Int], Set[Int]) = {
    val callSite = initStyle.runner.optimizer.getCallSite
    val defnSite = initStyle.runner.optimizer.getDefnSite
    if (callSite.isEnabled || defnSite.isEnabled) {
      val clearQueues = Set.newBuilder[Int]
      val forces = Set.newBuilder[Int]
      def process(clause: Member.SyntaxValuesClause, ftOpen: FormatToken)(
          cfg: ScalafmtOptimizer.ClauseElement,
      ): Unit = if (cfg.isEnabled) matchingOpt(ftOpen.left).foreach { close =>
        val values = clause.values
        if (
          values.lengthCompare(cfg.minCount) >= 0 &&
          (cfg.minSpan == 0 || cfg.minSpan < distance(ftOpen, close))
        ) {
          forces += ftOpen.meta.idx
          values.foreach(x => clearQueues += getHead(x).meta.idx)
        }
      }
      tokens.foreach {
        case ft @ FormatToken(_: T.LeftParen | _: T.LeftBracket, _, m) =>
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
      ft: FormatToken,
  ): Seq[Split] = {
    val lpOwner = ft.meta.leftOwner

    val FormatToken(open, r, _) = ft
    val close = matching(open).left
    val indentParam = Num(style.indent.getDefnSite(lpOwner))
    val indentSep = Num((indentParam.n - 2).max(0))
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
    val lastParens = allParenOwners.map(getLastToken)
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

    val paramGroupSplitter = Policy.on(lastParen, prefix = "VML") {
      // If this is a class, then don't dangle the last paren unless the line ends with a comment
      case Decision(ftd @ FormatToken(_, `lastParen`, _), _)
          if shouldNotDangle && !isLeftCommentThenBreak(ftd) =>
        Seq(Split(NoSplit, 0))
      // Indent separators `)(` and `](` by `indentSep`
      case Decision(FormatToken(_, `close`, _), _) =>
        Seq(Split(Newline, 0).withIndent(indentSep, close, ExpiresOn.After))
      case Decision(FormatToken(LeftParenOrBracket(), _, m), ss)
          if allParenOwners.contains(m.leftOwner) =>
        ss.filter(!_.isActiveFor(SplitTag.VerticalMultilineSingleLine))
      case Decision(ftd @ FormatToken(soft.ImplicitOrUsing(), _, m), _)
          if style.newlines.forceAfterImplicitParamListModifier &&
            !isRightCommentThenBreak(ftd) &&
            hasImplicitParamList(m.leftOwner) => Seq(Split(Newline, 0))
    }

    // Our policy is a combination of OneArgLineSplit and a custom splitter
    // for parameter groups.
    val policy = oneLinePerArg | paramGroupSplitter

    val firstIndent =
      if (r.is[T.RightParen]) // An empty param group
        Indent(indentSep, close, ExpiresOn.After)
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
      else {
        val afterLastParen = before(lastParen).right
        if (afterLastParen.is[T.Colon]) afterLastParen else lastParen
      }
    val slbSplit = Split(space, 0).withSingleLine(slbEnd)
      .preActivateFor(SplitTag.VerticalMultilineSingleLine)

    if (isBracket) {
      val noSlbPolicy = Policy.on(lastParen, prefix = "VML!SLB") {
        case Decision(FormatToken(LeftParenOrBracket(), _, m), ss)
            if allParenOwners.contains(m.leftOwner) =>
          ss.filter(!_.isActiveFor(SplitTag.VerticalMultilineSingleLine))
      }
      val noSplit =
        if (allParenOwners.isEmpty) Split(space, 0).withSingleLine(close)
        else {
          val lpNext = getHead(allParenOwners.head)
          val lpNextLeft = lpNext.left
          val slbPolicy = Policy ? isRightCommentThenBreak(lpNext) ||
            decideNewlinesOnlyAfterToken(lpNextLeft)
          // If we can fit the type params, make it so
          Split(space, 0).withSingleLine(lpNextLeft).orPolicy(slbPolicy)
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
        ft.hasBreak &&
        !style.newlines.sourceIgnored && style.configStyleDefnSite.prefer ||
        implicitParams.nonEmpty &&
        style.newlines.forceAfterImplicitParamListModifier
      val nlNoAlt = implicitNL ||
        !rightIsImplicit && style.verticalMultiline.newlineAfterOpenParen
      val nlMod = NewlineT(alt = if (nlNoAlt) None else Some(slbSplit.modExt))
      val spaceImplicit = !implicitNL && implicitParams.lengthCompare(1) > 0 &&
        style.newlines.notBeforeImplicitParamListModifier
      Seq(
        // If we can fit all in one block, make it so
        slbSplit.notIf(noSlb),
        Split(space, 0, policy = policy).onlyIf(spaceImplicit).andPolicy(
          decideNewlinesOnlyAfterClose(r),
          isRightCommentThenBreak(next(ft)),
        ).withIndent(firstIndent),
        // Otherwise split vertically
        Split(nlMod, 1, policy = policy).withIndent(firstIndent),
      )
    }

  }

  // Returns leading comment, if there exists one, otherwise formatToken
  @inline
  final def leadingComment(tree: Tree): FormatToken =
    leadingComment(tokenJustBefore(tree))
  @tailrec
  final def leadingComment(ft: FormatToken): FormatToken =
    if (ft.hasBlankLine || !ft.left.is[T.Comment]) ft
    else {
      val pft = prevNonCommentSameLineBefore(ft)
      if (pft.noBreak) ft else leadingComment(pft)
    }

  // Returns trailing comment, if there exists one, otherwise formatToken
  @inline
  final def trailingComment(ft: FormatToken, end: Int): FormatToken = {
    @inline
    def isDone(x: FormatToken) = x.hasBlankLine || x.right.end >= end
    @tailrec
    def iter(x: FormatToken): FormatToken = {
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
      ft: FormatToken,
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
      ft: FormatToken,
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
  def getFuncArrow(term: Term.FunctionTerm): Option[FormatToken] = tokens
    .tokenBeforeOpt(term.body)
    .orElse(tokenAfterOpt(term.paramClause).map(getArrowAfter))
    .orElse(findFirst(getHead(term), term.pos.end)(_.left.is[T.RightArrow]))

  // look for arrow before body, if any, else after cond/pat
  def getCaseArrow(term: Case): FormatToken = tokenBeforeOpt(term.body)
    .getOrElse(getArrowAfter(tokenAfter(term.cond.getOrElse(term.pat))))

  // look for arrow before body, if any, else after cond/pat
  def getCaseArrow(term: TypeCase): FormatToken = next(tokenAfter(term.pat))

  private def getArrowAfter(ft: FormatToken): FormatToken = {
    val maybeArrow = next(ft)
    if (maybeArrow.left.is[T.RightArrow]) maybeArrow
    else nextAfterNonComment(maybeArrow)
  }

  @tailrec
  final def findPrevSelectAndApply(
      tree: Tree,
      enclosed: Boolean,
      applyTree: Option[Member.Apply] = None,
  ): (Option[SelectLike], Option[Member.Apply]) = {
    @inline
    def isEnclosed: Boolean = enclosed && isEnclosedInParens(tree)
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
  private def findLastApplyAndNextSelectEnclosed(
      tree: Tree,
      select: Option[SelectLike] = None,
      prevApply: Option[Tree] = None,
  ): (Tree, Option[SelectLike]) =
    if (isEnclosedInParens(tree)) (prevApply.getOrElse(tree), select)
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
            .filter(isEnclosedInParens)
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
    def checkParent = thisTree.parent match {
      case `nextSelect` => style.includeNoParensInSelectChains
      case Some(p: Term.Apply) if getHeadToken(p.argClause).is[T.LeftBrace] =>
        style.includeCurlyBraceInSelectChains && !nextSelect.contains(lastApply) // exclude short curly
      case Some(p: Member.Apply) => p.fun eq thisTree
      case _ => false
    }
    ok &&
    (thisTree match {
      case _: Term.Match => // like select and apply in one
        !tokenAfter(thisSelectLike.nameToken).right.is[T.LeftBrace] ||
        style.includeCurlyBraceInSelectChains &&
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
      val prevPrevSelect = findPrevSelect(p, style.encloseSelectChains)
      inSelectChain(prevPrevSelect, p, lastApply)
  }

  @tailrec
  final def findXmlLastLineIndent(ft: FormatToken): Int = ft.left match {
    case _: T.Xml.Start => 0
    case t: T.Xml.Part => TokenOps.getXmlLastLineIndent(t) match {
        case Some(x) => x
        case None => findXmlLastLineIndent(prev(ft))
      }
    case t: T.Xml.SpliceEnd => findXmlLastLineIndent(prev(matching(t)))
    case _ => findXmlLastLineIndent(prev(ft))
  }

  def withIndentOnXmlStart(tok: T.Xml.Start, splits: Seq[Split])(implicit
      style: ScalafmtConfig,
  ): Seq[Split] =
    if (style.xmlLiterals.assumeFormatted) {
      val end = matching(tok)
      val indent = Num(findXmlLastLineIndent(prev(end)), true)
      splits.map(_.withIndent(indent, end.left, ExpiresOn.After))
    } else splits

  def withIndentOnXmlSpliceStart(ft: FormatToken, splits: Seq[Split])(implicit
      style: ScalafmtConfig,
  ): Seq[Split] = ft.left match {
    case t: T.Xml.SpliceStart if style.xmlLiterals.assumeFormatted =>
      val end = matching(t)
      val indent = Num(findXmlLastLineIndent(prev(ft)), true)
      splits.map(_.withIndent(indent, end.left, ExpiresOn.After))
    case _ => splits
  }

  object CtrlBodySplits {

    private object CallSite {

      private val penalizeOpenNL: Policy.Pf = { case Decision(_, s) =>
        s.map(x => if (x.isNL) x.withPenalty(1) else x)
      }

      @tailrec
      private def getNestedOpens(
          tree: Member.ArgClause,
          res: List[FormatToken],
      ): List[FormatToken] = {
        val args = tree.values
        if (args.isEmpty) res
        else {
          val newres = getHead(tree) :: res
          args match {
            case (t: Member.Apply) :: Nil => getNestedOpens(t.argClause, newres)
            case _ => newres
          }
        }
      }

      private def getNestedOpensPolicy(
          opens: List[FormatToken],
          policy: Policy,
      ): Policy = opens.foldLeft(policy) { case (res, x) =>
        val endPos = nextNonComment(x).right match {
          case t: T.LeftBrace => Policy.End > t
          case t => Policy.End == t
        }
        val onOpen = Policy(endPos, "NSTOPEN")(penalizeOpenNL)
        Policy.End == x.left ==> onOpen ==> res
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
          val opens = t.argClauses.foldLeft(List.empty[FormatToken]) {
            case (res, x) => getNestedOpens(x, res)
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
    )(implicit style: ScalafmtConfig, ft: FormatToken): Seq[Split] = {
      val btokens = body.tokens
      val blastFT = getLastNonTrivial(btokens, body)
      val blast = blastFT.left
      val expire = nextNonCommentSameLine(blastFT).left
      def penalize(penalty: Int) = Policy ? (penalty > 0) &&
        new PolicyOps.PenalizeAllNewlines(Policy.End == blast, penalty)
      def getNlSplit(penalty: Int, nlCost: Int = 1)(implicit
          fileLine: FileLine,
      ): Split = nlSplitFunc(nlCost).andPolicy(penalize(penalty))
        .forThisLine(nextLine)
      def getSplits(spaceSplit: Split, nlCost: Int = 1) = (
        spaceSplit.withIndents(spaceIndents),
        getNlSplit(1, nlCost)(spaceSplit.fileLine),
      )
      def getSlb(end: T, excl: TokenRanges)(implicit fileLine: FileLine) =
        SingleLineBlock(end, exclude = excl, noSyntaxNL = true)
      def getSlbSplit(
          end: T,
          exclude: TokenRanges = TokenRanges.empty,
          policy: Policy = Policy.NoPolicy,
      )(implicit fileLine: FileLine) = Split(Space, 0)
        .withPolicy(policy | getSlb(end, exclude)).withOptimalToken(
          end,
          killOnFail = exclude.isEmpty,
          ignore = blast.start > end.start,
        )
      def getSpaceSplit(penalty: Int, policy: Policy = Policy.NoPolicy)(implicit
          fileLine: FileLine,
      ) = {
        val spacePolicy = policy | penalize(penalty)
        val miniSlbEnd = getSlbEndOnLeft(next(ft)).left
        val slbLite = style.newlines.keep &&
          (body.parent match {
            case Some(p: Term.Assign) => !p.parent.is[Term.ArgClause] ||
              style.binPack.callSite == BinPack.Site.Never
            case _ => true
          })
        val opt = if (style.newlines.keep) miniSlbEnd else blast
        Split(Space, 0).withSingleLineNoOptimal(miniSlbEnd, noSyntaxNL = true)
          .andPolicy(spacePolicy)
          .withOptimalToken(opt, killOnFail = slbLite, recurseOnly = slbLite)
      }
      def getPolicySplits(penalty: Int, policy: Policy, nlCost: Int = 1)(
          implicit fileLine: FileLine,
      ) = getSplits(getSpaceSplit(penalty, policy), nlCost)
      def getSlbSplits(
          end: T = expire,
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
          getSplits(getSlbSplit(end.left))
        case _: Term.If => getSlbSplits()
        case _: Term.TryClause =>
          if (hasStateColumn) getSplits(getSpaceSplit(1)) else getSlbSplits()
        case _: Term.Block | _: Term.Match | _: Type.Match |
            _: Term.NewAnonymous => getSplits(getSpaceSplit(1))
        case t: Term.ForYield => getDelimsIfEnclosed(t.enumsBlock) match {
            case Some((forEnumHead, forEnumLast)) =>
              val exclude =
                TokenRanges(TokenRange(forEnumHead.left, forEnumLast.left))
              val afterYield = (t.body match {
                case b: Term.Block => getHeadAndLastIfEnclosed(b)
                    .map { case (forBodyHead, forBodyLastOpt) =>
                      if (forBodyLastOpt.isDefined) forBodyHead
                      else prevNonCommentBefore(forBodyHead)
                    }
                case b => tokenBeforeOpt(b)
              }).getOrElse(getLast(t))
              val end = endOfSingleLineBlock(afterYield)
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
            else getSplits(getSlbSplit(getLastToken(lia.lhs)))
          else getSplits(getSlbSplit(getLastToken(lia.op)))
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
    )(implicit style: ScalafmtConfig, ft: FormatToken): Seq[Split] =
      if (body.tokens.isEmpty) Seq(Split(Space, 0))
      else foldedNonEmptyNonComment(body, nlSplitFunc, isKeep, spaceIndents)

    private def unfoldedSpaceNonEmptyNonComment(body: Tree, slbOnly: Boolean)(
        implicit style: ScalafmtConfig,
    ): Split = {
      val expire = nextNonCommentSameLine(getLastNonTrivial(body)).left
      def slbSplit(end: T)(implicit fileLine: FileLine) = Split(Space, 0)
        .withSingleLine(end, noSyntaxNL = true)
      body match {
        // we force newlines in for/yield
        case _: Term.ForYield => Split.ignored
        // we force newlines in try/catch/finally
        case _: Term.TryClause => Split.ignored
        // don't tuck curried apply
        case t: Term.Apply if t.fun.is[Term.Apply] => slbSplit(expire)
        case EndOfFirstCall(end) if !slbOnly => slbSplit(end)
        case _ => slbSplit(expire)
      }
    }

    private def unfoldedNonComment(
        body: Tree,
        nlSplitFunc: Int => Split,
        spaceIndents: Seq[Indent],
        slbOnly: Boolean,
    )(implicit style: ScalafmtConfig): Seq[Split] =
      if (body.tokens.isEmpty) Seq(Split(Space, 0).withIndents(spaceIndents))
      else {
        val spaceSplit = unfoldedSpaceNonEmptyNonComment(body, slbOnly)
        Seq(spaceSplit.withIndents(spaceIndents), nlSplitFunc(1).forThisLine)
      }

    def checkComment(nlSplitFunc: Int => Split)(
        splitsFunc: FormatToken => Seq[Split],
    )(implicit ft: FormatToken): Seq[Split] =
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
        val policy = Policy
          .on(nextFt.right, "CBCMT") { case Decision(`nextFt`, _) => splits }
        Seq(Split(Space, 0, policy = policy))
      }

    def folded(
        body: Tree,
        isKeep: Boolean,
        spaceIndents: Seq[Indent] = Seq.empty,
    )(nlSplitFunc: Int => Split)(implicit
        style: ScalafmtConfig,
        ft: FormatToken,
    ): Seq[Split] = checkComment(nlSplitFunc) { _ =>
      foldedNonComment(body, nlSplitFunc, isKeep, spaceIndents)
    }

    def slbOnly(body: Tree, spaceIndents: Seq[Indent] = Seq.empty)(
        nlSplitFunc: Int => Split,
    )(implicit style: ScalafmtConfig, ft: FormatToken): Seq[Split] =
      checkComment(nlSplitFunc) { _ =>
        unfoldedNonComment(body, nlSplitFunc, spaceIndents, true)
      }

    def get(body: Tree, spaceIndents: Seq[Indent] = Seq.empty)(
        classicNoBreakFunc: => Split,
    )(nlSplitFunc: Int => Split)(implicit
        style: ScalafmtConfig,
        ft: FormatToken,
    ): Seq[Split] = checkComment(nlSplitFunc) { x =>
      def getFolded(isKeep: Boolean) =
        foldedNonComment(body, nlSplitFunc, isKeep = isKeep, spaceIndents)
      def getFoldedKeepNLOnly = getFolded(true).filter(_.isNL)
      style.newlines.getBeforeMultiline match {
        case Newlines.fold => getFolded(false)
        case Newlines.unfold =>
          unfoldedNonComment(body, nlSplitFunc, spaceIndents, false)
        case Newlines.classic if x.noBreak =>
          Option(classicNoBreakFunc).fold(getFolded(true)) { func =>
            func.forThisLine +: getFoldedKeepNLOnly
          }
        case Newlines.keep if x.noBreak => getFolded(true)
        case _ => getFoldedKeepNLOnly // keep/classic with break
      }
    }

    def getWithIndent(
        body: Tree,
        endFt: => FormatToken,
        spaceIndents: Seq[Indent] = Seq.empty,
    )(classicNoBreakFunc: => Split)(nlSplitFunc: Int => Split)(implicit
        style: ScalafmtConfig,
        ft: FormatToken,
    ): Seq[Split] = get(body, spaceIndents)(classicNoBreakFunc)(x =>
      withIndent(nlSplitFunc(x), body, endFt),
    )

    def withIndent(nlSplit: Split, endFt: FormatToken)(implicit
        ft: FormatToken,
        style: ScalafmtConfig,
    ): Split = withNLPolicy(endFt) {
      val right = nextNonComment(ft).right
      val rpOpt = if (right.is[T.LeftParen]) matchingOpt(right) else None
      val expire = nextNonCommentSameLine(rpOpt.fold(endFt) { rp =>
        if (rp.left.end >= endFt.left.end) rp else endFt
      })
      nlSplit.withIndent(Num(style.indent.main), expire.left, ExpiresOn.After)
    }

    def withIndent(nlSplit: Split, body: Tree, endFt: => FormatToken)(implicit
        ft: FormatToken,
        style: ScalafmtConfig,
    ): Split = asInfixApp(body)
      .fold(withIndent(nlSplit, endFt))(InfixSplits.withNLIndent(nlSplit))

    @tailrec
    def getBlockStat(t: Tree): Tree = t match {
      case b: Term.Block => getSingleStatExceptEndMarker(b.stats) match {
          case Some(s) if !isEnclosedInMatching(b) => getBlockStat(s)
          case _ => t
        }
      case _ => t
    }

  }

  def withNLPolicy(endFt: FormatToken)(nlSplit: Split): Split = nlSplit
    .andPolicy(nextNonCommentSameLine(endFt) match {
      case FormatToken(_, x: T.Keyword, _) => decideNewlinesOnlyBeforeToken(x)
      case ft @ FormatToken(_, x: T.Semicolon, _) =>
        val semiFt = nextNonCommentSameLineAfter(ft)
        val semi = semiFt.left
        if (semiFt.noBreak || (semi eq x) && !semiFt.right.is[T.Comment])
          decideNewlinesOnlyAfterToken(semi)
        else NoPolicy
      case _ => NoPolicy
    })

  // Redundant () delims around case statements
  def getClosingIfCaseBodyEnclosedAsBlock(
      postArrowFt: FormatToken,
      caseStat: CaseTree,
  )(implicit beforeMultiline: Newlines.SourceHints): Option[FormatToken] = {
    val body = caseStat.body
    val ok = body.eq(postArrowFt.meta.rightOwner) &&
      (beforeMultiline.ignoreSourceSplit || postArrowFt.noBreak)
    if (ok) getClosingIfBodyEnclosedAsBlock(body) else None
  }

  // Redundant () delims around body
  def getClosingIfBodyEnclosedAsBlock(body: Tree): Option[FormatToken] =
    body match {
      case _: Lit.Unit | _: Term.Tuple => None
      case t: Term.ApplyInfix if {
            val op = t.op.value
            op == "->" || op == "→"
          } => None
      case _ => getClosingIfInParens(body)
    }

  def isBodyEnclosedAsBlock(body: Tree): Boolean =
    getClosingIfBodyEnclosedAsBlock(body).isDefined

  object GetSelectLike {
    val OnRight =
      new ExtractFromMeta(m => onRightOpt(m.rightOwner, tokens(m.idx)))

    private[FormatOps] def onRightOpt(
        ro: Tree,
        ftOrNull: => FormatToken,
    ): Option[SelectLike] = ro match {
      case x: Term.Select => Some(SelectLike(x))
      case x: Term.Match if dialect.allowMatchAsOperator =>
        val ft = Option(ftOrNull).getOrElse(tokenAfter(x.expr))
        if (!ft.right.is[T.Dot]) None
        else nextNonCommentAfter(ft) match {
          case xft @ FormatToken(_, _: T.KwMatch, _) =>
            Some(SelectLike(x, next(xft)))
          case _ => None
        }
      case _ => None
    }

    def onRightOpt(ft: FormatToken): Option[SelectLike] =
      onRightOpt(ft.meta.rightOwner, ft)

    def unapply(tree: Tree): Option[SelectLike] = onRightOpt(tree, null)
  }

  def getSplitsForTypeBounds(
      noNLMod: => Modification,
      tparam: Type.Param,
      bounds: Type.Param => Seq[Type],
  )(implicit style: ScalafmtConfig, ft: FormatToken): Seq[Split] = {
    val boundOpt = bounds(tparam).find(_.pos.start > ft.right.end)
    val expireOpt = boundOpt.map(getLastNonTrivialToken)
    getSplitsForTypeBounds(noNLMod, tparam, expireOpt)
  }

  def getSplitsForTypeBounds(
      noNLMod: => Modification,
      typeOwner: Tree,
      boundEndOpt: Option[T],
  )(implicit style: ScalafmtConfig, ft: FormatToken): Seq[Split] = {
    val typeEnd = getLastNonTrivialToken(typeOwner)
    val boundEnd = boundEndOpt.getOrElse(typeEnd)
    def indent = Indent(Num(style.indent.main), boundEnd, ExpiresOn.After)
    def unfoldPolicy = typeOwner match {
      case tparam: Type.Param => Policy.on(typeEnd, prefix = "VB") {
          case Decision(t @ FormatToken(_, _: T.Colon | _: T.Viewbound, _), s)
              if t.meta.rightOwner eq tparam => Decision.onlyNewlineSplits(s)
        }
      case _ => NoPolicy
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
      def create(nft: FormatToken)(implicit
          style: ScalafmtConfig,
          ft: FormatToken,
      ): Option[OptionalBracesRegion]
    }

    // Optional braces in templates after `:|with`
    // Optional braces after any token that can start indentation:
    // )  =  =>  ?=>  <-  catch  do  else  finally  for
    // if  match  return  then  throw  try  while  yield
    def unapply(ftMeta: FormatToken.Meta)(implicit
        style: ScalafmtConfig,
    ): Option[Seq[Split]] = get(tokens(ftMeta.idx)).flatMap(_.splits)

    def get(
        ft: FormatToken,
    )(implicit style: ScalafmtConfig): Option[OptionalBracesRegion] =
      if (!style.dialect.allowSignificantIndentation) None
      else Option(ft.left match {
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
        case _: T.KwReturn | _: T.ContextArrow | _: T.LeftArrow | _: T.KwThrow |
            _: T.KwYield => BlockImpl
        case _ => null
      }).flatMap { impl =>
        implicit val ift: FormatToken = ft
        val nft = nextNonComment(ft)
        impl.create(nft).filter { ob =>
          !nft.right.is[T.LeftBrace] || nft.meta.rightOwner.parent != ob.owner
        }
      }

    def at(ft: FormatToken)(implicit style: ScalafmtConfig): Boolean = get(ft)
      .nonEmpty

    private def getSplits(
        tree: Tree,
        forceNL: Boolean,
        danglingKeyword: Boolean = true,
        indentOpt: Option[Int] = None,
        forceNLIfTrailingStandaloneComments: Boolean = true,
    )(implicit
        fileLine: FileLine,
        style: ScalafmtConfig,
        ft: FormatToken,
    ): Seq[Split] = {
      val treeTokens = tree.tokens
      val end = getOnOrAfterLast(treeTokens, tree)
      val nonTrivialEnd = prevNonComment(end)
      val slbExpire = nextNonCommentSameLine(nonTrivialEnd)
      def head = getHead(treeTokens, tree)
      val closeFT = (tree match {
        case _: Member.Tuple => None
        case Term.Block((_: Member.Tuple) :: Nil)
            if !head.left.is[T.LeftBrace] => None
        case _ => tokens.getClosingIfInParens(nonTrivialEnd)(head)
            .map(prevNonCommentSameLine)
      }).getOrElse(nextNonCommentSameLine(end))
      val close = closeFT.left
      def nlPolicy(implicit fileLine: FileLine) = Policy ? danglingKeyword && {
        val couldBeTucked = closeFT.right.is[T.CloseDelim] &&
          closeFT.meta.rightOwner.is[Member.SyntaxValuesClause]
        if (!couldBeTucked) decideNewlinesOnlyAfterClose(close)
        else decideNewlinesOnlyAfterToken(rank = 1, ifAny = true)(close)
      }
      val indentLen = indentOpt.getOrElse(style.indent.getSignificant)
      val indent = Indent(Num(indentLen), close, ExpiresOn.After)
      def nlOnly = forceNLIfTrailingStandaloneComments &&
        slbExpire.right.is[T.Comment] && slbExpire.right.start <= close.start
      if (ft.hasBlankLine)
        Seq(Split(Newline2x, 0).withIndent(indent).withPolicy(nlPolicy))
      else if (forceNL || nlOnly)
        Seq(Split(Newline, 0).withIndent(indent).withPolicy(nlPolicy))
      else Seq(
        Split(Space, 0).withSingleLine(slbExpire.left).withIndent(indent),
        Split(Newline, 1).withIndent(indent).withPolicy(nlPolicy),
      )
    }

    // https://dotty.epfl.ch/docs/reference/other-new-features/indentation.html#variant-indentation-marker-
    // TODO: amend for additional cases when the parser supports them
    private object ColonEolImpl extends Factory {
      def create(nft: FormatToken)(implicit
          style: ScalafmtConfig,
          ft: FormatToken,
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
          ft: FormatToken,
      ): Option[OptionalBracesRegion] = {
        def funcSplit(arg: Term.FunctionTerm)(implicit fl: FileLine) = {
          val end = getLast(arg)
          val opt = getOptimalTokenFor(getFuncArrow(arg).getOrElse(end))
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
        Some(new OptionalBracesRegion {
          def owner = ac.parent
          def splits = Some(args match {
            case (tf: Term.FunctionTerm) :: Nil
                if !style.newlines.alwaysBeforeCurlyLambdaParams &&
                  // https://dotty.epfl.ch/docs/internals/syntax.html
                  (tf.paramClause match { // LambdaStart
                    case tpc @ Term.ParamClause(tp :: Nil, mod) =>
                      (mod.isEmpty && tp.mods.isEmpty && tp.decltpe.isEmpty) ||
                      isEnclosedInParens(tpc)
                    case _ => true // multiple params are always in parens
                  }) =>
              getSplits(ac, forceNL = false, indentOpt = indent) match {
                case s +: rs if !s.isNL => funcSplit(tf)(s.fileLine) +: rs
                case ss => ss
              }
            case _ => getSplits(ac, forceNL = true, indentOpt = indent)
          })
          def rightBrace = treeLast(ac)
        })
      }
    }

    private object WithImpl extends Factory {
      def create(nft: FormatToken)(implicit
          style: ScalafmtConfig,
          ft: FormatToken,
      ): Option[OptionalBracesRegion] = {
        val lo = ft.meta.leftOwner
        def createImpl(
            tb: Tree.Block,
            ownerOpt: => Option[Tree],
            okRightBrace: => Boolean,
        ) =
          if (
            (nft.right.is[T.LeftBrace] && (nft.meta.rightOwner eq tb)) ||
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
      def create(nft: FormatToken)(implicit
          style: ScalafmtConfig,
          ft: FormatToken,
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
      def create(nft: FormatToken)(implicit
          style: ScalafmtConfig,
          ft: FormatToken,
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
                !isTreeSingleExpr(t.thenp) ||
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
      def create(nft: FormatToken)(implicit
          style: ScalafmtConfig,
          ft: FormatToken,
      ): Option[OptionalBracesRegion] = ft.meta.leftOwner match {
        case t: Case => // unsupported except for right brace, or when ends in comment
          Some(new OptionalBracesRegion {
            def owner = None
            def splits =
              if (getLastNotTrailingCommentOpt(t).forall(_.isRight)) None
              else Some(Seq(Split(Newline2x(ft), 0)))
            def rightBrace = blockLast(t.body)
          })
        case _ => BlockImpl.create(nft)
      }
    }

    private object ForImpl extends Factory {
      def create(nft: FormatToken)(implicit
          style: ScalafmtConfig,
          ft: FormatToken,
      ): Option[OptionalBracesRegion] = ft.meta.leftOwner match {
        case t @ Tree.WithEnums(x) if isSeqMulti(x) =>
          WithStats(nft, x.headOption, x.last, Some(t), nlOnly = false)
        case _ => None
      }
    }

    private object WhileImpl extends Factory {
      def create(nft: FormatToken)(implicit
          style: ScalafmtConfig,
          ft: FormatToken,
      ): Option[OptionalBracesRegion] = ft.meta.leftOwner match {
        case t: Term.While => t.expr match {
            case b: Term.Block
                if isMultiStatBlock(b) &&
                  !matchingOpt(nft.right).exists(_.left.end >= b.pos.end) =>
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
      def create(nft: FormatToken)(implicit
          style: ScalafmtConfig,
          ft: FormatToken,
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
      def create(nft: FormatToken)(implicit
          style: ScalafmtConfig,
          ft: FormatToken,
      ): Option[OptionalBracesRegion] = ft.meta.leftOwner match {
        case t: Ctor.Secondary =>
          if (t.body.stats.isEmpty) None
          else WithStats(nft, Some(t.body.init), t.body, t.parent)
        case t @ Tree.WithBody(b) => (b match {
            case x: Term.Block => getBlockWithNonSingleTermStat(x)
            case x: Term.PartialFunction => Some(x)
            case _ => None
          }).flatMap(x => WithStats(nft, x.exprs.headOption, b, Some(t)))
        case _ => BlockImpl.create(nft)
      }
    }

    private object TryImpl extends Factory {
      def create(nft: FormatToken)(implicit
          style: ScalafmtConfig,
          ft: FormatToken,
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
      def create(nft: FormatToken)(implicit
          style: ScalafmtConfig,
          ft: FormatToken,
      ): Option[OptionalBracesRegion] = ft.meta.leftOwner match {
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
      def create(nft: FormatToken)(implicit
          style: ScalafmtConfig,
          ft: FormatToken,
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
          case t: Term.Try => t.finallyp.map(createImpl {
              isCatchUsingOptionalBraces(t) || isTreeUsingOptionalBraces(t.expr)
            })
          case t: Term.TryWithHandler => t.finallyp
              .map(createImpl(isTreeUsingOptionalBraces(t.expr)))
          case _ => None
        }
      }
    }

    private object MatchImpl extends Factory {
      def create(nft: FormatToken)(implicit
          style: ScalafmtConfig,
          ft: FormatToken,
      ): Option[OptionalBracesRegion] = ft.meta.leftOwner match {
        case t @ Tree.WithCasesBlock(x) =>
          val ind = style.indent.matchSite
          WithStats(nft, x.cases.headOption, t, Some(t), indentOpt = ind)
        case _ => None
      }
    }

    private object ThenImpl extends Factory {
      def create(nft: FormatToken)(implicit
          style: ScalafmtConfig,
          ft: FormatToken,
      ): Option[OptionalBracesRegion] = ft.meta.leftOwner match {
        case t: Term.If => Some(new OptionalBracesRegion {
            def owner = Some(t)
            def splits = Some(getSplitsForIf(nft, t))
            def rightBrace = blockLast(t.thenp)
          })
        case _ => None
      }
    }

    private object IfImpl extends Factory {
      def create(nft: FormatToken)(implicit
          style: ScalafmtConfig,
          ft: FormatToken,
      ): Option[OptionalBracesRegion] = ft.meta.leftOwner match {
        case t: Term.If =>
          val nr = nft.right
          t.cond match {
            case b: Term.Block if (matchingOpt(nr) match {
                  case Some(t) => t.left.end < b.pos.end
                  case None => isMultiStatBlock(b)
                }) =>
              Some(new OptionalBracesRegion {
                def owner = Some(t)
                def splits = Some {
                  val dangle = style.danglingParentheses.ctrlSite
                  val forceNL = !nr.is[T.LeftParen]
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
      def create(nft: FormatToken)(implicit
          style: ScalafmtConfig,
          ft: FormatToken,
      ): Option[OptionalBracesRegion] = ft.meta.leftOwner match {
        case t: Term.If => (getTreeSingleExpr(t.elsep) match {
            case Some(x: Term.If) =>
              val forceNL = isJustBeforeTree(nft)(x) && ft.hasBreak &&
                ((ft ne nft) || style.newlines.keep)
              if (forceNL) Some(true) else None
            case Some(_) if !getLastNotTrailingCommentOpt(t).exists(_.isLeft) =>
              if (!isThenPWithOptionalBraces(t)) None
              else Some(shouldBreakInOptionalBraces(ft, nft))
            case _ => Some(true)
          }).map { forceNL =>
            new OptionalBracesRegion {
              def owner = Some(t)
              def splits = Some(getSplits(t.elsep, forceNL))
              def rightBrace = blockLast(t.elsep)
            }
          }
        case _ => None
      }
    }

    private def getSplitsMaybeBlock(
        nft: FormatToken,
        tree: Tree,
        danglingKeyword: Boolean = true,
    )(implicit
        fileLine: FileLine,
        style: ScalafmtConfig,
        ft: FormatToken,
    ): Seq[Split] = {
      val forceNL = !hasSingleTermStatIfBlock(tree) ||
        shouldBreakInOptionalBraces(ft, nft)
      getSplits(tree, forceNL, danglingKeyword)
    }

    private class WithStats private (
        nft: FormatToken,
        body: Tree,
        val owner: Option[Tree],
        nlOnly: Boolean,
        indentOpt: Option[Int],
    )(implicit fileLine: FileLine, style: ScalafmtConfig, ft: FormatToken)
        extends OptionalBracesRegion {
      def splits: Option[Seq[Split]] = {
        val forceNL = nlOnly || shouldBreakInOptionalBraces(ft, nft)
        Some(getSplits(body, forceNL = forceNL, indentOpt = indentOpt))
      }
      def rightBrace = treeLast(body)
    }

    private object WithStats {
      def apply(
          nft: FormatToken,
          head: Option[Tree],
          body: => Tree,
          owner: => Option[Tree],
          nlOnly: Boolean = true,
          indentOpt: Option[Int] = None,
      )(implicit
          fileLine: FileLine,
          style: ScalafmtConfig,
          ft: FormatToken,
      ): Option[WithStats] = head.flatMap { head =>
        if (!isJustBeforeTree(nft)(head)) None
        else Some(new WithStats(nft, body, owner, nlOnly, indentOpt))
      }
    }

    private def getSplitsForIf(nft: FormatToken, t: Term.If)(implicit
        fileLine: FileLine,
        style: ScalafmtConfig,
        ft: FormatToken,
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
          (!before.right.is[T.LeftBrace] || matchingOpt(before.right)
            .exists(_.left.end < thenp.pos.end))
      }
    }

    @tailrec
    private def isElsePWithOptionalBraces(tree: Term.If): Boolean = {
      val elsep = tree.elsep
      !getHeadToken(elsep).is[T.LeftBrace] &&
      (elsep match {
        case t: Term.If => isThenPWithOptionalBraces(t) ||
          !ifWithoutElse(t) && isElsePWithOptionalBraces(t)
        case Term.Block((t: Term.If) :: Nil) => isThenPWithOptionalBraces(t) ||
          !ifWithoutElse(t) && isElsePWithOptionalBraces(t)
        case t => !isTreeSingleExpr(t)
      })
    }

    private def shouldBreakInOptionalBraces(ft: FormatToken, nft: FormatToken)(
        implicit style: ScalafmtConfig,
    ): Boolean = style.newlines.source match {
      case Newlines.unfold => true
      case Newlines.fold => false
      case Newlines.keep => ft.hasBreak
      case _ => (ft ne nft) && ft.hasBreak
    }

    private def isTreeUsingOptionalBraces(tree: Tree): Boolean =
      !isTreeSingleExpr(tree) && !tokenBefore(tree).left.is[T.LeftBrace]

    @inline
    private def treeLast(tree: Tree): Option[FormatToken] = getLastOpt(tree)
    @inline
    private def blockLast(tree: Tree): Option[FormatToken] =
      if (isTreeMultiStatBlock(tree)) treeLast(tree) else None
    @inline
    private def blockLast(tree: Term.Block): Option[FormatToken] =
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
      val kw = getClosingIfInParens(end)(beg).fold(end)(next).right
      if (!kw.is[A]) return None

      val indent = style.indent.ctrlSite.getOrElse(style.indent.getSignificant)
      def policy =
        if (split.isNL) decideNewlinesOnlyBeforeClose(kw)
        else decideNewlinesOnlyBeforeCloseOnBreak(kw)
      split.withIndent(Num(indent), kw, ExpiresOn.Before)
        .andPolicy(policy, !style.danglingParentheses.ctrlSite)
    }

  }

  object MissingBraces {

    type Ranges = Seq[(Tree, Tree)]
    type Result = Option[(Tree, Ranges)]

    private trait Factory {
      def getBlocks(ft: FormatToken, nft: FormatToken, all: Boolean): Result
    }

    def getBlocks(ft: FormatToken, all: Boolean): Result = {
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
      def getBlocks(ft: FormatToken, nft: FormatToken, all: Boolean): Result = {
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
      def getBlocks(ft: FormatToken, nft: FormatToken, all: Boolean): Result =
        ft.meta.leftOwner match {
          case t: Term.FunctionTerm =>
            val skip = t.parent.exists(TreeOps.isExprWithParentInBraces(t))
            if (skip) None else Some((t.body, seq(all, t.paramClause.values)))
          case _ => None
        }
    }

    private object RightParenImpl extends Factory {
      def getBlocks(ft: FormatToken, nft: FormatToken, all: Boolean): Result =
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
      def getBlocks(ft: FormatToken, nft: FormatToken, all: Boolean): Result =
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
      def getBlocks(ft: FormatToken, nft: FormatToken, all: Boolean): Result =
        ft.meta.leftOwner match {
          case t: Term.Do => Some((t.body, seq(all, t.expr)))
          case _ => None
        }
    }

    private object EqualsImpl extends Factory {
      def getBlocks(ft: FormatToken, nft: FormatToken, all: Boolean): Result =
        ft.meta.leftOwner match {
          case t: Ctor.Secondary => Some((t, seq(all, t.body)))
          case t: Tree.WithBody => Some((t.body, Nil))
          case _ => BlockImpl.getBlocks(ft, nft, all)
        }
    }

    private object TryImpl extends Factory {
      def getBlocks(ft: FormatToken, nft: FormatToken, all: Boolean): Result =
        ft.meta.leftOwner match {
          case t: Term.TryClause =>
            Some((t.expr, seq(all, t.catchClause) ++ seq(all, t.finallyp)))
          case _ => None
        }
    }

    private object CatchImpl extends Factory {
      def getBlocks(ft: FormatToken, nft: FormatToken, all: Boolean): Result =
        ft.meta.leftOwner match {
          case t: Term.TryClause => t.catchClause.map { x =>
              (x, seq(all, t.expr) ++ seq(all, t.finallyp))
            }
          case _ => None
        }
    }

    private object FinallyImpl extends Factory {
      def getBlocks(ft: FormatToken, nft: FormatToken, all: Boolean): Result =
        ft.meta.leftOwner match {
          case t: Term.TryClause => t.finallyp
              .map(x => (x, seq(all, t.expr) ++ seq(all, t.catchClause)))
          case _ => None
        }
    }

    private object ElseImpl extends Factory {
      def getBlocks(ft: FormatToken, nft: FormatToken, all: Boolean): Result =
        ft.meta.leftOwner match {
          case x: Term.If if !x.elsep.is[Term.If] =>
            Some((x.elsep, seq(all, x.thenp) ++ seq(all, x.cond)))
          case _ => None
        }
    }

    private object YieldImpl extends Factory {
      def getBlocks(ft: FormatToken, nft: FormatToken, all: Boolean): Result =
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

  def getHeadToken(tree: Tree): T = getHead(tree).left

  def getLastToken(tree: Tree): T = getLast(tree).left

  def getLastTokenOpt(tree: Tree): Option[T] = getLastOpt(tree).map(_.left)

  def getLastNonTrivialToken(tree: Tree): T = getLastNonTrivial(tree).left

  def getLastNonTrivialTokenOpt(tree: Tree): Option[T] = tokens
    .getLastNonTrivialOpt(tree).map(_.left)

  def getEndOfBlock(ft: FormatToken, parensToo: => Boolean)(implicit
      style: ScalafmtConfig,
  ): Option[FormatToken] = ft.left match {
    case x: T.LeftBrace => matchingOpt(x)
    case x: T.LeftParen => if (parensToo) matchingOpt(x) else None
    case _ => OptionalBraces.get(ft)
        .flatMap(_.rightBrace.map(x => nextNonCommentSameLine(x)))
  }

  def isCloseDelimForTrailingCommasMultiple(ft: FormatToken): Boolean =
    ft.meta.rightOwner match {
      case x: Importer => x.importees.lengthCompare(1) > 0
      // take last arg when multiple
      case x => getArgsOrNil(x).view.drop(1).lastOption match {
          case None | Some(_: Term.Repeated) => false
          case Some(t: Term.Param) => !t.decltpe.is[Type.Repeated]
          case _ => true
        }
    }

  def rightIsCloseDelimToAddTrailingComma(left: T, ft: => FormatToken)(implicit
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

  def getMustDangleForTrailingCommas(getCloseFt: => FormatToken)(implicit
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

  def getBinpackDefnSiteFlags(
      ftAfterOpen: => FormatToken,
      ftBeforeClose: FormatToken,
  )(implicit
      style: ScalafmtConfig,
      clauseSiteFlags: ClauseSiteFlags,
  ): BinpackSiteFlags =
    getBinpackSiteFlags(ftAfterOpen, ftBeforeClose, literalArgList = false)

  def getBinpackCallSiteFlags(
      ftAfterOpen: FormatToken,
      ftBeforeClose: FormatToken,
  )(implicit
      style: ScalafmtConfig,
      clauseSiteFlags: ClauseSiteFlags,
  ): BinpackSiteFlags = {
    val literalArgList = styleMap.opensLiteralArgumentList(ftAfterOpen)
    getBinpackSiteFlags(ftAfterOpen, ftBeforeClose, literalArgList)
  }

  def getBinpackSiteFlags(
      defnSite: Boolean,
      ftAfterOpen: FormatToken,
      ftBeforeClose: FormatToken,
  )(implicit
      style: ScalafmtConfig,
      clauseSiteFlags: ClauseSiteFlags,
  ): BinpackSiteFlags =
    if (defnSite) getBinpackDefnSiteFlags(ftAfterOpen, ftBeforeClose)
    else getBinpackCallSiteFlags(ftAfterOpen, ftBeforeClose)

  def getBinpackSiteFlags(
      getFtAfterOpen: => FormatToken,
      ftBeforeClose: FormatToken,
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
      val close = ftBeforeClose.right
      if (scalaJsStyle) Policy(Policy.End == close, prefix = "tuckSJS") {
        case Decision(`ftBeforeClose`, ss) => Decision.noNewlineSplits(ss)
      }
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
      ftBeforeClose: FormatToken,
      bpFlags: BinpackSiteFlags,
  ): FormatToken =
    if (bpFlags.scalaJsStyle) {
      val ftAfterClose = nextNonCommentAfter(ftBeforeClose)
      val continue = ftAfterClose != ftBeforeClose &&
        ftAfterClose.right.is[T.RightParen] && ftAfterClose.noBreak &&
        isArgClauseSite(ftAfterClose.meta.rightOwner)
      if (continue) {
        val open = matching(ftAfterClose.right)
        implicit val style: ScalafmtConfig = styleMap.at(open)
        implicit val clauseSiteFlags: ClauseSiteFlags = ClauseSiteFlags
          .atCallSite(ftAfterClose.meta.rightOwner)
        val bpFlagsAfter = getBinpackCallSiteFlags(open, ftAfterClose)
        scalaJsOptCloseOnRight(ftAfterClose, bpFlagsAfter)
      } else ftBeforeClose
    } else ftBeforeClose

  object BinPackOneline {

    private def noRighDelim(
        xtok: Token,
        xft: FormatToken,
    ): Option[FormatToken] = xtok match {
      case _: T.CloseDelim => None
      case _: T.Comma => Some(null) // trailing comma, has NL
      case _: T.Comment => if (xft.noBreak) None else Some(null)
      case _ => Some(xft)
    }

    private def policyOnRightDelim(
        ft: FormatToken,
        exclude: TokenRanges,
    ): (Option[T], Policy) = {
      val beforeDelims = findTokenWith(ft, prev) { xft =>
        noRighDelim(xft.left, xft)
      }.merge
      if (beforeDelims eq null) return (None, NoPolicy)

      val afterDelims = findTokenWith(ft, next) { xft =>
        noRighDelim(xft.right, xft)
      }.merge
      if (afterDelims eq null) return (None, NoPolicy)

      def closeBreakPolicy() = {
        @tailrec
        def iter(currft: FormatToken): Option[Policy] = {
          val prevft = prevNonComment(currft)
          val tok = prevft.left
          val breakBeforeClose = matchingOpt(tok) match {
            case Some(open) =>
              val cfg = styleMap.at(open)
              def cfgStyle = cfg.configStyleCallSite.prefer
              def dangle = cfg.danglingParentheses
                .atCallSite(prevft.meta.leftOwner)
              cfg.newlines.source match {
                case Newlines.unfold => true
                case Newlines.fold => cfgStyle ||
                  !cfg.binPack.indentCallSiteOnce
                case _ => !cfgStyle || dangle || prev(prevft).hasBreak
              }
            case _ => false
          }
          if (breakBeforeClose) Some(decideNewlinesOnlyBeforeClose(tok))
          else if (prevft eq beforeDelims) None
          else iter(prev(prevft))
        }

        iter(afterDelims)
      }

      def policyWithDelay(policy: Policy) = {
        val beforeDelimsEnd = beforeDelims.right.end
        // force break if multiline and if there's no other break
        delayedBreakPolicy(Policy.End == beforeDelimsEnd, exclude)(
          Policy.RelayOnSplit { case (s, nextft) =>
            s.isNL && nextft.right.end > beforeDelimsEnd // don't need anymore
          }(policy, NoPolicy),
        )
      }

      (afterDelims.right match {
        case c: T.Dot => // check if Dot rule includes a break option
          c -> GetSelectLike.onRightOpt(afterDelims).map { x =>
            implicit val cfg = styleMap.at(afterDelims)
            cfg.newlines.getSelectChains match {
              case Newlines.classic =>
                val (expireTree, nextSelect) =
                  findLastApplyAndNextSelect(x.tree, cfg.encloseSelectChains)
                Right(canStartSelectChain(x, nextSelect, expireTree))
              case Newlines.keep => Left(Policy.on(c, "BP1L.NL") {
                  case Decision(`afterDelims`, ss) => Decision
                      .onlyNewlineSplits(ss)
                      .map(_.preActivateFor(SplitTag.SelectChainBinPackNL))
                })
              case _ => Right(true)
            }
          }.getOrElse(Right(false))
        case x @ LeftParenOrBracket() =>
          nextNonCommentSameLineAfter(afterDelims).right match {
            case _: T.Comment => null
            case c => // check if break would cause cfg style but not otherwise
              val cfg = styleMap.at(x)
              val ok = cfg.newlines.sourceIgnored || ! {
                cfg.configStyleCallSite.prefer &&
                cfg.danglingParentheses.atCallSite(afterDelims.meta.rightOwner)
              } || next(afterDelims).hasBreak
              c -> Right(ok)
          }
        case _ => null
      }) match {
        case null => (None, NoPolicy)
        case (c, policyOrOkToBreak) =>
          val policyOpt = policyOrOkToBreak match {
            case Left(policy) => Some(policy)
            case Right(okToBreak) if !okToBreak => closeBreakPolicy()
            case _ => Some(decideNewlinesOnlyBeforeToken(c))
          }
          (Some(c), policyOpt.fold(Policy.noPolicy)(policyWithDelay))
      }
    }

    def getPolicy(isCallSite: Boolean, exclude: TokenRanges)(
        afterArg: FormatToken,
    )(implicit fileLine: FileLine): (Option[T], Policy) = afterArg.right match {
      case c: T.Comma // check for trailing comma, which needs no breaks
          if !nextNonCommentAfter(afterArg).right.is[T.CloseDelim] =>
        (None, splitOneArgPerLineAfterCommaOnBreak(exclude)(c))
      case _: T.CloseDelim if isCallSite => policyOnRightDelim(afterArg, exclude)
      case _ => (None, NoPolicy)
    }
  }

  def indentedPackage(body: Pkg.Body): Boolean = getHeadOpt(body)
    .exists(_.meta.leftOwner eq body)
  @inline
  def indentedPackage(pkg: Pkg): Boolean = indentedPackage(pkg.body)

}

object FormatOps {
  class SelectLike(val tree: Term, val qual: Term, val nameFt: FormatToken) {
    def nameToken: Token = nameFt.left
  }

  object SelectLike {
    def apply(tree: Term.Select)(implicit ftoks: FormatTokens): SelectLike =
      new SelectLike(tree, tree.qual, ftoks.getHead(tree.name))
    def apply(tree: Term.Match, kw: FormatToken): SelectLike =
      new SelectLike(tree, tree.expr, kw)
  }

  case class TemplateSupertypeGroup(
      superType: Tree,
      superTypeGroup: Seq[Tree],
      expireTokenFunc: Seq[Tree] => FormatToken,
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
    def rightBrace: Option[FormatToken]
  }

  def getOpenParenAlignIndents(
      end: T,
  )(implicit style: ScalafmtConfig): Seq[Indent] =
    if (style.align.closeParenSite) Seq(
      Indent(Length.StateColumn, end, ExpiresOn.After),
      Indent(Length.Num(1), end, ExpiresOn.Before),
      Indent(Length.Num(-1), end, ExpiresOn.After),
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
      configStyle = style.configStyleDefnSite,
      alignOpenDelim = style.align.atDefnSite(owner),
      dangleCloseDelim = style.danglingParentheses.atDefnSite(owner),
    )

    def atCallSite(
        owner: Tree,
    )(implicit style: ScalafmtConfig): ClauseSiteFlags = ClauseSiteFlags(
      configStyle = style.configStyleCallSite,
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

  class ExtractFromMeta[A](f: FormatToken.Meta => Option[A]) {
    def unapply(meta: FormatToken.Meta): Option[A] = f(meta)
  }

  val ImplicitUsingOnLeft =
    new ExtractFromMeta(meta => TreeOps.getImplicitParamList(meta.leftOwner))

  val WithTemplateOnLeft = new ExtractFromMeta(_.leftOwner match {
    case lo: Stat.WithTemplate => Some(lo.templ)
    case _ => None
  })

  val TemplateOnRight = new ExtractFromMeta(_.rightOwner match {
    case ro: Template => Some(ro)
    case _ => None
  })

  val EnumeratorAssignRhsOnLeft = new ExtractFromMeta(_.leftOwner match {
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
