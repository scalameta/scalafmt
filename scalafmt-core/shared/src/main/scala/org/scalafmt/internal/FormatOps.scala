package org.scalafmt.internal

import org.scalafmt.config._
import org.scalafmt.rewrite.RedundantBraces
import org.scalafmt.util._

import org.scalameta.FileLine
import scala.meta.classifiers.Classifier
import scala.meta.internal.tokens.Chars.isOperatorPart
import scala.meta.tokens.{Token => T}
import scala.meta.{Token => _, _}

import scala.annotation.tailrec

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

  implicit val (tokens: FormatTokens, styleMap: StyleMap) =
    FormatTokens(topSourceTree)(baseStyle)

  private[internal] val initStyle = styleMap.init
  implicit val dialect: Dialect = initStyle.dialect

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
      case _: T.Comma | _: T.Semicolon | _: T.Equals |
          _: T.Interpolation.Start | _: T.Interpolation.SpliceEnd |
          _: T.Interpolation.End | _: T.Interpolation.SpliceStart |
          _: T.Interpolation.Part => null
      case _: T.RightArrow => // given breaks before `=>`
        if (start.rightOwner.is[Member.ParamClauseGroup]) start else null
      case _ if start.hasBlankLine => start
      case _
          if AsInfixOp(start.rightOwner)
            .orElse(AsInfixOp(prevNonComment(start).leftOwner))
            .exists(style.newlines.infix.get(_).isNone) =>
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
          val bpFlags =
            getBinpackSiteFlags(isDefnSite, matchingRight(start), start)
          if (bpFlags.scalaJsStyle)
            if (start.hasBreak) start else scalaJsOptCloseOnRight(start, bpFlags)
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
      case _: T.Colon
          if !style.newlines.sometimesBeforeColonInMethodReturnType &&
            colonDeclType(start.rightOwner).isDefined => tokens(start, 2) // can't break after colon either
      case _: T.Comment if start.noBreak =>
        val nft = nextNonCommentSameLineAfter(start)
        if (!start.left.is[T.LeftParen] || nft.hasBreakOrEOF) return nft // RETURN!!!
        start
      case _ => start
    }

    if (nft eq start) start
    else getSlbEndOnLeft(if (nft ne null) nft else next(start))
  }

  def parensTuple(ft: FT): TokenRanges = matchingOptLeft(ft)
    .fold(TokenRanges.empty)(other => TokenRanges(TokenRange(ft, other)))
  def parensTuple(tree: Tree): TokenRanges = parensTuple(getLast(tree))

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

  def splitOneArgPerLineAfterCommaOnBreak(
      comma: FT,
  )(implicit fl: FileLine, ft: FT): Policy =
    splitOneArgPerLineAfterCommaOnBreak(TokenRanges.empty)(comma)

  def splitOneArgPerLineAfterCommaOnBreak(
      exclude: TokenRanges,
  )(comma: FT)(implicit fl: FileLine, ft: FT): Policy = Policy ?
    (comma.right.is[T.Comment] && comma.noBreak) || {
      val policy = delayedBreakPolicy(Policy.End < comma, exclude)(
        Policy.onlyFor(comma, prefix = "NL->A[,]")(
          getOneArgPerLineSplitsAfterComma(comma.right, _),
        ),
      )
      Policy.End >= nextNonComment(ft) ==> policy
    }

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
    // we'll be RETURNING!!!
    val op = app.op.value
    if (app.is[Type] && style.spaces.neverAroundInfixTypes.contains(op))
      return Seq(Split(NoSplit, 0))

    val isBeforeOp = ft.meta.leftOwner ne app.op
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
    def spc(implicit fl: FileLine) = Split(spaceMod, 0)
    def spaceSplits(implicit fl: FileLine) = Seq(spc)

    val afterInfix = style.newlines.infix.get(app)
    if (isBeforeOp && afterInfix.sourceIgnoredAt(ft)) return spaceSplits

    val (fullInfix, fullInfixEnclosedIn) = InfixSplits
      .findMaybeEnclosingInfix(app)

    def sourceIgnoredSplits(implicit fl: FileLine) = {
      val ok = fullInfixEnclosedIn.isDefined || fullInfix.parent.forall {
        case t: Defn.Val => t.rhs eq fullInfix
        case t: Defn.Var => t.body eq fullInfix
        case _ => true
      }
      if (ok) InfixSplits(app, ft, fullInfix, fullInfixEnclosedIn)
        .getBeforeLhsOrRhs(afterInfix, spaceMod = spaceMod)
      else spaceSplits
    }

    if (afterInfix.sourceIgnored) return sourceIgnoredSplits

    // we don't modify line breaks generally around infix expressions
    // TODO: if that ever changes, modify how rewrite rules handle infix
    val fullInfixEnclosedInParens = fullInfixEnclosedIn.exists(_.isRight)
    def okSpaceAfterOp(xft: FT) = xft.noBreak &&
      (style.newlines.ignoreInSyntax || tokens.getNonMultilineEnd(xft).isDefined)
    def okBreakBeforeOp: Boolean = fullInfixEnclosedInParens ||
      initStyle.dialect.allowInfixOperatorAfterNL ||
      (fullInfix.parent match {
        case Some(p: Case) => p.cond.contains(fullInfix)
        case Some(p: Member.ArgClause) => isEnclosedWithinParens(p)
        case _ => false
      }) || app.op.tokens.rfindWideNot(_.is[T.HTrivia], -1, Int.MinValue)
        .exists(_.is[T.AtEOL]) // had a break in the original code

    def modNL = Newline2x(fullInfixEnclosedInParens && ft.hasBlankLine)
    def nl(cost: Int)(implicit fl: FileLine) = Split(modNL, cost)
    def withIndent(split: Split) =
      Seq(InfixSplits.withNLIndent(split, app, (fullInfix, fullInfixEnclosedIn)))

    if (isBeforeOp)
      if (ft.noBreak) withIndent(spc) // !sourceIgnored
      else if (!okBreakBeforeOp) spaceSplits
      else if (isFewerBracesRhs(app.arg)) Seq(nl(0))
      else if (!afterInfix.isNone) withIndent {
        val nft = next(ft) // if we break before, let's not break immediately after
        if (okSpaceAfterOp(nft)) nl(0).withSingleLine(nft)
          .andPolicy(penalizeOneNewline(nft, Constants.ShouldBeSingleLine))
        else nl(0)
      }
      else withIndent(nl(0))
    else {
      // now are after the op
      val useSpace = okSpaceAfterOp(ft)
      if (afterInfix.isNone) withIndent(if (useSpace) spc else nl(0))
      else if (useSpace)
        if (prev(ft).hasBreak && okBreakBeforeOp) spaceSplits
        else sourceIgnoredSplits
      else (ft.right match {
        case _: T.LeftBrace => matchingOptRight(ft)
        case _ => None
      }) match {
        case Some(cft) if cft.right.end >= app.arg.pos.end =>
          sourceIgnoredSplits
            .map(s => if (s.isNL) s.copy(cost = 0, rank = -1) else s)
        case _ => withIndent(nl(1))
      }
    }
  }

  def functionExpire(function: Member.Function): (FT, ExpiresOn) =
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
      case _ => Policy.NoPolicy
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
      Seq(Split(nlMod, 0, nlPolicy(false)).withIndent(indent))
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
        Split(nlMod, 1, nlPolicy(isAlways) & pnlPolicy).withIndent(indent),
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
      val nlSplit = Split(Newline, 1, policy).withIndent(firstIndent)
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
        Split(space, 0, policy).onlyIf(spaceImplicit).andPolicy(
          decideNewlinesOnlyAfterClose(nft),
          isRightCommentThenBreak(nft),
        ).withIndent(firstIndent),
        // Otherwise split vertically
        Split(nlMod, 1, policy).withIndent(firstIndent),
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

  @tailrec
  private def findLastApplyAndNextSelectEnclosed(
      tree: Tree,
      select: Option[Select] = None,
      prevApply: Option[Tree] = None,
  ): (Tree, Option[Select]) =
    if (isEnclosedWithinParens(tree)) (prevApply.getOrElse(tree), select)
    else tree.parent match {
      case Some(Select(p)) =>
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
      select: Option[Select] = None,
      prevEnclosed: Option[Tree] = None,
      prevApply: Option[Tree] = None,
  ): (Tree, Option[Select]) = tree.parent match {
    case Some(Select(p)) =>
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
  ): (Tree, Option[Select]) =
    if (enclosed) findLastApplyAndNextSelectEnclosed(tree)
    else findLastApplyAndNextSelectPastEnclosed(tree)

  def canStartSelectChain(
      thisSelectLike: Select,
      nextSelectLike: Option[Select],
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
      prevSelect: Option[Select],
      thisSelect: Select,
      lastApply: Tree,
  )(implicit style: ScalafmtConfig): Boolean = prevSelect match {
    case None => false
    case Some(p) if canStartSelectChain(p, Some(thisSelect), lastApply) => true
    case Some(p) => inSelectChain(Select.prev(p), p, lastApply)
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

      private val penalizeOpenNL: Policy.Pf = {
        case Decision(ft, s) if !ft.left.is[T.Comment] => s.penalizeNL(1)
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
        val miniSlbEnd = getSlbEndOnLeft(next(ft))
        val slbPolicy = SingleLineBlock(miniSlbEnd)
        val slbFails = slbPolicy.exists { // wouldn't check, past expiration
          case p: SingleLineBlock => p.failsLeftSyntaxNL(miniSlbEnd)
          case _ => false
        }
        if (slbFails) Split.ignored
        else {
          val slbLite = style.newlines.keep &&
            (body.parent match {
              case Some(p: Term.Assign) => !p.parent.is[Term.ArgClause] ||
                style.binPack.callSite == BinPack.Site.Never
              case _ => true
            })
          val opt = if (style.newlines.keep) miniSlbEnd else expire
          Split(Space, 0, slbPolicy & (policy | penalize(penalty)))
            .withOptimalToken(opt, killOnFail = slbLite, recurseOnly = slbLite)
        }
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
      def getExprBeg(expr: Tree): FT = {
        val exprBeg = getHead(expr)
        val exprHasLB = exprBeg.left.is[T.LeftBrace]
        if (exprHasLB) exprBeg else prevNonCommentBefore(exprBeg)
      }
      val (spaceSplit, nlSplit) = adjustedBody match {
        case t: Term.If =>
          if (isKeep || ifWithoutElse(t) || hasStateColumn)
            getSplits(getSlbSplit(getExprBeg(t.thenp)))
          else getSlbSplits()
        case t: Term.TryClause =>
          if (hasStateColumn) getSplits(getSpaceSplit(1))
          else if (isKeep) getSplits(getSlbSplit(getExprBeg(t.expr)))
          else getSlbSplits()
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
          val lia = InfixSplits.findLeftInfix(ia)
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

    private def unfoldedSpaceNonEmptyNonComment(
        body: Tree,
        slbOnly: Boolean,
        last: FT,
    )(implicit style: ScalafmtConfig): Split = {
      val expire = nextNonCommentSameLine(last)
      def slbSplit(end: FT)(implicit fileLine: FileLine) = Split(Space, 0)
        .withSingleLine(end, noSyntaxNL = true)
      if (slbOnly) slbSplit(expire)
      else getBlockStat(body) match {
        // we force newlines in for/yield
        case _: Term.ForYield => Split.ignored
        // we force newlines in try/catch/finally
        case _: Term.TryClause => Split.ignored
        // don't tuck curried apply
        case t: Term.Apply if t.fun.is[Term.Apply] => slbSplit(expire)
        case t => slbSplit(getEndOfFirstCall(t).getOrElse(expire))
      }
    }

    private def unfoldedNonComment(
        body: Tree,
        nlSplitFunc: Int => Split,
        spaceIndents: Seq[Indent],
        slbOnly: Boolean,
    )(implicit style: ScalafmtConfig, ft: FT): Seq[Split] =
      if (tokens.isEmpty(body)) Seq(Split(Space, 0).withIndents(spaceIndents))
      else {
        val nextFt = nextNonCommentSameLineAfter(ft)
        val last = getLastNonTrivial(body)
        getClosingIfWithinParens(last)(nextFt).fold {
          val spaceSplit =
            if (!style.newlines.sourceIgnored && ft.hasBreak) Split.ignored
            else unfoldedSpaceNonEmptyNonComment(body, slbOnly, last)
          Seq(spaceSplit.withIndents(spaceIndents), nlSplitFunc(1).forThisLine)
        } { _ =>
          val policy = Policy.onlyFor(nextFt, "CTRLUF")(ss =>
            ss.map(s =>
              if (s.isNL) s
              else s.withSingleLine(last, extend = true, noOptimal = !s.noCost),
            ),
          )
          Seq(
            Split(Space, 0, policy),
            nlSplitFunc(1).forThisLine
              .withSingleLineNoOptimal(last, extend = true),
          )
        }
      }

    def checkComment(
        nlSplitFunc: Int => Split,
    )(splitsFunc: FT => Seq[Split])(implicit ft: FT): Seq[Split] =
      if (!ft.right.is[T.Comment] && !ft.hasBlankLine) splitsFunc(ft)
      else if (ft.hasBreak) Seq(nlSplitFunc(0).forThisLine)
      else {
        val nextFt = nextNonCommentSameLineAfter(ft)
        val nlPolicy = decideNewlinesOnlyAfterClose(nextFt)
        if (nextFt.hasBreakOrEOF)
          Seq(nlSplitFunc(1).forThisLine.withMod(Space).andPolicy(nlPolicy))
        else splitsFunc(nextFt)
          .map(s => s.withMod(Space).andPolicy(nlPolicy, !s.isNL))
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
    )(_ => unfoldedNonComment(body, nlSplitFunc, spaceIndents, slbOnly = true))

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
          unfoldedNonComment(body, nlSplitFunc, spaceIndents, slbOnly = false)
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

  }

  def withNLPolicy(endFt: FT)(nlSplit: Split): Split = {
    val nft = nextNonCommentSameLine(endFt)
    nlSplit.andPolicy(nft.right match {
      case _: T.Keyword => decideNewlinesOnlyBeforeToken(next(nft))
      case x: T.Semicolon =>
        val semi = nextNonCommentSameLineAfter(nft)
        if (semi.noBreak || (semi.left eq x) && !semi.right.is[T.Comment])
          decideNewlinesOnlyAfterToken(semi)
        else Policy.NoPolicy
      case _ => Policy.NoPolicy
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
    val literalArgList = styleMap.isForcedBinPack(ftAfterOpen)
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
        case _ => Policy.NoPolicy
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
          closeBreak && preserveConfigStyle(ftAfterOpen, breakBeforeClose = true)
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
      if (beforeDelims eq null) return (None, Policy.NoPolicy)

      val afterDelims = getAfterRightDelims(ft)
      if (afterDelims eq null) return (None, Policy.NoPolicy)

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
          }(policy)(Policy.NoPolicy),
        )

      (afterDelims.right match {
        case _: T.Dot => // check if Dot rule includes a break option
          afterDelims -> Select.onRightOpt(afterDelims).map { x =>
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
        case null => (None, Policy.NoPolicy)
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
    )(implicit fl: FileLine, ft: FT): (Option[FT], Policy) =
      afterArg.right match {
        case _: T.Comma // check for trailing comma, which needs no breaks
            if !nextNonCommentAfter(afterArg).right.is[T.CloseDelim] =>
          (None, splitOneArgPerLineAfterCommaOnBreak(exclude)(next(afterArg)))
        case _: T.CloseDelim if isCallSite =>
          policyOnRightDelim(afterArg, exclude)
        case _ => (None, Policy.NoPolicy)
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
        case Some(_: Term.FunctionLike) => Some(TokenRanges.empty)
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

}

object FormatOps {

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

}
