package org.scalafmt.internal

import org.scalafmt.Error.UnexpectedTree
import org.scalafmt.config.Align
import org.scalafmt.config.BinPack
import org.scalafmt.config.ImportSelectors
import org.scalafmt.config.Indents
import org.scalafmt.config.Newlines
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.config.Spaces
import org.scalafmt.internal.ExpiresOn.After
import org.scalafmt.internal.ExpiresOn.Before
import org.scalafmt.internal.Length.Num
import org.scalafmt.internal.Length.StateColumn
import org.scalafmt.internal.Policy.NoPolicy
import org.scalafmt.rewrite.RedundantBraces
import org.scalafmt.util._

import org.scalameta.FileLine
import scala.meta._
import scala.meta.tokens.{Token => T}

import scala.annotation.tailrec
import scala.language.implicitConversions

object Constants {
  val ShouldBeNewline = 100000
  val ShouldBeSingleLine = 30
  val BinPackAssignmentPenalty = 10
  val SparkColonNewline = 10
  val BracketPenalty = 20
  val ExceedColumnPenalty = 1000
  // Breaking a line like s"aaaaaaa${111111 + 22222}" should be last resort.
  val BreakSingleLineInterpolatedString = 1000 * ExceedColumnPenalty
}

/** Assigns splits to format tokens.
  *
  * NOTE(olafurpg). The pattern match in this file has gotten out of hand. It's
  * difficult even for myself to keep track of what's going on in some cases,
  * especially around applications and lambdas. I'm hoping to sunset this file
  * along with BestFirstSearch in favor of
  * https://github.com/scalameta/scalafmt/issues/917
  */
class Router(formatOps: FormatOps) {

  import Constants._
  import FormatOps._
  import LoggerOps._
  import PolicyOps._
  import TokenOps._
  import TreeOps._
  import formatOps._
  import tokens._

  private def getSplitsImpl(implicit ft: FormatToken): Seq[Split] = {
    implicit val style = styleMap.at(ft)
    val leftOwner = ft.meta.leftOwner
    val rightOwner = ft.meta.rightOwner
    val newlines = ft.newlinesBetween
    @inline
    def noBreak(): Boolean = FormatToken.noBreak(newlines)
    @inline
    def hasBreak(): Boolean = !noBreak()
    @inline
    def hasBlankLine: Boolean = FormatToken.hasBlankLine(newlines)

    ft match {
      // between sources (EOF -> @ -> BOF)
      case FormatToken(_: T.EOF, _, _) =>
        Seq(Split(NoSplit.orNL(prev(ft).left.is[T.BOF]), 0))
      case FormatToken(_, _: T.BOF, _) =>
        Seq(Split(NoSplit.orNL(next(ft).right.is[T.EOF]), 0))
      // End files with trailing newline
      case FormatToken(_, _: T.EOF, _) => Seq(Split(Newline, 0))
      case FormatToken(_: T.BOF, _, _) => Seq(Split(NoSplit, 0))
      case FormatToken(_: T.Shebang, _, _) => Seq(Split(Newline2x(ft), 0))
      case FormatToken(start: T.Interpolation.Start, _, m) =>
        val end = matching(start).left
        val policy = {
          val penalty = BreakSingleLineInterpolatedString
          if (style.newlines.inInterpolation eq Newlines.InInterpolation.avoid)
            Policy.on(end, "INTERP-AVOID-NL", rank = -1) {
              case Decision(_, ss) => ss.map { s =>
                  if (s.isNL) s.withPenalty(penalty)
                  else if (s.optimalAt.isEmpty) s
                  else s.copy(optimalAt = None)
                }
            }
          else Policy ?
            (style.newlines.sourceIgnored || isTripleQuote(m.left.text)) ||
            Policy.on(end, "INTERP-KEEP-NONL") {
              case Decision(x, ss) if x.noBreak =>
                ss.map(s => if (s.isNL) s.withPenalty(penalty) else s)
            }
        }
        val split = Split(NoSplit, 0, policy = policy)
        val alignIndents =
          if (style.align.stripMargin) findInterpolate(leftOwner)
            .flatMap { ti =>
              getStripMarginChar(ti).map { pipe =>
                val startsWithPipe = ti.parts.headOption match {
                  case Some(Lit.String(x)) => x.headOption.contains(pipe)
                  case _ => false
                }
                Seq(
                  Indent(StateColumn, end, After),
                  // -1 because of margin characters |
                  Indent(if (startsWithPipe) -1 else -2, end, After),
                )
              }
            }
          else None
        val indents = alignIndents
          .getOrElse(Seq(Indent(style.indent.main, end, After)))
        Seq(split.withIndents(indents))
      // Interpolation
      case FormatToken(
            _: T.Interpolation.Id | _: T.Interpolation.Part |
            _: T.Interpolation.Start | _: T.Interpolation.SpliceStart,
            _,
            _,
          ) => Seq(Split(NoSplit, 0))
      case FormatToken(
            _,
            T.Interpolation.Part(_) | T.Interpolation.End() | T.Interpolation
              .SpliceEnd(),
            _,
          ) => Seq(Split(NoSplit, 0))
      case FormatToken(T.LeftBrace(), T.RightBrace(), _) =>
        Seq(Split(NoSplit, 0))
      // Import
      case FormatToken(_: T.Dot, _, _)
          if existsParentOfType[ImportExportStat](rightOwner) =>
        Seq(Split(NoSplit, 0))
      // Import left brace
      case FormatToken(open: T.LeftBrace, _, _)
          if existsParentOfType[ImportExportStat](leftOwner) =>
        val closeFt = matching(open)
        val close = closeFt.left
        val beforeClose = prev(closeFt)
        val policy = SingleLineBlock(
          close,
          okSLC = style.importSelectors eq ImportSelectors.singleLine,
        )
        val newlineBeforeClosingCurly = decideNewlinesOnlyBeforeClose(close)

        val mustDangleForTrailingCommas =
          getMustDangleForTrailingCommas(beforeClose)
        val mustUseNL = hasBreak() && isRightCommentThenBreak(ft)
        val newlinePolicy = style.importSelectors match {
          case ImportSelectors.singleLine if mustUseNL => policy
          case ImportSelectors.singleLine if !mustDangleForTrailingCommas =>
            NoPolicy
          case ImportSelectors.binPack => newlineBeforeClosingCurly
          case _ => newlineBeforeClosingCurly &
              splitOneArgOneLine(close, leftOwner)
        }

        Seq(
          Split(Space(style.spaces.inImportCurlyBraces), 0)
            .notIf(mustUseNL || mustDangleForTrailingCommas).withPolicy(policy),
          Split(Newline, 1, policy = newlinePolicy).notIf(newlinePolicy.isEmpty)
            .withIndent(style.indent.main, close, Before),
        )
      case FormatToken(_, _: T.RightBrace, _)
          if existsParentOfType[ImportExportStat](rightOwner) =>
        Seq(Split(Space(style.spaces.inImportCurlyBraces), 0))

      // Interpolated string left brace
      case FormatToken(open @ T.LeftBrace(), _, _)
          if prev(ft).left.is[T.Interpolation.SpliceStart] =>
        val closeFt = matching(open)
        val close = closeFt.left
        val alignIndents =
          if (style.align.inInterpolation) Some {
            Seq(
              Indent(Length.StateColumn, close, ExpiresOn.After),
              Indent(Length.Num(style.indent.main), close, ExpiresOn.Before),
              Indent(Length.Num(-1), close, ExpiresOn.After),
            )
          }
          else None
        def spaceSplit(implicit fileLine: FileLine) =
          Split(Space(style.spaces.inInterpolatedStringCurlyBraces), 0)
            .withIndents(alignIndents.getOrElse(Nil))
        def newlineSplit(cost: Int)(implicit fileLine: FileLine) = {
          def mainIndents =
            Seq(Indent(style.indent.main, close, ExpiresOn.Before))
          Split(Newline, cost).withIndents(alignIndents.getOrElse(mainIndents))
            .withPolicy(decideNewlinesOnlyBeforeClose(close))
        }
        def isSimpleInterpolate = !(leftOwner match {
          case t: Pat.Interpolate => findArgAfter(open.end, t.args)
          case t: Term.Interpolate => findArgAfter(open.end, t.args)
          case t: Term.Block => getBlockSingleStat(t)
          case _ => None
        }).isOpt[Term.If]

        style.newlines.inInterpolation match {
          case Newlines.InInterpolation.avoid => Seq(spaceSplit)
          case _ if style.newlines.keepBreak(newlines) => Seq(newlineSplit(0))
          case Newlines.InInterpolation.allow
              if !dialect.allowSignificantIndentation || isSimpleInterpolate =>
            Seq(spaceSplit)
          case _ =>
            /* sequence of tokens:
             * - 0 RBrace
             * - 1 Interpolation.SpliceEnd (empty)
             * - 2 Interpolation.Part (next string)
             * - 3 Interpolation.End (quotes) or Interpolation.SliceStart/LBrace (${)
             */
            val afterClose = tokens(closeFt, 3)
            val lastPart = afterClose.left.is[T.Interpolation.End]
            val slbEnd = endOfSingleLineBlock(
              if (lastPart) afterClose else next(afterClose),
            )
            Seq(spaceSplit.withSingleLine(slbEnd), newlineSplit(1))
        }

      case FormatToken(_, _: T.RightBrace, _)
          if next(ft).right.is[T.Interpolation.SpliceEnd] =>
        Seq(Split(Space(style.spaces.inInterpolatedStringCurlyBraces), 0))

      // optional braces: block follows
      case FormatToken(
            _: T.Equals | _: T.Colon | _: T.KwWith | _: T.RightParen |
            _: T.KwReturn | _: T.ContextArrow | _: T.LeftArrow |
            _: T.RightArrow | _: T.KwMatch | _: T.KwThen | _: T.KwElse |
            _: T.KwThrow | _: T.KwTry | _: T.KwCatch | _: T.KwFinally |
            _: T.KwFor | _: T.KwDo | _: T.KwWhile | _: T.KwYield | _: T.KwIf,
            _,
            OptionalBraces(splits),
          ) if dialect.allowSignificantIndentation => splits

      case FormatToken(_: T.Equals, _, DefValAssignLeft(rhs)) =>
        maybeGetInfixSplitsBeforeLhs() {
          def endFt = getLast(rhs)
          getSplitsDefValEquals(rhs, endFt) {
            if (leftOwner.is[Tree.WithParamClauses])
              getSplitsDefEquals(rhs, endFt)
            else getSplitsValEquals(rhs, endFt)(
              getSplitsValEqualsClassic(rhs, endFt),
            )
          }
        }

      // { ... } Blocks
      case FormatToken(open: T.LeftBrace, right, _) =>
        val closeFT = matching(open)
        val close = closeFT.left
        val newlineBeforeClosingCurly = decideNewlinesOnlyBeforeClose(close)
        val isSelfAnnotationNL = style.optIn.selfAnnotationNewline &&
          (hasBreak() || style.newlines.sourceIgnored) &&
          (leftOwner match { // Self type: trait foo { self => ... }
            case t: Template.Body => t.selfOpt.nonEmpty
            case _ => false
          })
        val rightIsComment = right.is[T.Comment]
        val nl: Modification =
          if (rightIsComment && noBreak()) Space
          else Newline2x(
            hasBlankLine ||
              !isSelfAnnotationNL &&
              rightIsComment && blankLineBeforeDocstring(ft),
          )

        // lambdaNLOnly: None for single line only
        type LambdaInfo = (FormatToken, Int, Option[Boolean])
        def getLambdaNone: LambdaInfo = (null, 0, None)
        @tailrec
        def getLambdaInfo(ts: List[Tree]): LambdaInfo = ts match {
          case (t: Case) :: Nil if t.cond.isEmpty =>
            val arrow = getCaseArrow(t)
            val nlOnly =
              if (style.newlines.alwaysBeforeCurlyLambdaParams) Some(true)
              else if (
                style.newlines.beforeCurlyLambdaParams ne
                  Newlines.BeforeCurlyLambdaParams.never
              ) None
              else Some(false)
            (arrow, 0, nlOnly)
          case (t: Term.FunctionTerm) :: Nil =>
            val arrow = getFuncArrow(lastLambda(t))
            val expire = arrow.getOrElse(getLast(t))
            val nlOnly =
              if (style.newlines.alwaysBeforeCurlyLambdaParams) Some(true)
              else if (
                style.newlines.beforeCurlyLambdaParams eq
                  Newlines.BeforeCurlyLambdaParams.multiline
              ) None
              else Some(false)
            (expire, 0, nlOnly)
          case (t: Term.PartialFunction) :: Nil => getLambdaInfo(t.cases)
          case (t: Term.CasesBlock) :: Nil if (t.parent match {
                case Some(t: Term.Match)
                    if dialect.allowMatchAsOperator &&
                      tokenAfter(t.expr).right.is[T.Dot] => true
                case _ => false
              }) => getLambdaInfo(t.cases)
          case (t: Term.Block) :: Nil if !isEnclosedInBraces(t) =>
            getLambdaInfo(t.stats)
          case _ => getLambdaNone
        }
        val (lambdaExpire, lambdaIndent, lambdaNLOnly) = leftOwner match {
          case t: Template.Body => t.selfOpt.fold {
              if (t.parent.parent.isOpt[Term.NewAnonymous])
                getLambdaInfo(t.stats)
              else getLambdaNone
            } { owner =>
              val anno = owner.tokens.last
              val indent = style.indent.main
              val annoFT = tokens(anno)
              val arrow = annoFT.left.is[T.RightArrow]
              val expire = if (arrow) annoFT else nextAfterNonComment(annoFT)
              (expire, indent, Some(isSelfAnnotationNL))
            }
          case t: Term.ArgClause if isEnclosedInBraces(t) =>
            getLambdaInfo(t.values)
          case t: Term.Block => getLambdaInfo(t.stats)
          case t => getLambdaInfo(t :: Nil)
        }

        def getSingleLinePolicy = {
          val needBreak = closeFT.right match {
            case _: T.KwElse => closeFT.meta.rightOwner match {
                case p: Term.If => p.thenp eq leftOwner
                case _ => false
              }
            case _: T.KwCatch => closeFT.meta.rightOwner match {
                case p: Term.TryClause => p.expr eq leftOwner
                case _ => false
              }
            case _: T.KwFinally => closeFT.meta.rightOwner match {
                case p: Term.TryClause => (p.expr eq leftOwner) ||
                  p.catchClause.contains(leftOwner)
                case _ => false
              }
            case _ => false
          }
          Policy ? needBreak && decideNewlinesOnlyAfterClose(close)
        }
        def getClassicSingleLineDecisionOpt =
          if (noBreak()) Some(true) else None

        def getSingleLineLambdaDecisionOpt = {
          val ok = !lambdaNLOnly.contains(true) &&
            getSpaceAndNewlineAfterCurlyLambda(newlines)._1
          if (ok) Some(true) else None
        }

        // null if skipping
        val singleLineDecisionOpt = style.newlines.source match {
          case Newlines.keep if hasBreak() => None
          case Newlines.unfold => None
          case Newlines.fold =>
            val isTopLevelBlock = leftOwner.parent.forall {
              case t: Template =>
                // false for
                // new A { () =>
                //   println("A")
                // }
                // but true for
                // new A {
                //   def f = x
                // }
                !t.parent.is[Term.NewAnonymous] || (t.body eq leftOwner) &&
                t.body.stats.exists(_.is[Defn])
              case _: Pkg | _: Source | _: Pkg.Body => true
              case t => t.parent.isAnyOpt[Source, Pkg.Body] // includes when parent is empty
            }

            // do not fold top-level blocks
            if (isTopLevelBlock) None
            else if (lambdaExpire != null) getSingleLineLambdaDecisionOpt
            else Some(false)
          // old behaviour
          case _ =>
            if (lambdaExpire == null) getClassicSingleLineDecisionOpt
            else getSingleLineLambdaDecisionOpt
        }

        val noSplitMod = xmlSpace(leftOwner)
        val singleLineSplitOpt = singleLineDecisionOpt.map { sld =>
          val sldPolicy = getSingleLinePolicy
          val expire = leftOwner.parent match {
            case Some(p: Term.ForYield)
                if !sld && sldPolicy.isEmpty && style.newlines.fold &&
                  leftOwner.is[Term.EnumeratorsBlock] => getLastToken(p)
            case _ => endOfSingleLineBlock(closeFT)
          }
          val toParens = initStyle.rewrite.bracesToParensForOneLineApply &&
            RedundantBraces.noSplitForParensOnRightBrace(closeFT).isDefined
          if (toParens) Right {
            val noCloseSpacePolicy = Policy.End < close ==>
              Policy.on(close, "PAREN1LAPPLY") {
                case Decision(FormatToken(_, `close`, _), ss) => ss.map { s =>
                    if (s.isNL) s else s.withMod(NoSplit)
                  }
              }
            // copy logic from `( ...`, binpack=never, defining `slbSplit`
            val slbEnd =
              if (style.newlines.isBeforeOpenParenCallSite) close
              else endOfSingleLineBlock(closeFT)
            Split(NoSplit, 0).withSingleLine(slbEnd, noSyntaxNL = true)
              .andPolicy(sldPolicy & noCloseSpacePolicy)
          }
          else Left {
            Split(noSplitMod, 0).withSingleLine(expire, noSyntaxNL = true)
              .andPolicy(sldPolicy)
          }
        }

        val nlSplit = Split(nl, if (nl.isNL) 1 else 0)
          .withPolicy(newlineBeforeClosingCurly)
          .withIndent(style.indent.main, close, Before)

        // must be after nlSplit
        val noLambdaSplit = style.newlines.keepBreak(newlines) ||
          lambdaExpire == null || !lambdaNLOnly.contains(false)
        val lambdaSplit =
          if (noLambdaSplit) Split.ignored
          else {
            val arrowOptimal = getOptimalTokenFor(lambdaExpire)
            val policy = singleLineSplitOpt match {
              case Some(Left(slbSplit)) => Policy.after(arrowOptimal, "FNARR") {
                  case Decision(FormatToken(`arrowOptimal`, _, _), ss) =>
                    val nlPolicy = newlineBeforeClosingCurly
                    var hadNoSplit = false
                    val nlSplits = ss.flatMap { s =>
                      if (s.isNL) Some(s.andPolicy(nlPolicy))
                      else { hadNoSplit = true; None }
                    }
                    if (hadNoSplit) slbSplit.withMod(Space) +: nlSplits
                    else nlSplits
                }
              case _ => newlineBeforeClosingCurly &
                  decideNewlinesOnlyAfterToken(arrowOptimal)
            }
            Split(noSplitMod, 0).withSingleLine(arrowOptimal).andPolicy(policy)
              .withIndent(lambdaIndent, close, Before)
          }

        val singleLineSplit = singleLineSplitOpt match {
          case Some(Right(slbSplit)) => slbSplit
          case Some(Left(slbSplit)) if lambdaSplit.isIgnored => slbSplit
          case _ => Split.ignored
        }
        val splits = Seq(singleLineSplit, lambdaSplit, nlSplit)
        right match {
          case t: T.Xml.Start => withIndentOnXmlStart(t, splits)
          case _ => splits
        }

      case FormatToken(_: T.RightArrow | _: T.ContextArrow, r, _)
          if (leftOwner match {
            case t: Term.FunctionTerm => !r.is[T.Comment] &&
              t.body.tokens.nonEmpty && isBlockFunction(t)
            case _ => false
          }) =>
        val leftFunc = leftOwner.asInstanceOf[Term.FunctionTerm]
        val (afterCurlySpace, afterCurlyNewlines) =
          getSpaceAndNewlineAfterCurlyLambda(newlines)
        def spaceSplitBase(implicit line: FileLine): Split = Split(Space, 0)
        val spaceSplit = leftFunc.body match {
          case _: Term.FunctionTerm => spaceSplitBase
          case Term.Block((_: Term.FunctionTerm) :: Nil)
              if !nextNonComment(ft).right.is[T.LeftBrace] => spaceSplitBase
          case _ if afterCurlySpace && {
                style.newlines.fold || !rightOwner.is[Defn]
              } =>
            val exp = getOptimalTokenFor(getLastNonTrivial(leftFunc.body).left)
            spaceSplitBase.withSingleLine(exp, noSyntaxNL = true)
          case _ => Split.ignored
        }
        val (endIndent, expiresOn) = functionExpire(leftFunc)
        Seq(
          spaceSplit,
          Split(afterCurlyNewlines, 1)
            .withIndent(style.indent.main, endIndent, expiresOn),
        )

      case FormatToken(_: T.RightArrow | _: T.ContextArrow, right, _)
          if (leftOwner match {
            case _: Term.FunctionTerm | _: Term.PolyFunction => true
            case t: Self => t.ancestor(2).is[Term.NewAnonymous]
            case _ => false
          }) =>
        val (endOfFunction, expiresOn) = leftOwner match {
          case t: Term.FunctionTerm => functionExpire(t)
          case t => getLastNonTrivialToken(t) -> ExpiresOn.Before
        }

        val indent = // don't indent if the body is empty `{ x => }`
          if (isEmptyFunctionBody(leftOwner) && !right.is[T.Comment]) 0
          else if (leftOwner.is[Template]) 0 // { applied the indent
          else (leftOwner.parent match {
            case Some(ac: Term.ArgClause) => ac.parent
            case Some(x: Term) => x.parent.flatMap {
                case ac: Term.ArgClause => ac.parent
                case _ => None
              }
            case _ => None
          }) match {
            case Some(p: Term.Apply) if isFewerBraces(p) =>
              style.indent.getSignificant
            case _ => style.indent.main
          }

        def noSingleLine = {
          // for constructors with empty args lambda
          // new Foo { () =>
          //   println("wow")
          // }
          val isCurlyLambda = leftOwner.is[Template.Body] ||
            leftOwner.parent.is[Template.Body]

          def noSquash = style.newlines.afterCurlyLambdaParams ne
            Newlines.AfterCurlyLambdaParams.squash

          style.newlines.source match {
            case Newlines.fold => false
            case Newlines.unfold => isCurlyLambda && noSquash
            case Newlines.keep => hasBreak()
            case Newlines.classic => isCurlyLambda && hasBreak() && noSquash
          }
        }
        def newlineSplit(cost: Int)(implicit fileLine: FileLine) =
          Split(Newline, cost).withIndent(indent, endOfFunction, expiresOn)
        if (isRightCommentThenBreak(ft))
          Seq(newlineSplit(if (ft.noBreak) 0 else 1))
        else {
          // 2020-01: break after same-line comments, and any open brace
          val nonComment = nextNonCommentSameLine(ft)
          val hasBlock = nonComment.right.is[T.LeftBrace] &&
            (matching(nonComment.right).left eq endOfFunction)
          val noSplit =
            if (!hasBlock && (nonComment eq ft)) Split(noSingleLine, 0)(Space)
              .withSingleLine(endOfFunction)
            else
              // break after the brace or comment if fits, or now if doesn't
              // if brace, don't add indent, the LeftBrace rule will do that
              Split(Space, 0)
                .withIndent(indent, endOfFunction, expiresOn, hasBlock)
                .withOptimalToken(
                  getOptimalTokenFor(next(nonComment)),
                  killOnFail = false,
                )
          Seq(noSplit, newlineSplit(1 + nestedApplies(leftOwner)))
        }

      // Case arrow
      case FormatToken(_: T.RightArrow, rt, _)
          if leftOwner.is[CaseTree] && !rt.isAny[T.KwCatch, T.KwFinally] =>
        val owner = leftOwner.asInstanceOf[CaseTree]
        val body = owner.body
        val condIsDefined = leftOwner match {
          case c: Case => c.cond.isDefined
          case _ => false
        }
        val bodyIsEmpty = isEmptyTree(body)
        def baseSplit(implicit l: FileLine) = Split(Space, 0)
        def nlSplit(ft: FormatToken)(cost: Int)(implicit l: FileLine) =
          Split(Newline2x(ft), cost)
        CtrlBodySplits.checkComment(nlSplit(ft)) { nft =>
          def withSlbSplit(implicit l: FileLine) = Seq(
            baseSplit.withSingleLine(getLastNonTrivialToken(body)),
            nlSplit(nft)(1),
          )
          implicit val beforeMultiline = style.newlines.getBeforeMultiline
          def getNLOnlySplit(cost: Int)(implicit l: FileLine) =
            Seq(nlSplit(nft)(cost))
          def getFolded(isKeep: Boolean)(implicit l: FileLine) = CtrlBodySplits
            .foldedNonEmptyNonComment(body, nlSplit(nft), isKeep)
          if (
            isCaseBodyABlock(nft, owner) ||
            getClosingIfCaseBodyEnclosedAsBlock(nft, owner).isDefined
          ) Seq(baseSplit)
          else if (nft.right.is[T.KwCase]) getNLOnlySplit(0)
          else if (hasBreak() && !beforeMultiline.ignoreSourceSplit)
            if ((beforeMultiline eq Newlines.keep) && !bodyIsEmpty)
              getFolded(isKeep = true).filter(_.isNL)
            else getNLOnlySplit(1)
          else if (bodyIsEmpty)
            if (rt.isAny[T.RightBrace, T.Semicolon])
              Seq(baseSplit, nlSplit(nft)(1))
            else getNLOnlySplit(1)
          else if (beforeMultiline eq Newlines.unfold)
            if (style.newlines.unfold) getNLOnlySplit(0) else withSlbSplit
          else if (
            condIsDefined || beforeMultiline.eq(Newlines.classic) ||
            getSingleStatExceptEndMarker(body).isEmpty
          ) withSlbSplit
          else getFolded(beforeMultiline eq Newlines.keep)
        }
      // New statement
      case FormatToken(_: T.Semicolon, _, StartsStatementRight(stmt))
          if !stmt.is[Term.EndMarker] =>
        Seq(
          Split(!style.newlines.okSpaceForSource(newlines), 0)(Space)
            .withSingleLine(endOfSingleLineBlock(getLast(stmt))),
          // For some reason, this newline cannot cost 1.
          Split(Newline2x(ft), 0),
        )

      case FormatToken(
            _: T.RightParen,
            _,
            ParamClauseParentLeft(_: Defn.ExtensionGroup),
          )
          if !dialect.allowSignificantIndentation &&
            !LeftParenOrBrace(nextNonComment(ft).right) => Seq(Split(Space, 0))

      case FormatToken(left, right, StartsStatementRight(_)) =>
        val annoRight = right.is[T.At]
        val annoLeft = isSingleIdentifierAnnotation(prev(ft))

        if (
          (annoRight || annoLeft) && style.optIn.annotationNewlines &&
          !style.newlines.sourceIgnored
        ) Seq(Split(getMod(ft), 0))
        else maybeGetInfixSplitsBeforeLhs(
          Some(if (left.is[T.Comment] && noBreak()) Space else Newline2x(ft)),
        ) {
          val spaceCouldBeOk = annoLeft &&
            (style.newlines.source match {
              case Newlines.unfold => right.is[T.Comment] ||
                !style.optIn.annotationNewlines && annoRight
              case Newlines.fold => right.is[T.Comment] || annoRight ||
                !style.optIn.annotationNewlines && Reserved(right)
              case Newlines.keep => noBreak() && (annoRight || Reserved(right))
              case _ => noBreak() && Reserved(right)
            })
          def expire = (rightOwner match {
            case Tree.WithBody(body) => tokenBeforeOpt(body)
            case _ => None
          }).fold(right) { x =>
            val y = nextNonCommentSameLine(x)
            if (y.noBreak && y.right.is[T.LeftBrace]) y.right else y.left
          }
          Seq(
            // This split needs to have an optimalAt field.
            Split(Space, 0).onlyIf(spaceCouldBeOk).withSingleLine(expire),
            // For some reason, this newline cannot cost 1.
            Split(Newline2x(ft), 0),
          )
        }

      case FormatToken(_, T.RightBrace(), _) =>
        Seq(Split(xmlSpace(rightOwner), 0), Split(Newline2x(ft), 0))
      case FormatToken(_: T.KwPackage, _, _) if leftOwner.is[Pkg] =>
        Seq(Split(Space, 0))
      // Opening [ with no leading space.
      // Opening ( with no leading space.
      case FormatToken(left, open @ LeftParenOrBracket(), _)
          if noSpaceBeforeOpeningParen(rightOwner) && {
            val prevFt = prevNonComment(ft)
            prevFt.left match {
              case _: T.RightParen | _: T.RightBrace =>
                prevFt.meta.leftOwner match {
                  case _: Term.For | _: Term.If | _: Term.While => false
                  case t: Term.EnumeratorsBlock => t.parent.is[Term.For]
                  case _ => true
                }
              case _: T.RightBracket | _: T.KwSuper | _: T.KwThis | _: T.Ident |
                  _: T.Underscore | _: T.Constant.Symbol => true
              case _ => false
            }
          } =>
        def modification: Modification = Space(leftOwner match {
          case _ if left.is[T.Comment] => true
          case _: Mod => true
          // Add a space between constructor annotations and their parameter lists
          // see:
          // https://github.com/scalameta/scalafmt/pull/1516
          // https://github.com/scalameta/scalafmt/issues/1528
          case t: Init => t.parent.isOpt[Mod.Annot]
          case Term.Name(name) => style.spaces.afterTripleEquals &&
            name == "===" ||
            (rightOwner match {
              case _: Term.ArgClause => style.spaces.beforeApplyArgInParens(name)
              case _: Member.ParamClause => style.spaces.afterSymbolicDefs &&
                isSymbolicName(name)
              case _ => false
            })
          case _: Defn.ExtensionGroup => style.spaces.afterKeywordBeforeParen &&
            soft.KwExtension.matches(left)
          case _ => false
        })
        def baseNoSplit(implicit fileLine: FileLine) = Split(modification, 0)
        val defn = isParamClauseSite(rightOwner)
        val defRhs = if (defn) defDefBodyParent(rightOwner) else None
        val beforeDefRhs = defRhs.flatMap(tokenJustBeforeOpt)
        def getSplitsBeforeOpenParen(
            src: Newlines.SourceHints,
            indentLen: Int,
            shouldAlignBefore: Align => Boolean,
        )(lastSyntaxClause: => Option[Member.SyntaxValuesClause]) = {
          val closeFt = matching(open)
          val close = closeFt.left
          val indent = Indent(indentLen, close, ExpiresOn.After)
          val isAlignFirstParen = shouldAlignBefore(style.align) &&
            !prevNonComment(ft).left.is[T.RightParen]
          def noSplitSplit(implicit fileLine: FileLine) =
            if (isAlignFirstParen) baseNoSplit
            else baseNoSplit.withSingleLine(close)
          def afterClose: Option[T] = {
            val ftAfterClose = nextNonComment(closeFt)
            val tokAfterClose = ftAfterClose.right
            val matches = tokAfterClose match {
              case _: T.LeftParen => true
              case _: T.Colon if defn =>
                ftAfterClose.left.is[T.Comment] ||
                style.newlines.sometimesBeforeColonInMethodReturnType &&
                colonDeclType(ftAfterClose.meta.rightOwner).isDefined
              case _ => false
            }
            if (matches) Some(tokAfterClose) else None
          }
          val splits = src match {
            case Newlines.unfold =>
              val rightParent = rightOwner.parent.get
              val slbEnd =
                if (defn) beforeDefRhs
                  .fold(getLastToken(rightParent))(prevNonComment(_).left)
                else getLastToken(getLastCall(rightParent))
              Seq(
                baseNoSplit.withSingleLine(slbEnd),
                Split(Newline, 1).withIndent(indent).withPolicy(
                  penalizeNewlineByNesting(open, close),
                  isSeqMulti(getArgs(ft.meta.rightOwner)),
                ).andPolicy(afterClose.map(decideNewlinesOnlyBeforeClose)),
              )
            case Newlines.keep =>
              if (hasBreak()) Seq(Split(Newline, 0).withIndent(indent))
              else Seq(noSplitSplit, Split(Newline, 1).withIndent(indent))
            case _ =>
              val nlColonPolicy = afterClose.map {
                case t: T.Colon => decideNewlinesOnlyBeforeClose(t)
                case _ => NoPolicy
              }
              Seq(
                noSplitSplit,
                Split(Newline, 1).withIndent(indent).withPolicy(nlColonPolicy),
              )
          }
          val argsOpt = if (isAlignFirstParen) lastSyntaxClause else None
          argsOpt.flatMap(getLastTokenOpt).fold(splits) { x =>
            val noSplitIndents = Seq(
              Indent(StateColumn, x, ExpiresOn.Before),
              Indent(-indentLen, x, ExpiresOn.Before),
            )
            splits.map(s => if (s.isNL) s else s.withIndents(noSplitIndents))
          }
        }
        val beforeOpenParenSplits =
          if (!open.is[T.LeftParen]) None
          else if (defn) style.newlines.getBeforeOpenParenDefnSite.map { x =>
            val indent = beforeDefRhs.fold(style.indent.main) { y =>
              val ob = OptionalBraces.at(y)
              style.indent.extraBeforeOpenParenDefnSite +
                (if (ob) style.indent.getSignificant else style.indent.main)
            }
            getSplitsBeforeOpenParen(x, indent, _.beforeOpenParenDefnSite) {
              @tailrec
              def iter(tree: Tree): Option[Member.ParamClause] = tree match {
                case _: Member.ParamClause => tree.parent match {
                    case Some(p) => iter(p)
                    case None => None
                  }
                case p: Tree.WithParamClauses => p.paramClauses.lastOption
                case _ => None
              }
              iter(rightOwner)
            }
          }
          else if (style.dialect.allowSignificantIndentation) style.newlines
            .getBeforeOpenParenCallSite.map { x =>
              val indent = style.indent.getSignificant
              @tailrec
              def findLastCallArgs(t: Member.Apply): Member.ArgClause =
                t.parent match {
                  case Some(p: Member.Apply) => findLastCallArgs(p)
                  case _ => t.argClause
                }
              getSplitsBeforeOpenParen(x, indent, _.beforeOpenParenCallSite) {
                rightOwner.parent.collect { case p: Member.Apply =>
                  findLastCallArgs(p)
                }
              }
            }
          else None
        beforeOpenParenSplits.getOrElse(Seq(baseNoSplit))

      // Defn.{Object, Class, Trait, Enum}
      case FormatToken(
            _: T.KwObject | _: T.KwClass | _: T.KwTrait | _: T.KwEnum,
            _,
            WithTemplateOnLeft(template),
          ) =>
        val policy = Policy ?
          (style.binPack.keepParentConstructors || template.pos.isEmpty) || {
            val expire = templateDerivesOrCurlyOrLastNonTrivial(template).left
            val forceNewlineBeforeExtends = Policy.before(expire, "NLPCTOR") {
              case Decision(FormatToken(_, soft.ExtendsOrDerives(), m), s)
                  if m.rightOwner eq template =>
                s.filter(x => x.isNL && !x.isActiveFor(SplitTag.OnelineWithChain))
            }
            delayedBreakPolicyBefore(expire)(forceNewlineBeforeExtends)
          }
        Seq(Split(Space, 0).withPolicy(policy))

      // DefDef
      case FormatToken(_: T.KwDef, _: T.Ident, _) => Seq(Split(Space, 0))

      case FormatToken(open @ LeftParenOrBracket(), _, _)
          if ft.meta.formatOff &&
            leftOwner.isAny[Member.SyntaxValuesClause, Member.Tuple] =>
        val close = matching(open).left
        def splits(xft: FormatToken, policy: Policy)(implicit l: FileLine) =
          Seq(Split(Provided(xft), 0, policy = policy))
        val policy = Policy.on(close, "(FMT:OFF)", rank = Int.MaxValue) {
          case Decision(xft, _) => splits(xft, NoPolicy)
        }
        splits(ft, policy)

      // Parameter opening for one parameter group. This format works
      // on the WHOLE defnSite (via policies)
      case FormatToken(LeftParenOrBracket(), _, _)
          if style.verticalMultiline.atDefnSite &&
            isParamClauseSite(leftOwner) => verticalMultiline()

      // Term.Apply and friends
      case FormatToken(lp: T.LeftParen, _, LambdaAtSingleArgCallSite(lambda)) =>
        val closeFt = matching(lp)
        val close = closeFt.left
        val newlinePolicy = Policy ? style.danglingParentheses.callSite &&
          decideNewlinesOnlyBeforeClose(close)
        val noSplitMod =
          if (
            style.newlines.alwaysBeforeCurlyLambdaParams ||
            getMustDangleForTrailingCommas(prev(closeFt))
          ) null
          else getNoSplitAfterOpening(ft, commentNL = null)

        def multilineSpaceSplit(implicit fileLine: FileLine): Split = {
          val lambdaLeft: Option[T] =
            matchingOpt(functionExpire(lambda)._1) match {
              case Some(FormatToken(lb: T.LeftBrace, _, _)) => Some(lb)
              case _ => None
            }

          val arrowFt = getFuncArrow(lambda).get
          val lambdaIsABlock = lambdaLeft.contains(arrowFt.right)
          val lambdaToken =
            getOptimalTokenFor(if (lambdaIsABlock) next(arrowFt) else arrowFt)

          val spacePolicy = SingleLineBlock(lambdaToken) ==> {
            Policy ? lambdaIsABlock || delayedBreakPolicy(
              Policy.End == lambdaLeft.getOrElse(close),
            )(newlinePolicy)
          }
          Split(noSplitMod, 0, policy = spacePolicy)
            .withOptimalToken(lambdaToken, killOnFail = true)
        }

        if (noSplitMod == null) Seq(
          Split(Newline, 0, policy = newlinePolicy)
            .withIndent(style.indent.callSite, close, Before),
        )
        else {
          val newlinePenalty = 3 + nestedApplies(leftOwner)
          val noMultiline = style.newlines.beforeCurlyLambdaParams eq
            Newlines.BeforeCurlyLambdaParams.multiline
          Seq(
            if (noMultiline) Split(noSplitMod, 0).withSingleLine(close)
            else multilineSpaceSplit,
            Split(Newline, newlinePenalty, policy = newlinePolicy)
              .withIndent(style.indent.callSite, close, Before),
          )
        }

      case FormatToken(T.LeftParen(), T.RightParen(), _) =>
        val noNL = style.newlines.sourceIgnored || noBreak()
        Seq(Split(NoSplit.orNL(noNL), 0))

      case FormatToken(open @ LeftParenOrBracket(), right, _) if {
            if (isArgClauseSite(leftOwner)) style.binPack.callSiteFor(open) ==
              BinPack.Site.Never
            else style.binPack.defnSiteFor(open) == BinPack.Site.Never &&
            isParamClauseSite(leftOwner)
          } =>
        val afterClose = matching(open)
        val close = afterClose.left
        val beforeClose = prev(afterClose)
        val tupleSite = isTuple(leftOwner)
        val anyDefnSite = isParamClauseSite(leftOwner)
        val defnSite = !tupleSite && anyDefnSite

        val args = getArgs(leftOwner)
        // In long sequence of select/apply, we penalize splitting on
        // parens furthest to the right.
        def leftOwnerIsEnclosed = leftOwner.is[Member.Function]
        val lhsPenalty = leftOwner match {
          case t: Member.SyntaxValuesClause => t.parent match {
              case Some(p: Init) => treeDepth(p.tpe)
              case Some(p: Member.Apply) => treeDepth(p.fun)
              case Some(_: Member.Function) => maxTreeDepth(t.values)
              case _ => 0
            }
          case t => treeDepth(t)
        }

        // XXX: sometimes we have zero args, so multipleArgs != !singleArgument
        val numArgs = args.length
        val singleArgument = numArgs == 1
        val multipleArgs = numArgs > 1
        val notTooManyArgs = multipleArgs && numArgs <= 100

        val isBracket = open.is[T.LeftBracket]
        val bracketCoef = if (isBracket) Constants.BracketPenalty else 1

        val mustDangleForTrailingCommas =
          getMustDangleForTrailingCommas(beforeClose)

        implicit val clauseSiteFlags = ClauseSiteFlags(leftOwner, defnSite)
        implicit val configStyleFlags = clauseSiteFlags.configStyle
        val closeBreak = beforeClose.hasBreak
        val forceConfigStyle = mustForceConfigStyle(ft)
        val onlyConfigStyle = forceConfigStyle ||
          preserveConfigStyle(ft, mustDangleForTrailingCommas || closeBreak)
        val configStyleFlag = configStyleFlags.prefer

        val sourceIgnored = style.newlines.sourceIgnored
        val (onlyArgument, isSingleEnclosedArgument) =
          if (singleArgument) {
            val arg = args(0)
            val maybeEnclosed = arg.parent match {
              case Some(p: Member.SyntaxValuesClause)
                  if lhsPenalty != 0 && !p.is[Member.ArgClause] => p
              case _ => arg
            }
            val isEnclosed = isEnclosedInMatching(maybeEnclosed)
            if (isEnclosed) (maybeEnclosed, true) else (arg, false)
          } else (null, false)

        val nestedPenalty = 1 + nestedApplies(leftOwner) + lhsPenalty

        val indentLen =
          if (anyDefnSite) style.indent.getDefnSite(leftOwner)
          else style.indent.callSite
        val indent = Indent(Num(indentLen), close, ExpiresOn.Before)

        val isBeforeOpenParen =
          if (defnSite) style.newlines.isBeforeOpenParenDefnSite
          else style.newlines.isBeforeOpenParenCallSite
        val optimalFtOpt =
          if (isBeforeOpenParen || !defnSite || isBracket) None
          else defnSiteLastToken(leftOwner)
        val optimalFt: FormatToken =
          getSlbEndOnLeft(optimalFtOpt.getOrElse(afterClose))
        val optimal = optimalFt.left

        val wouldDangle = onlyConfigStyle || mustDangleForTrailingCommas ||
          dangleCloseDelim || closeBreak && beforeClose.left.is[T.Comment]
        val optimalIsComment = optimal.is[T.Comment]

        val newlinePolicy: Policy = Policy ?
          (wouldDangle || optimalIsComment) &&
          decideNewlinesOnlyBeforeClose(close)

        // covers using as well
        val handleImplicit = !tupleSite &&
          (if (onlyConfigStyle) opensConfigStyleImplicitParamList(ft)
           else hasImplicitParamList(rightOwner))

        val rightIsComment = right.is[T.Comment]
        val align = !rightIsComment && alignOpenDelim &&
          (!handleImplicit || style.newlines.forceAfterImplicitParamListModifier)
        val alignTuple = align && tupleSite && !onlyConfigStyle

        val noSplitForNL = !onlyConfigStyle && right.is[T.LeftBrace]
        val skipNoSplit = !noSplitForNL && !alignTuple &&
          (style.newlines.keepBreak(newlines) || {
            if (!handleImplicit) onlyConfigStyle
            else style.newlines.forceBeforeImplicitParamListModifier
          })
        val noSplitMod =
          if (skipNoSplit) null
          else getNoSplitAfterOpening(ft, commentNL = null, spaceOk = !isBracket)

        val keepConfigStyleSplit = !sourceIgnored && configStyleFlag &&
          hasBreak()
        val splitsForAssign =
          if (defnSite || isBracket || keepConfigStyleSplit) None
          else getAssignAtSingleArgCallSite(args).map { assign =>
            val assignToken = assign.rhs match {
              case b: Term.Block => getHead(b)
              case _ => tokens(assign.tokens.find(_.is[T.Equals]).get)
            }
            val breakToken = getOptimalTokenFor(assignToken)
            val newlineAfterAssignDecision = Policy ? newlinePolicy.isEmpty ||
              decideNewlinesOnlyAfterToken(breakToken)
            Seq(
              Split(Newline, nestedPenalty + Constants.ExceedColumnPenalty)
                .withPolicy(newlinePolicy).withIndent(indent),
              Split(NoSplit, nestedPenalty).withSingleLineNoOptimal(breakToken)
                .andPolicy(newlinePolicy & newlineAfterAssignDecision),
            )
          }

        @tailrec
        def isExcludedTree(tree: Tree): Boolean = tree match {
          case t: Init => t.argClauses.nonEmpty
          case t: Term.Apply => t.argClause.nonEmpty
          case t: Term.ApplyType => t.argClause.nonEmpty
          case t: Tree.WithCasesBlock => t.casesBlock.cases.nonEmpty
          case t: Term.New => t.init.argClauses.nonEmpty
          case _: Term.NewAnonymous => true
          case t: Term.AnonymousFunction => isExcludedTree(t.body)
          case _ => false
        }

        val excludeBlocks =
          if (isBracket) {
            val excludeBeg = if (align) getHead(args.last) else ft
            insideBlock[T.LeftBracket](excludeBeg, close)
          } else if (
            multipleArgs ||
            (!isSingleEnclosedArgument || leftOwnerIsEnclosed) &&
            style.newlines.unfold
          ) TokenRanges.empty
          else if (
            style.newlines.fold && {
              isSingleEnclosedArgument ||
              singleArgument && isExcludedTree(onlyArgument)
            }
          )
            if (onlyArgument eq leftOwner) TokenRanges(TokenRange(open, close))
            else parensTuple(getLast(onlyArgument).left)
          else insideBracesBlock(ft, close)

        def singleLine(newlinePenalty: Int)(implicit fileLine: FileLine): Policy =
          if (multipleArgs && (isBracket || excludeBlocks.isEmpty))
            SingleLineBlock(close, noSyntaxNL = true)
          else if (isBracket)
            PenalizeAllNewlines(close, newlinePenalty, penalizeLambdas = false)
          else {
            val penalty =
              if (!multipleArgs) newlinePenalty else Constants.ShouldBeNewline
            policyWithExclude(excludeBlocks, Policy.End.On, Policy.End.On)(
              PenalizeAllNewlines(
                close,
                penalty = penalty,
                penalizeLambdas = multipleArgs,
                noSyntaxNL = multipleArgs,
              ),
            )
          }

        val preferNoSplit = !skipNoSplit && singleArgument &&
          style.newlines.keepBreak(noBreak())
        val oneArgOneLine = newlinePolicy &
          (leftOwner match {
            case t @ (_: Member.SyntaxValuesClause | _: Member.Tuple) =>
              splitOneArgOneLine(close, t)
            case _ => Policy.NoPolicy
          })
        val extraOneArgPerLineIndent =
          if (multipleArgs && style.poorMansTrailingCommasInConfigStyle)
            Indent(Num(2), right, After)
          else Indent.Empty
        val (implicitPenalty, implicitPolicy) =
          if (!handleImplicit) (2, Policy.NoPolicy)
          else (0, decideNewlinesOnlyAfterToken(right))

        val splitsNoNL =
          if (noSplitMod == null) Seq.empty
          else if (onlyConfigStyle) Seq(
            Split(noSplitMod, 0).withPolicy(oneArgOneLine & implicitPolicy)
              .withOptimalToken(right, killOnFail = true)
              .withIndents(extraOneArgPerLineIndent, indent),
          )
          else {
            val useOneArgPerLineSplit = (notTooManyArgs && align) ||
              (handleImplicit &&
                style.newlines.notBeforeImplicitParamListModifier)
            val slbSplit =
              if (mustDangleForTrailingCommas) Split.ignored
              else {
                def getSlb(implicit l: FileLine) = SingleLineBlock(
                  close,
                  exclude = excludeBlocks,
                  noSyntaxNL = multipleArgs,
                )
                val needDifferentFromOneArgPerLine = newlinePolicy.nonEmpty &&
                  handleImplicit && useOneArgPerLineSplit
                val noSplitPolicy =
                  if (needDifferentFromOneArgPerLine) getSlb
                  else if (preferNoSplit && splitsForAssign.isEmpty)
                    singleLine(2)
                  else if (
                    wouldDangle || optimalIsComment && isBracket ||
                    sourceIgnored &&
                    configStyleFlag && !isSingleEnclosedArgument
                  ) getSlb
                  else if (splitsForAssign.isDefined) singleLine(3)
                  else singleLine(10)
                val kof = (style.newlines.keep || excludeBlocks.isEmpty) &&
                  needDifferentFromOneArgPerLine
                val noOptimal = style.newlines.keep && !useOneArgPerLineSplit ||
                  singleArgument && !excludeBlocks.isEmpty &&
                  excludeBlocks.ranges.forall(_.lt.is[T.LeftParen])
                val okIndent = rightIsComment || handleImplicit
                Split(noSplitMod, 0, policy = noSplitPolicy).withOptimalToken(
                  optimal,
                  ignore = noOptimal,
                  killOnFail = kof,
                ).withIndent(indent, ignore = !okIndent)
              }
            val oneArgPerLineSplit =
              if (useOneArgPerLineSplit)
                Split(noSplitMod, (implicitPenalty + lhsPenalty) * bracketCoef)
                  .withPolicy(oneArgOneLine & implicitPolicy).withIndents(
                    if (align) getOpenParenAlignIndents(close) else Seq(indent),
                  )
              else Split.ignored
            Seq(slbSplit, oneArgPerLineSplit)
          }

        val splitsNL =
          if (
            splitsNoNL.nonEmpty &&
            (alignTuple ||
              !(onlyConfigStyle || multipleArgs || splitsForAssign.isEmpty))
          ) Seq.empty
          else {
            val cost =
              if (forceConfigStyle) if (splitsNoNL.isEmpty) 0 else 1
              else (if (preferNoSplit) Constants.ExceedColumnPenalty else 0) +
                bracketCoef * (nestedPenalty + (if (multipleArgs) 2 else 0))
            val split =
              if (multipleArgs) Split(Newline, cost, policy = oneArgOneLine)
                .withIndent(extraOneArgPerLineIndent)
              else {
                val noConfigStyle = noSplitForNL || newlinePolicy.isEmpty ||
                  !configStyleFlag
                val policy =
                  if (!noSplitForNL) newlinePolicy
                  else decideNewlinesOnlyBeforeToken(matching(right).left)
                Split(NoSplit.orNL(noSplitForNL), cost, policy = policy)
                  .andPolicy(Policy ? noConfigStyle && singleLine(4)).andPolicy(
                    Policy ? singleArgument && asInfixApp(args.head)
                      .map(InfixSplits(_, ft).nlPolicy),
                  )
              }
            Seq(split.withIndent(indent, ignore = !split.isNL))
          }

        splitsNoNL ++ splitsNL ++ splitsForAssign.getOrElse(Seq.empty)

      case FormatToken(open @ LeftParenOrBracket(), right, _)
          if style.binPack.defnSiteFor(open) != BinPack.Site.Never &&
            isParamClauseSite(leftOwner) =>
        val closeFt = matching(open)
        val close = closeFt.left
        val noSplitMod = Space(style.spaces.inParentheses)
        if (close eq right) Seq(Split(noSplitMod, 0))
        else {
          implicit val clauseSiteFlags = ClauseSiteFlags.atDefnSite(leftOwner)

          val isBracket = open.is[T.LeftBracket]
          val bracketPenalty =
            if (isBracket) Some(Constants.BracketPenalty) else None
          val penalizeBrackets = bracketPenalty
            .map(p => PenalizeAllNewlines(close, p + 3))

          val binpack = style.binPack.defnSiteFor(isBracket)
          val firstArg = optimizationEntities.argument
          val nextComma = firstArg.flatMap { x =>
            val ok = isSeqMulti(getArgs(leftOwner))
            if (ok) findFirstOnRight[T.Comma](getLast(x), close) else None
          }
          val nextCommaOneline = if (binpack.isOneline) nextComma else None

          val flags = getBinpackDefnSiteFlags(ft, prev(closeFt))
          val (nlOnly, nlCloseOnOpen) = flags.nlOpenClose()
          val noNLPolicy = flags.noNLPolicy
          val slbOrNL = nlOnly || noNLPolicy == null

          val rightIsComment = right.is[T.Comment]
          def getNoSplit(slbEnd: Option[T])(implicit fileLine: FileLine) = {
            val mod = if (rightIsComment) Space.orNL(noBreak()) else noSplitMod
            slbEnd.fold(Split(mod, 0)) { x =>
              Split(mod, 0).withOptimalToken(x, killOnFail = true)
                .withPolicy(SingleLineBlock(x, okSLC = true, noSyntaxNL = true))
            }
          }

          val noSplit =
            if (nlOnly) Split.ignored
            else if (slbOrNL) getNoSplit(Some(close))
            else {
              val opensPolicy = bracketPenalty.map { p =>
                Policy.before(close, "PENBP[") {
                  case Decision(ftd @ FormatToken(o: T.LeftBracket, _, m), s)
                      if isParamClauseSite(m.leftOwner) &&
                        styleMap.at(o).binPack.bracketDefnSite
                          .exists(_ != BinPack.Site.Never) =>
                    if (isRightCommentThenBreak(ftd)) s
                    else s.map(x => if (x.isNL) x.withPenalty(p) else x)
                }
              }
              getNoSplit(nextComma.map(endOfSingleLineBlock))
                .andPolicy((opensPolicy | penalizeBrackets) & noNLPolicy())
            }

          def nlCost = bracketPenalty.getOrElse(1)
          val nlMod = Space.orNL(rightIsComment && nlOnly && noBreak())
          def getDanglePolicy(implicit fileLine: FileLine) =
            decideNewlinesOnlyBeforeClose(close)
          val nlPolicy = nlCloseOnOpen match {
            case NlClosedOnOpen.Cfg => splitOneArgOneLine(close, leftOwner) ==>
                getDanglePolicy
            case NlClosedOnOpen.Yes => getDanglePolicy
            case NlClosedOnOpen.No => NoPolicy
          }
          val nlOnelinePolicy = nextCommaOneline
            .map(x => splitOneArgPerLineAfterCommaOnBreak(x.right))

          val (indentLen, bpIndentLen) = style.indent
            .getBinPackDefnSites(leftOwner)
          val nlIndentLen =
            if (nlCloseOnOpen eq NlClosedOnOpen.Cfg) indentLen else bpIndentLen

          def noSplitIndents =
            if (alignOpenDelim) getOpenParenAlignIndents(close)
            else Seq(Indent(bpIndentLen, close, Before))

          Seq(
            noSplit.withIndents(noSplitIndents),
            Split(nlMod, if (slbOrNL) 0 else nlCost)
              .withIndent(nlIndentLen, close, Before)
              .withPolicy(nlPolicy & penalizeBrackets & nlOnelinePolicy),
          )
        }

      case FormatToken(open @ LeftParenOrBracket(), right, _)
          if style.binPack.callSiteFor(open) != BinPack.Site.Never &&
            isArgClauseSite(leftOwner) =>
        val closeFt = matching(open)
        val close = closeFt.left
        val beforeClose = prev(closeFt)
        val isBracket = open.is[T.LeftBracket]
        val bracketPenalty = if (isBracket) Constants.BracketPenalty else 1

        val args = getArgs(leftOwner)
        val isSingleArg = isSeqSingle(args)
        val firstArg = args.headOption
        val singleArgAsInfix =
          if (isSingleArg) firstArg.flatMap(asInfixApp) else None

        implicit val clauseSiteFlags = ClauseSiteFlags.atCallSite(leftOwner)
        val flags = getBinpackCallSiteFlags(ft, beforeClose)
        val (nlOpen, nlCloseOnOpen) = flags.nlOpenClose()
        val singleLineOnly = style.binPack.literalsSingleLine &&
          flags.literalArgList
        val nlOnly = nlOpen && !singleLineOnly

        val binPack = style.binPack.callSiteFor(open)
        val oneline = binPack.isOneline
        val afterFirstArgOneline =
          if (oneline) firstArg.map(tokenAfter) else None

        val (indentLen, bpIndentLen) = style.indent.getBinPackCallSites

        val exclude =
          if (!isBracket) insideBracesBlock(ft, close)
          else insideBlock[T.LeftBracket](ft, close)
        val sjsOneline = !isBracket && binPack == BinPack.Site.OnelineSjs
        val sjsExclude = exclude.getIf(sjsOneline)

        val newlinesPenalty = 3 + indentLen * bracketPenalty
        val penalizeNewlinesPolicy =
          policyWithExclude(exclude, Policy.End.Before, Policy.End.On) {
            val expire =
              findToken(beforeClose, prev)(x => !RightParenOrBracket(x.left))
            new PenalizeAllNewlines(Policy.End == expire.left, newlinesPenalty)
          }

        val (onelineCurryToken, onelinePolicy) = afterFirstArgOneline
          .map(BinPackOneline.getPolicy(true, sjsExclude))
          .getOrElse((None, NoPolicy))

        def baseNoSplit(implicit fileLine: FileLine) =
          Split(Space(style.spaces.inParentheses), 0)
        val noNLPolicy = flags.noNLPolicy

        val noSplit =
          if (nlOnly) Split.ignored
          else if (singleLineOnly || noNLPolicy == null) baseNoSplit
            .withSingleLine(
              close,
              exclude = sjsExclude,
              noSyntaxNL = true,
              killOnFail = Some(!dangleCloseDelim || sjsExclude.isEmpty),
            )
          else {
            def noSingleArgIndents = oneline || singleArgAsInfix.isDefined ||
              !style.binPack.indentCallSiteSingleArg ||
              !isBracket && getAssignAtSingleArgCallSite(args).isDefined
            val indent = Indent(bpIndentLen, close, Before)
            val noSplitIndents =
              if (isSingleArg && noSingleArgIndents) Nil
              else if (style.binPack.indentCallSiteOnce) {
                @tailrec
                def iter(tree: Tree): Option[T] = tree.parent match {
                  case Some(p: Term.Select) => iter(p)
                  case Some(p) if isArgClauseSite(p) => Some(getIndentTrigger(p))
                  case _ => None
                }
                val trigger = leftOwner.parent.flatMap(iter)
                Seq(trigger.fold(indent)(x => Indent.before(indent, x)))
              } else if (alignOpenDelim) getOpenParenAlignIndents(close)
              else Seq(indent)

            val nextComma =
              if (firstArg.isEmpty) None
              else if (!oneline) tokens.findTokenEx(ft) { xft =>
                xft.right match {
                  case `close` | _: T.RightBrace | _: T.RightArrow => null
                  case x: T.Comma => Right(x)
                  case x: T.LeftBrace => Left(matching(x))
                  case _ => Left(next(xft))
                }
              }.toOption
              else if (isSingleArg) None
              else afterFirstArgOneline.map(_.right)
            val opt = nextComma
              .getOrElse(endOfSingleLineBlock(next(beforeClose)))

            val slbArg = oneline && !noSplitIndents.exists(_.hasStateColumn)
            val slbPolicy: Policy = (if (slbArg) nextComma else None).map {
              SingleLineBlock(_, noSyntaxNL = true, exclude = sjsExclude)
            }
            val noSplitPolicy = slbPolicy ==> onelinePolicy &
              penalizeNewlinesPolicy
            val indentPolicy = Policy ? noSplitIndents.isEmpty || {
              def unindentPolicy = Policy ? (isSingleArg || sjsOneline) &&
                unindentAtExclude(exclude, Num(-bpIndentLen))
              def indentOncePolicy =
                Policy ? style.binPack.indentCallSiteOnce && {
                  val trigger = getIndentTrigger(leftOwner)
                  Policy.on(close, prefix = "IND1") {
                    case Decision(FormatToken(LeftParenOrBracket(), _, m), s)
                        if isArgClauseSite(m.leftOwner) =>
                      s.map(x => if (x.isNL) x else x.switch(trigger, false))
                  }
                }
              unindentPolicy & indentOncePolicy
            }
            val optLite = style.newlines.keep && preferConfigStyle &&
              !isSingleArg
            val kof = style.newlines.keep && preferConfigStyle && isSingleArg &&
              !dangleCloseDelim && singleArgAsInfix.isEmpty
            baseNoSplit
              .withOptimalToken(opt, recurseOnly = optLite, killOnFail = kof)
              .withPolicy(noSplitPolicy & indentPolicy & noNLPolicy())
              .withIndents(noSplitIndents)
          }

        val nlClosedOnOpenEffective = {
          val nlClosedOnOpenOk = onelineCurryToken.forall(x =>
            if (x.is[T.Dot]) onelinePolicy.nonEmpty else flags.scalaJsStyle,
          )
          val res =
            if (nlClosedOnOpenOk) nlCloseOnOpen
            else if (preferConfigStyle) NlClosedOnOpen.Cfg
            else NlClosedOnOpen.Yes
          res match {
            case NlClosedOnOpen.Cfg if styleMap.forcedBinPack(leftOwner) =>
              NlClosedOnOpen.Yes
            case x => x
          }
        }

        val nlPolicy: Policy = {
          def newlineBeforeClose(implicit fileLine: FileLine) =
            decideNewlinesOnlyBeforeClose(close)
          nlClosedOnOpenEffective match {
            case NlClosedOnOpen.No => onelinePolicy
            case NlClosedOnOpen.Cfg => splitOneArgOneLine(close, leftOwner) ==>
                newlineBeforeClose
            case _ => newlineBeforeClose & onelinePolicy
          }
        }

        val nlIndentLen =
          if (nlClosedOnOpenEffective eq NlClosedOnOpen.Cfg) indentLen
          else bpIndentLen
        val nlMod =
          if (nlOnly && noBreak() && right.is[T.Comment]) Space
          else NewlineT(alt = if (singleLineOnly) Some(NoSplit) else None)
        val nlSplit = Split(nlMod, bracketPenalty * (if (oneline) 4 else 2))
          .withIndent(nlIndentLen, close, Before)
          .withSingleLineNoOptimal(close, ignore = !singleLineOnly).andPolicy(
            Policy ? singleLineOnly || nlPolicy & penalizeNewlinesPolicy,
          ).andPolicy(singleArgAsInfix.map(InfixSplits(_, ft).nlPolicy))
        Seq(noSplit, nlSplit)

      // Closing def site ): ReturnType
      case FormatToken(left, _: T.Colon, ColonDeclTpeRight(returnType))
          if style.newlines.sometimesBeforeColonInMethodReturnType ||
            left.is[T.Comment] && hasBreak() =>
        val expireFt = getLastNonTrivial(returnType)
        val expire = expireFt.left
        val sameLineSplit = Space(endsWithSymbolIdent(left))
        val bopSplits = style.newlines.getBeforeOpenParenDefnSite.map { x =>
          val ob = OptionalBraces.at(nextAfterNonComment(expireFt))
          def extraIfBody = style.indent.extraBeforeOpenParenDefnSite
          val indent =
            if (ob) style.indent.getSignificant + extraIfBody
            else style.indent.main +
              (if (defDefBody(rightOwner).isEmpty) 0 else extraIfBody)
          Seq(
            Split(sameLineSplit, 0).onlyIf(noBreak() || x.ne(Newlines.keep))
              .withSingleLine(expire),
            Split(Newline, 1).withIndent(indent, expire, After),
          )
        }
        bopSplits.getOrElse {
          def penalizeNewlines(extra: Int)(implicit fileLine: FileLine) =
            PenalizeAllNewlines(expire, Constants.BracketPenalty + extra)
          val indent = style.indent.getDefnSite(leftOwner)
          if (style.newlines.keepBreak(hasBreak())) Seq(
            Split(Newline, 1).withIndent(indent, expire, After)
              .withPolicy(penalizeNewlines(1)),
          )
          else {
            val nlPenalty = 2 + treeDepth(returnType)
            Seq(
              Split(sameLineSplit, 0).withPolicy(penalizeNewlines(0)),
              // Spark style guide allows this:
              // https://github.com/databricks/scala-style-guide#indent
              Split(Newline, Constants.SparkColonNewline + nlPenalty)
                .withIndent(indent, expire, After)
                .withPolicy(penalizeNewlines(nlPenalty)),
            )
          }
        }
      case FormatToken(_: T.Colon, _, ColonDeclTpeLeft(returnType))
          if style.newlines.avoidInResultType =>
        val expire = getLastNonTrivialToken(returnType)
        val policy = PenalizeAllNewlines(expire, Constants.ShouldBeNewline)
        Seq(
          Split(Space, 0).withPolicy(policy)
            .withOptimalToken(expire, killOnFail = false),
        )

      case FormatToken(T.LeftParen(), T.LeftBrace(), _) => Seq(Split(NoSplit, 0))

      case FormatToken(_, T.LeftBrace(), _) if isXmlBrace(rightOwner) =>
        withIndentOnXmlSpliceStart(ft, Seq(Split(NoSplit, 0)))

      case FormatToken(T.RightBrace(), _, _) if isXmlBrace(leftOwner) =>
        Seq(Split(NoSplit, 0))
      // non-statement starting curly brace
      case FormatToken(_: T.Comma, open: T.LeftBrace, _)
          if !style.poorMansTrailingCommasInConfigStyle &&
            isArgClauseSite(leftOwner) =>
        val close = matching(open).left
        val binPackIsEnabled = style.binPack.callSiteFor(leftOwner) !=
          BinPack.Site.Never
        val useSpace = !style.newlines.keepBreak(newlines)
        val singleSplit =
          if (!binPackIsEnabled) Split(Space.orNL(useSpace), 0)
          else Split(Space, 0).onlyIf(useSpace).withSingleLine(close)
        val otherSplits = rightOwner match {
          case _: Term.PartialFunction | Term
                .Block(List(_: Term.FunctionTerm | _: Term.PartialFunction)) =>
            Seq(Split(Newline, 0))
          case _ =>
            val breakAfter =
              endOfSingleLineBlock(next(nextNonCommentSameLine(ft)))
            val multiLine = decideNewlinesOnlyAfterToken(breakAfter) ==>
              decideNewlinesOnlyBeforeClose(close)
            Seq(
              Split(Newline, 0).withSingleLine(close),
              Split(Space, 1, policy = multiLine),
            )
        }
        val oneArgPerLineSplits =
          if (binPackIsEnabled) otherSplits
            .map(_.preActivateFor(SplitTag.OneArgPerLine))
          else otherSplits.map(_.onlyFor(SplitTag.OneArgPerLine))
        singleSplit +: oneArgPerLineSplits

      case FormatToken(
            _: T.MacroSplice | _: T.MacroQuote,
            _: T.LeftBrace | _: T.LeftBracket,
            _,
          ) => Seq(Split(NoSplit, 0))
      case FormatToken(_: T.KwMatch, _, _) if !leftOwner.is[Term.Name] => // exclude end marker
        val indentLen = style.indent.matchSite.fold(0)(_ - style.indent.main)
        def expire = getLastNonTrivialToken(leftOwner) // should rbrace
        Seq(Split(Space, 0).withIndent(indentLen, expire, ExpiresOn.Before))
      case FormatToken(_, lb: T.LeftBrace, _) if ! { // partial initial expr
            @tailrec
            def startsInfix(ai: Term.ApplyInfix): Boolean = ai.parent match {
              case Some(_: Term.ArgClause) => false
              case Some(p: Term.ApplyInfix) => startsInfix(p)
              case _ => true
            }
            val roPos = rightOwner.pos
            isTokenHeadOrBefore(lb, roPos) && rightOwner.parent.exists {
              case p: Term.ApplyInfix => // exclude start of infix
                startsInfix(p)
              case _: Term.ArgClause => false
              case p => isTokenHeadOrBefore(lb, p) && matchingOpt(lb)
                  .exists(x => isTokenLastOrAfter(x.left, roPos))
            }
          } =>
        val slbParensSplit =
          if (!initStyle.rewrite.bracesToParensForOneLineApply) None
          else RedundantBraces.noSplitForParensOnRightBrace(matching(lb))
            .map { rbft =>
              // copy logic from `( ...`, binpack=never, defining `slbSplit`
              val isBeforeOpenParen = style.newlines.isBeforeOpenParenCallSite
              val optimal: T =
                if (isBeforeOpenParen) rbft.left else endOfSingleLineBlock(rbft)
              Split(NoSplit, 0).withSingleLine(optimal)
            }
        Seq(slbParensSplit.getOrElse(Split.ignored), Split(Space, 0))

      // Delim
      case FormatToken(left, _: T.Comma, _)
          if !left.is[T.Comment] || noBreak() => Seq(Split(NoSplit, 0))
      // These are mostly filtered out/modified by policies.
      case FormatToken(lc: T.Comma, _: T.Comment, m) =>
        val nextFt = next(ft)
        if (hasBlankLine) Seq(Split(Newline2x, 0))
        else if (nextFt.hasBreak || m.right.hasNL)
          Seq(Split(Space.orNL(newlines), 0))
        else if (noBreak()) {
          val endFt = nextNonCommentSameLine(nextFt)
          val useSpaceOnly = endFt.hasBreak ||
            rightIsCloseDelimToAddTrailingComma(lc, endFt)
          Seq(Split(Space, 0), Split(useSpaceOnly, 1)(Newline))
        } else if (
          !style.comments.willWrap &&
          rightIsCloseDelimToAddTrailingComma(lc, nextNonComment(nextFt))
        ) Seq(Split(Space, 0), Split(Newline, 1))
        else Seq(Split(Newline, 0))
      case FormatToken(_: T.Comma, right, _) if !leftOwner.is[Template] =>
        def forBinPack(binPack: BinPack.Site, callSite: Boolean) =
          if (binPack eq BinPack.Site.Never) None
          else optimizationEntities.argument.map { nextArg =>
            val lastFT = getLast(nextArg)
            val lastTok = lastFT.left
            val oneline = binPack.isOneline
            val afterNextArg = nextNonComment(lastFT)
            val nextCommaOrParen = afterNextArg.right match {
              case _: T.Comma | _: T.RightParen | _: T.RightBracket =>
                Some(afterNextArg)
              case _ => None
            }
            def getEndOfResultType(cp: FormatToken) =
              if (
                cp.right.is[T.Comma] || !style.newlines.avoidInResultType ||
                style.newlines.sometimesBeforeColonInMethodReturnType
              ) None
              else {
                val nft = findToken(next(cp), next) { x =>
                  x.right match {
                    case _: T.Comment => x.hasBreak
                    case _: T.RightParen | _: T.RightBracket => false
                    case _ => true
                  }
                }
                if (nft.right.is[T.Colon]) colonDeclType(nft.meta.rightOwner)
                  .flatMap(getLastNonTrivialOpt) // could be empty tree
                else None
              }

            val sjsExclude =
              if (binPack ne BinPack.Site.OnelineSjs) TokenRanges.empty
              else insideBracesBlock(ft, lastTok)
            val onelinePolicy = Policy ? oneline &&
              nextCommaOrParen
                .map(BinPackOneline.getPolicy(callSite, sjsExclude)(_)._2)

            val indentOncePolicy = Policy ?
              (callSite && style.binPack.indentCallSiteOnce) && {
                val trigger = getIndentTrigger(leftOwner)
                Policy.on(lastTok, prefix = "IND1") {
                  case Decision(FormatToken(LeftParenOrBracket(), _, m), s)
                      if isArgClauseSite(m.leftOwner) =>
                    s.map(x => if (x.isNL) x else x.switch(trigger, true))
                }
              }
            val nlSplit =
              Split(Newline, 1, policy = onelinePolicy & indentOncePolicy)
            val noSplit =
              if (style.newlines.keepBreak(newlines)) Split.ignored
              else {
                val end = endOfSingleLineBlock(
                  nextCommaOrParen.flatMap(getEndOfResultType).getOrElse(lastFT),
                )
                val slbPolicy =
                  SingleLineBlock(end, exclude = sjsExclude, noSyntaxNL = true)
                Split(Space, 0, policy = slbPolicy ==> onelinePolicy)
                  .withOptimalToken(end, killOnFail = sjsExclude.isEmpty)
              }
            Seq(noSplit, nlSplit)
          }

        def defaultSplits(implicit fileLine: FileLine) =
          Seq(Split(Space, 0), Split(Newline, 1))

        def altSplits = leftOwner match {
          case _: Defn.Val | _: Defn.Var => Seq(
              Split(Space, 0),
              Split(Newline, 1)
                .withIndent(style.indent.getDefnSite(leftOwner), right, After),
            )
          case _: Defn.RepeatedEnumCase if {
                if (!style.newlines.sourceIgnored) hasBreak()
                else style.newlines.unfold
              } => Seq(Split(Newline, 0))
          case _: ImportExportStat => Seq(
              Split(Space, 0),
              Split(Newline, 1)
                .withIndent(style.indent.main, right, ExpiresOn.After),
            )
          case _ => defaultSplits
        }

        style.binPack.siteFor(leftOwner).fold(altSplits) {
          case (bp, isCallSite) => forBinPack(bp, isCallSite)
              .getOrElse(defaultSplits)
        }

      case FormatToken(_, _: T.Semicolon, _) => Seq(Split(NoSplit, 0))
      case FormatToken(_: T.KwReturn, r, _) =>
        val mod =
          if (hasBlankLine) Newline2x
          else leftOwner match {
            case Term.Return(unit: Lit.Unit) if unit.tokens.isEmpty =>
              if (r.is[T.RightParen]) Space(style.spaces.inParentheses)
              else Newline //  force blank line for Unit "return".
            case _ => Space
          }
        Seq(Split(mod, 0))

      /*
       * Type Bounds
       */

      case FormatToken(_, _: T.Colon, _) if rightOwner.is[Type.Param] =>
        val tp = rightOwner.asInstanceOf[Type.Param]
        def noNLMod = Space(style.spaces.beforeContextBoundColon match {
          case Spaces.BeforeContextBound.Always => true
          case Spaces.BeforeContextBound.IfMultipleBounds => 1 <
              tp.cbounds.length +
              tp.vbounds.length + tp.tbounds.lo.size + tp.tbounds.hi.size
          case _ => false
        })
        getSplitsForTypeBounds(noNLMod, tp, _.cbounds)
      case FormatToken(_, _: T.Viewbound, _) if rightOwner.is[Type.Param] =>
        val tp = rightOwner.asInstanceOf[Type.Param]
        getSplitsForTypeBounds(Space, tp, _.vbounds)
      /* Type bounds in type definitions and declarations such as:
       * type `Tuple <: Alpha & Beta = Another` or `Tuple <: Alpha & Beta`
       */
      case FormatToken(_, bound @ (_: T.Subtype | _: T.Supertype), _)
          if rightOwner.is[Type.Bounds] && rightOwner.parent.isDefined =>
        val tbounds = rightOwner.asInstanceOf[Type.Bounds]
        val boundOpt = bound match {
          case _: T.Subtype => tbounds.hi
          case _: T.Supertype => tbounds.lo
          case _ => None
        }
        val boundEnd = boundOpt.map(getLastNonTrivialToken)
        val typeOwner = rightOwner.parent.get
        getSplitsForTypeBounds(Space, typeOwner, boundEnd)

      case FormatToken(left, _: T.Colon, _) =>
        val mod = left match {
          case ident: T.Ident => identModification(ident)
          case _ => NoSplit
        }
        Seq(Split(mod, 0))

      case FormatToken(_, _: T.Dot, _)
          if !style.newlines.keep && rightOwner.is[Term.Select] &&
            findTreeWithParent(rightOwner) {
              case _: Term.ArgClause => None
              case _: Type.Select | _: Importer | _: Pkg => Some(true)
              case _: Term.Select | _: Member.Apply => None
              case _ => Some(false)
            }.isDefined => Seq(Split(NoSplit, 0))

      case FormatToken(left, r: T.Dot, GetSelectLike.OnRight(thisSelect)) =>
        val enclosed = style.encloseSelectChains
        val (expireTree, nextSelect) =
          findLastApplyAndNextSelect(rightOwner, enclosed)
        val (prevSelect, prevApply) =
          findPrevSelectAndApply(thisSelect.qual, enclosed)
        val afterComment = left.is[T.Comment]
        val nlOnly =
          style.newlines.sourceIgnored && afterComment && hasBreak() ||
            prevApply.exists(x => getHeadToken(x.argClause).is[T.Colon])
        val expire = getLastEnclosedToken(expireTree)
        val indentLen = style.indent.main

        def checkFewerBraces(tree: Tree) = tree match {
          case p: Term.Apply => isFewerBraces(p)
          case p: Term.Match => getHead(p.casesBlock).meta.leftOwner ne
              p.casesBlock
          case p: Term.NewAnonymous => getHeadOpt(p.templ.body)
              .exists(_.left.is[T.Colon])
          case _ => false
        }
        val nextDotIfSig = nextSelect.flatMap { ns =>
          val ok = checkFewerBraces(ns.qual)
          if (ok) Some(tokenBefore(ns.nameToken)) else None
        }
        def forcedBreakOnNextDotPolicy = nextSelect.map { selectLike =>
          val tree = selectLike.tree
          Policy.before(selectLike.nameToken, "NEXTSELFNL") {
            case d @ Decision(FormatToken(_, _: T.Dot, m), _)
                if m.rightOwner eq tree => d.onlyNewlinesWithoutFallback
          }
        }

        def breakOnNextDot: Policy = nextSelect.map { selectLike =>
          val tree = selectLike.tree
          Policy.before(selectLike.nameToken, "NEXTSEL2NL") {
            case Decision(FormatToken(_, _: T.Dot, m), s)
                if m.rightOwner eq tree =>
              val filtered = s.flatMap { x =>
                val y = x.activateFor(SplitTag.SelectChainSecondNL)
                if (y.isActive) Some(y) else None
              }
              if (filtered.isEmpty) Seq.empty
              else {
                val minCost = math
                  .max(0, filtered.map(_.costWithPenalty).min - 1)
                filtered.map { x =>
                  val p = x.policy.filter(!_.isInstanceOf[PenalizeAllNewlines])
                  x.copy(penalty = x.costWithPenalty - minCost, policy = p)
                }
              }
          }
        }

        def getSlbEnd() = {
          val nft = nextNonCommentSameLineAfter(ft)
          val eft = if (nft.noBreak) nextNonCommentSameLineAfter(nft) else nft
          endOfSingleLineBlock(eft)
        }

        val ftAfterRight = tokens(ft, 2)
        def modSpace = Space(afterComment)
        val baseSplits = style.newlines.getSelectChains match {
          case Newlines.classic =>
            def getNlMod = {
              val endSelect = nextDotIfSig.fold {
                nextSelect.fold {
                  val ko = style.getFewerBraces() ==
                    Indents.FewerBraces.always && checkFewerBraces(expireTree)
                  if (ko) None else Some(expire)
                }(ns => Some(getLastNonTrivialToken(ns.qual)))
              } { nd =>
                val ok = style.getFewerBraces() == Indents.FewerBraces.never
                if (ok) Some(nd.left) else None
              }
              val altIndent = endSelect.map(Indent(-indentLen, _, After))
              NewlineT(alt = Some(ModExt(modSpace).withIndentOpt(altIndent)))
            }

            val prevChain = inSelectChain(prevSelect, thisSelect, expireTree)
            if (canStartSelectChain(thisSelect, nextSelect, expireTree)) {
              val chainExpire =
                if (nextSelect.isEmpty) thisSelect.nameToken else expire
              val nestedPenalty = nestedSelect(rightOwner) +
                nestedApplies(leftOwner)
              // This policy will apply to both the space and newline splits, otherwise
              // the newline is too cheap even it doesn't actually prevent other newlines.
              val penalizeBreaks = PenalizeAllNewlines(chainExpire, 2)
              val newlinePolicy = breakOnNextDot & penalizeBreaks
              val ignoreNoSplit = nlOnly ||
                hasBreak &&
                (afterComment || style.optIn.breakChainOnFirstMethodDot)
              val chainLengthPenalty =
                if (
                  style.newlines.penalizeSingleSelectMultiArgList &&
                  nextSelect.isEmpty
                )
                  // penalize by the number of arguments in the rhs open apply.
                  // I know, it's a bit arbitrary, but my manual experiments seem
                  // to show that it produces OK output. The key insight is that
                  // many arguments on the same line can be hard to read. By not
                  // putting a newline before the dot, we force the argument list
                  // to break into multiple lines.
                  ftAfterRight.meta.rightOwner match {
                    case Member.ArgClause(v) => math.max(0, v.length - 1)
                    case _ => 0
                  }
                else 0
              // when the flag is on, penalize break, to avoid idempotence issues;
              // otherwise, after the break is chosen, the flag prohibits nosplit
              val nlBaseCost =
                if (style.optIn.breakChainOnFirstMethodDot) 3 else 2
              val nlCost = nlBaseCost + nestedPenalty + chainLengthPenalty
              val nlMod = getNlMod
              val legacySplit = Split(!prevChain, 1) { // must come first, for backwards compat
                if (style.optIn.breaksInsideChains) Newline
                  .orMod(hasBreak(), modSpace)
                else nlMod
              }.withPolicy(newlinePolicy).onlyFor(SplitTag.SelectChainSecondNL)
              val slbSplit =
                if (ignoreNoSplit) Split.ignored
                else {
                  val noSplit = Split(modSpace, 0)
                  if (prevChain) noSplit
                  else chainExpire match { // allow newlines in final {} block
                    case x: T.RightBrace => noSplit
                        .withSingleLine(matching(x).left, noSyntaxNL = true)
                    case x => noSplit
                        .withSingleLineNoOptimal(x, noSyntaxNL = true)
                  }
                }.andPolicy(penalizeBreaks)
              val nlSplit = Split(if (ignoreNoSplit) Newline else nlMod, nlCost)
                .withPolicy(newlinePolicy)
              Seq(legacySplit, slbSplit, nlSplit)
            } else {
              val doBreak = nlOnly || afterComment && hasBreak
              Seq(
                Split(!prevChain, 1) {
                  if (style.optIn.breaksInsideChains) Newline
                    .orMod(hasBreak(), modSpace)
                  else Newline.orMod(doBreak, getNlMod)
                }.withPolicy(breakOnNextDot)
                  .onlyFor(SplitTag.SelectChainSecondNL),
                Split(Newline.orMod(doBreak, modSpace), 0),
              )
            }

          case Newlines.keep =>
            if (hasBreak()) Seq(Split(Newline, 0))
            else if (hasBreakAfterRightBeforeNonComment(ft))
              Seq(Split(modSpace, 0).withPolicy(
                decideNewlinesOnlyAfterClose(Split(Newline, 0))(r),
                ignore = next(ft).right.is[T.Comment],
              ))
            else Seq(
              Split(modSpace, 0),
              Split(Newline, 1).onlyFor(SplitTag.SelectChainBinPackNL),
            )

          case Newlines.unfold =>
            val nlCost = if (nlOnly) 0 else 1
            if (prevSelect.isEmpty && nextSelect.isEmpty) Seq(
              Split(nlOnly, 0)(modSpace).withSingleLine(getSlbEnd()),
              Split(Newline, nlCost),
            )
            else Seq(
              Split(nlOnly, 0)(modSpace)
                .withSingleLine(expire, noSyntaxNL = true),
              Split(NewlineT(alt = Some(modSpace)), nlCost)
                .withPolicy(forcedBreakOnNextDotPolicy),
            )

          case Newlines.fold =>
            val nlCost = if (nlOnly) 0 else 1
            def nlSplitBase(implicit fileLine: FileLine) =
              Split(NewlineT(alt = Some(modSpace)), nlCost)
            if (nextDotIfSig.isEmpty) {
              val noSplit =
                if (nlOnly) Split.ignored
                else {
                  val end = nextSelect
                    .fold(expire)(x => getLastNonTrivialToken(x.qual))
                  val exclude = insideBracesBlock(ft, end, parensToo = true)
                  Split(modSpace, 0).withSingleLine(end, exclude = exclude)
                }
              Seq(noSplit, nlSplitBase)
            } else {
              val policy: Policy = forcedBreakOnNextDotPolicy
              val noSplit = Split(nlOnly, 0)(modSpace).withPolicy(policy)
              Seq(noSplit, nlSplitBase.withPolicy(policy))
            }
        }

        val delayedBreakPolicyOpt = nextSelect.map { selectLike =>
          val tree = selectLike.tree
          Policy.before(selectLike.nameToken, "NEXTSEL1NL") {
            case Decision(FormatToken(_, _: T.Dot, m), s)
                if m.rightOwner eq tree =>
              SplitTag.SelectChainFirstNL.activateOnly(s)
            case Decision(FormatToken(l, _: T.Comment, m), s)
                if m.rightOwner.eq(tree) && !l.is[T.Comment] =>
              SplitTag.SelectChainFirstNL.activateOnly(s)
          }
        }

        // trigger indent only on the first newline
        val fbIndent = style.getFewerBraces() != Indents.FewerBraces.never
        val noIndent = !fbIndent && checkFewerBraces(thisSelect.qual)
        val nlIndent =
          if (noIndent) Indent.Empty else Indent(indentLen, expire, After)
        val spcPolicy: Policy = delayedBreakPolicyOpt
        val nlPolicy = spcPolicy ? noIndent
        val splits =
          if (nextNonCommentSameLine(ftAfterRight).right.is[T.Comment])
            // will break
            baseSplits.map(_.withIndent(nlIndent).andFirstPolicy(nlPolicy))
          else {
            val spcIndent = nextDotIfSig.fold {
              val ok = style.getFewerBraces() == Indents.FewerBraces.always &&
                nextSelect.isEmpty && checkFewerBraces(expireTree)
              if (ok) nlIndent else Indent.empty
            } { x =>
              if (fbIndent) Indent(indentLen, x.left, Before) else Indent.Empty
            }
            baseSplits.map { s =>
              if (s.isNL) s.withIndent(nlIndent).andFirstPolicy(nlPolicy)
              else s.withIndent(spcIndent).andFirstPolicy(spcPolicy)
            }
          }

        if (prevSelect.isEmpty) splits
        else baseSplits ++ splits.map(_.onlyFor(SplitTag.SelectChainFirstNL))

      // ApplyUnary
      case FormatToken(T.Ident(_), T.Literal(), _) if leftOwner == rightOwner =>
        Seq(Split(NoSplit, 0))
      case FormatToken(op: T.Ident, right, _) if leftOwner.parent.exists {
            case unary: Term.ApplyUnary => unary.op.tokens.head == op
            case _ => false
          } => Seq(Split(Space(isSymbolicIdent(right)), 0))
      // Annotations, see #183 for discussion on this.
      case FormatToken(_, _: T.At, _) if rightOwner.is[Pat.Bind] =>
        Seq(Split(Space, 0))
      case FormatToken(_: T.At, _, _) if leftOwner.is[Pat.Bind] =>
        Seq(Split(Space, 0))
      case FormatToken(T.At(), _: T.Symbolic, _) => Seq(Split(NoSplit, 0))
      case FormatToken(T.At(), right @ T.Ident(_), _) =>
        // Add space if right starts with a symbol
        Seq(Split(identModification(right), 0))

      // Enum Case
      case FormatToken(_, T.KwExtends(), _)
          if rightOwner.isInstanceOf[Defn.EnumCase] =>
        val enumCase = rightOwner.asInstanceOf[Defn.EnumCase]
        binPackParentConstructorSplits(
          true,
          Set(rightOwner),
          enumCase.inits.headOption,
          getLast(rightOwner),
          style.indent.extendSite,
          enumCase.inits.lengthCompare(1) > 0,
        )

      // Template
      case FormatToken(_, soft.ExtendsOrDerives(), TemplateOnRight(template)) =>
        def lastToken = templateDerivesOrCurlyOrLastNonTrivial(template)

        binPackParentConstructorSplits(
          true,
          Set(template),
          findTemplateGroupOnRight(_.superType)(template),
          lastToken,
          style.indent.extendSite,
          if (template.earlyClause.nonEmpty) template.inits.nonEmpty
          else template.inits.lengthCompare(1) > 0,
        )

      // trait A extends B, C, D, E
      case FormatToken(_: T.Comma, _, _) if leftOwner.is[Template] =>
        val template = leftOwner.asInstanceOf[Template]
        typeTemplateSplits(template, style.indent.commaSiteRelativeToExtends)

      case FormatToken(_, _: T.KwWith, _) => rightOwner match {
          // something like new A with B with C
          case template: Template
              if template.parent
                .isAny[Term.New, Term.NewAnonymous, Defn.Given] =>
            val (isFirstCtor, extendsThenWith) = template.inits match {
              case _ :: x :: _ => (x.pos.start > ft.right.start, true)
              case _ => (false, false)
            }
            binPackParentConstructorSplits(
              isFirstCtor,
              Set(template),
              findTemplateGroupOnRight(_.superType)(template),
              templateCurlyOrLastNonTrivial(template),
              style.indent.main,
              extendsThenWith,
            )
          // trait A extends B with C with D with E
          case template: Template =>
            typeTemplateSplits(template, style.indent.withSiteRelativeToExtends)
          case t @ WithChain(top) => binPackParentConstructorSplits(
              !t.lhs.is[Type.With],
              withChain(top).toSet,
              Some(t.rhs),
              getLast(top),
              style.indent.main,
            )
          case enumCase: Defn.EnumCase =>
            val indent = style.indent.withSiteRelativeToExtends
            val expire = getLastToken(enumCase)
            Seq(
              Split(Space, 0).withIndent(indent, expire, ExpiresOn.After),
              Split(Newline, 1).withIndent(indent, expire, ExpiresOn.After),
            )
          case _ => Seq(Split(Space, 0))
        }
      // If/For/While/For with (
      case FormatToken(open: T.LeftParen, _, _) if (leftOwner match {
            case t: Term.EnumeratorsBlock => getHeadOpt(t).contains(ft)
            case _: Term.If | _: Term.While =>
              !isTokenHeadOrBefore(open, leftOwner)
            case _ => false
          }) =>
        val close = matching(open).left
        val indentLen = style.indent.ctrlSite.getOrElse(style.indent.callSite)
        def indents =
          if (style.align.openParenCtrlSite) getOpenParenAlignIndents(close)
          else Seq(Indent(indentLen, close, ExpiresOn.Before))
        val penalizeNewlines = penalizeNewlineByNesting(open, close)
        def baseNoSplit(commentNL: Modification = null)(implicit
            fileLine: FileLine,
        ) = Split.opt(getNoSplitAfterOpening(ft, commentNL = commentNL), 0)

        if (style.danglingParentheses.ctrlSite) {
          val noSplit =
            if (style.align.openParenCtrlSite) baseNoSplit()
              .withIndents(indents).withPolicy(penalizeNewlines)
              .andPolicy(decideNewlinesOnlyBeforeCloseOnBreak(close))
            else baseNoSplit().withSingleLine(close)
          Seq(
            noSplit,
            Split(Newline, 1).withIndent(indentLen, close, Before)
              .withPolicy(penalizeNewlines)
              .andPolicy(decideNewlinesOnlyBeforeClose(close)),
          )
        } else Seq(
          baseNoSplit(Newline).withIndents(indents).withPolicy(penalizeNewlines),
        )

      case FormatToken(_: T.KwIf, right, _) if leftOwner.is[Term.If] =>
        val owner = leftOwner.asInstanceOf[Term.If]
        val expire = getLastToken(owner)
        val isKeep = style.newlines.keep
        val mod =
          if (isKeep && hasBreak()) Newline
          else Space(style.spaces.isSpaceAfterKeyword(right))
        val slb = Split(mod.isNL, 0)(mod).withSingleLine(expire)
        val mlSplitBase = Split(mod, if (slb.isIgnored) 0 else 1).withPolicy(
          if (isKeep) getBreakBeforeElsePolicy(owner)
          else getBreaksBeforeElseChainPolicy(owner),
        )
        val mlSplitOpt = OptionalBraces
          .indentAndBreakBeforeCtrl[T.KwThen](owner.cond, mlSplitBase)
        Seq(slb, mlSplitOpt.getOrElse(mlSplitBase))
      case FormatToken(_: T.KwWhile | _: T.KwFor, right, _)
          if !leftOwner.is[Term.Name] => // exclude end marker
        def spaceMod = Space(style.spaces.isSpaceAfterKeyword(right))
        val splitBase = {
          val onlyNL = style.newlines.keepBreak(newlines)
          Split(if (onlyNL) Newline else spaceMod, 0)
        }
        val split = (leftOwner match {
          // block expr case is handled in OptionalBraces.WhileImpl
          case t: Term.While => OptionalBraces
              .indentAndBreakBeforeCtrl[T.KwDo](t.expr, splitBase)
          // below, multi-enum cases are handled in OptionalBraces.ForImpl
          case t: Term.For => getSingleElement(t.enumsBlock).flatMap(
              OptionalBraces.indentAndBreakBeforeCtrl[T.KwDo](_, splitBase),
            )
          case t: Term.ForYield => getSingleElement(t.enumsBlock).flatMap(
              OptionalBraces.indentAndBreakBeforeCtrl[T.KwYield](_, splitBase),
            )
          case _ => None
        }).getOrElse(Split(spaceMod, 0))
        Seq(split)
      case FormatToken(close: T.RightParen, right, _) if (leftOwner match {
            case t: Term.If => !nextNonComment(ft).right.is[T.KwThen] &&
              !isTokenLastOrAfter(close, t)
            case t: Term.While => !nextNonComment(ft).right.is[T.KwDo] &&
              !isTokenLastOrAfter(close, t)
            case t: Term.EnumeratorsBlock => t.parent match {
                case Some(_: Term.For) => !nextNonComment(ft).right.is[T.KwDo]
                case Some(_: Term.ForYield) => style.indentYieldKeyword
                case _ => false
              }
            case _ => false
          }) =>
        val body = leftOwner match {
          case t: Term.If => t.thenp
          case t: Term.While => t.body
          case t: Term.EnumeratorsBlock => t.parent match {
              case Some(p: Tree.WithBody) => p.body
              case _ => t
            }
        }
        val expire = getLastToken(body)
        def nlSplitFunc(cost: Int)(implicit l: sourcecode.Line) =
          Split(Newline2x(ft), cost).withIndent(style.indent.main, expire, After)
        if (style.newlines.getBeforeMultiline eq Newlines.unfold) CtrlBodySplits
          .checkComment(nlSplitFunc) { nft =>
            if (nft.right.is[T.LeftBrace]) {
              val nextFt = nextNonCommentSameLineAfter(nft)
              val policy = decideNewlinesOnlyAfterToken(nextFt.left)
              Seq(Split(Space, 0, policy = policy))
            } else Seq(nlSplitFunc(0))
          }
        else CtrlBodySplits.get(body) {
          Split(Space, 0).withSingleLineNoOptimal(
            expire,
            insideBracesBlock(ft, expire),
            noSyntaxNL = right.is[T.KwYield],
          )
        }(nlSplitFunc)
      case FormatToken(T.RightBrace(), T.KwElse(), _) =>
        val nlOnly = style.newlines.alwaysBeforeElseAfterCurlyIf ||
          !leftOwner.is[Term.Block] || !leftOwner.parent.contains(rightOwner)
        Seq(Split(Space.orNL(!nlOnly), 0))

      case FormatToken(T.RightBrace(), T.KwYield(), _) => Seq(Split(Space, 0))
      case FormatToken(_, kw @ (_: T.KwElse | _: T.KwYield), _) => Seq(
          if (style.newlines.okSpaceForSource(newlines)) {
            val expire = getLastToken(rightOwner)
            Split(Space, 0).withSingleLineNoOptimal(
              expire,
              exclude = insideBracesBlock(ft, expire),
              noSyntaxNL = kw.is[T.KwYield],
            )
          } else Split.ignored,
          Split(Newline, 1),
        )
      case FormatToken(_, _: T.KwThen | _: T.KwDo, _) =>
        val okSpace = style.newlines.sourceIgnored || noBreak()
        Seq(
          Split(!okSpace, 0)(Space).withOptimalToken(
            nextNonCommentSameLineAfter(ft).left,
            killOnFail = false,
          ),
          Split(Newline, 1),
        )
      // Last else branch
      case FormatToken(_: T.KwElse, _, _) if (leftOwner match {
            case t: Term.If => t.elsep match {
                case _: Term.If => false
                case b @ Term.Block((_: Term.If) :: Nil) =>
                  matchingOpt(nextNonComment(ft).right)
                    .exists(_.left.end >= b.pos.end)
                case _ => true
              }
            case x => throw new UnexpectedTree[Term.If](x)
          }) =>
        val body = leftOwner.asInstanceOf[Term.If].elsep
        val expire = getLastToken(body)
        def nlSplitFunc(cost: Int) = Split(Newline2x(ft), cost)
          .withIndent(style.indent.main, expire, After)
        if (style.newlines.getBeforeMultiline eq Newlines.unfold)
          Seq(nlSplitFunc(0))
        else CtrlBodySplits.get(body) {
          Split(Space, 0).withSingleLineNoOptimal(expire)
        }(nlSplitFunc)

      // Type variance
      case FormatToken(_: T.Ident, right @ (_: T.Ident | _: T.Underscore), _)
          if leftOwner.is[Mod.Variant] =>
        Seq(Split(Space(isSymbolicIdent(right)), 0))

      // Kind projector type lambda
      case FormatToken(T.Ident("+" | "-"), T.Underscore(), _)
          if rightOwner.is[Type] => Seq(Split(NoSplit, 0))

      // Var args
      case FormatToken(_, T.Ident("*"), _) if rightOwner.is[Type.Repeated] =>
        Seq(Split(NoSplit, 0))

      case FormatToken(open: T.LeftParen, right, _) =>
        val closeFt = matching(open)
        val close = closeFt.left
        val beforeClose = prev(closeFt)
        implicit val clauseSiteFlags = ClauseSiteFlags.atCallSite(leftOwner)
        val isConfig = couldPreserveConfigStyle(ft, beforeClose.hasBreak)

        val enclosed = findEnclosedBetweenParens(open, close, leftOwner)
        def spaceSplitWithoutPolicy(implicit fileLine: FileLine) = {
          val indent: Length = right match {
            case _: T.KwIf => StateColumn
            case _: T.KwFor if !style.indentYieldKeyword => StateColumn
            case _ =>
              val needIndent = enclosed.forall {
                case _: Term.ApplyInfix | _: Term.NewAnonymous => false
                case Term.ArgClause((_: Term.ApplyInfix) :: Nil, _) => false
                case _ => true
              } && {
                val pft = prevNonCommentSameLine(beforeClose)
                (pft eq beforeClose) && beforeClose.left.is[T.Comment]
              }
              Num(if (needIndent) style.indent.main else 0)
          }
          Split.opt(getNoSplitAfterOpening(ft, commentNL = null), 0)
            .withIndent(indent, close, Before)
        }
        def spaceSplit(implicit fileLine: FileLine) = spaceSplitWithoutPolicy
          .withPolicy(PenalizeAllNewlines(close, 1))
        def newlineSplit(cost: Int, forceDangle: Boolean)(implicit
            fileLine: FileLine,
        ) = {
          val shouldDangle = forceDangle || dangleCloseDelim
          Split(Newline, cost)
            .withPolicy(decideNewlinesOnlyBeforeClose(close), !shouldDangle)
            .withIndent(style.indent.callSite, close, Before)
        }
        if (isRightCommentThenBreak(ft)) Seq(newlineSplit(0, isConfig))
        else style.newlines.source match {
          case Newlines.classic =>
            Seq(if (isConfig) newlineSplit(0, true) else spaceSplit)
          case Newlines.keep =>
            Seq(if (hasBreak()) newlineSplit(0, isConfig) else spaceSplit)
          case _ =>
            val singleLine = enclosed.forall { x =>
              style.newlines.unfold && x.parent.exists {
                case _: Template.Body | _: Defn | _: Member.Infix => false
                case _ => true
              }
            }
            Seq(
              if (!singleLine) spaceSplit
              else spaceSplitWithoutPolicy.withSingleLine(close).andPolicy(
                getSingleLineInfixPolicy(close),
                ignore = !enclosed.exists(isInfixApp),
              ),
              newlineSplit(10, true),
            )
        }

      // Infix operator.
      case FormatToken(_: T.Ident, _, FormatToken.LeftOwner(AsInfixOp(app))) =>
        insideInfixSplit(app)
      case FormatToken(_, _: T.Ident, FormatToken.RightOwner(AsInfixOp(app))) =>
        insideInfixSplit(app)

      // Case
      case FormatToken(_: T.KwCase, _, _)
          if leftOwner.is[Defn.RepeatedEnumCase] =>
        val indent =
          Indent(style.indent.main, leftOwner.tokens.last, ExpiresOn.After)
        Seq(Split(Space, 0).withIndent(indent))

      case FormatToken(_: T.KwCase, _, _) if leftOwner.is[CaseTree] =>
        val owner = leftOwner.asInstanceOf[CaseTree]
        val arrowFt = leftOwner match {
          case c: Case => getCaseArrow(c)
          case tc: TypeCase => getCaseArrow(tc)
        }
        val arrow = arrowFt.left
        val postArrowFt = nextNonCommentSameLine(arrowFt)
        val postArrow = postArrowFt.left
        val ownerEnd = getLast(owner)
        val expire = nextNonCommentSameLine(ownerEnd).left

        val bodyBlock = isCaseBodyABlock(arrowFt, owner)
        def defaultPolicy = decideNewlinesOnlyAfterToken(postArrow)
        val postArrowPolicy =
          if (bodyBlock || (arrowFt ne postArrowFt) && postArrowFt.hasBreak)
            NoPolicy
          else {
            // postArrowFt points to non-comment after arrowFt
            // possibly on next line without intervening comments
            implicit val beforeMultiline = style.newlines.getBeforeMultiline
            getClosingIfCaseBodyEnclosedAsBlock(postArrowFt, owner).fold {
              Policy ? beforeMultiline.in(Newlines.fold, Newlines.keep) ||
              defaultPolicy
            } { rparenFt =>
              val lparentFt = next(postArrowFt)
              val postParenFt = nextNonCommentSameLine(lparentFt)
              val rparen = rparenFt.right
              val indent = style.indent.main
              val lindents = Seq(
                Indent(indent, rparen, Before),
                Indent(-indent, expire, After),
              )
              val lmod = NewlineT(noIndent = rhsIsCommentedOut(postParenFt))
              val lsplit = Seq(Split(lmod, 0).withIndents(lindents))
              val rsplit = Seq(Split(Newline, 0))
              Policy.after(postParenFt.left, prefix = "CASE[(]") {
                case Decision(`postParenFt`, _) => lsplit
                // fires only if there's a comment between lparentFt and postParentFt
                case Decision(`lparentFt`, _) => Seq(Split(Space, 0))
              } ==> Policy.on(rparen, prefix = "CASE[)]") {
                case Decision(`rparenFt`, _) => rsplit
              }
            }
          }

        val bodyIndent = if (bodyBlock) 0 else style.indent.main
        val arrowIndent = style.indent.caseSite - bodyIndent
        val indents = Seq(
          Indent(bodyIndent, expire, After),
          Indent(arrowIndent, arrow, After),
        )
        val mod = ModExt(Space, indents)
        val slbExpireOpt = prevNotTrailingComment(ownerEnd).toOption
        val policy = slbExpireOpt.fold(postArrowPolicy) { slbExpireFt =>
          val slbExpire = slbExpireFt.left
          val onArrowPolicy = Policy.End == arrow ==>
            Policy.after(postArrow, "CASESLB>ARROW") { case Decision(_, ss) =>
              ss.flatMap { s =>
                Seq(
                  s.notIf(s.isNL).withSingleLine(slbExpire, extend = true),
                  s.andPolicy(postArrowPolicy),
                )
              }
            }
          Policy.RelayOnSplit((s, _) => s.isNL)(onArrowPolicy, postArrowPolicy)
        }
        Seq(Split(mod, 0, policy = policy))

      case FormatToken(_, cond: T.KwIf, _) if rightOwner.is[Case] =>
        val arrow = getCaseArrow(rightOwner.asInstanceOf[Case]).left
        val noSplit =
          if (style.newlines.keepBreak(newlines)) Split.ignored
          else {
            val afterIf = nextNonCommentSameLineAfter(ft)
            if (style.newlines.keepBreak(afterIf)) {
              val indent = Indent(style.indent.main, arrow, ExpiresOn.Before)
              Split(Space, 0).withSingleLine(afterIf.left).withIndent(indent)
            } else {
              val exclude = insideBracesBlock(ft, arrow)
              Split(Space, 0).withSingleLine(
                arrow,
                exclude = exclude,
                recurseOnly = !exclude.isEmpty,
              )
            }
          }
        Seq(
          noSplit,
          Split(Newline, 1).withPolicy(penalizeNewlineByNesting(cond, arrow)),
        )

      case FormatToken(_: T.KwIf, _, _) if leftOwner.is[Case] =>
        val useNL = style.newlines.keepBreak(nextNonCommentSameLine(ft))
        Seq(Split(Space.orNL(!useNL), 0))

      // ForYield
      case FormatToken(
            _: T.LeftArrow | _: T.Equals,
            _,
            EnumeratorAssignRhsOnLeft(rhs),
          ) => getSplitsEnumerator(rhs)

      case FormatToken(kw @ (_: T.KwTry | _: T.KwCatch | _: T.KwFinally), _, _)
          if !leftOwner.is[Term.Name] => // exclude end marker
        val body = leftOwner match {
          case t: Term.TryClause => kw match {
              case _: T.KwTry => t.expr
              case _: T.KwCatch => t.catchClause.getOrElse(t)
              case _: T.KwFinally => t.finallyp.getOrElse(t)
              case _ => t
            }
          case t => t
        }
        val end = getLastToken(body)
        val indent = Indent(style.indent.main, end, ExpiresOn.After)
        CtrlBodySplits.get(body, Seq(indent)) {
          Split(Space, 0).withSingleLineNoOptimal(end)
        }(Split(Newline2x(ft), _).withIndent(indent))

      // Term.ForYield
      case FormatToken(T.KwYield(), _, _) if leftOwner.is[Term.ForYield] =>
        val lastToken = getLastToken(leftOwner.asInstanceOf[Term.ForYield].body)
        val indent = Indent(style.indent.main, lastToken, ExpiresOn.After)
        if (style.newlines.avoidAfterYield && !rightOwner.is[Term.If]) {
          val noIndent = !isRightCommentWithBreak(ft)
          Seq(Split(Space, 0).withIndent(indent, noIndent))
        } else Seq(
          // Either everything fits in one line or break on =>
          Split(Space, 0).withSingleLine(lastToken),
          Split(Newline, 1).withIndent(indent),
        )

      // Term.For
      case FormatToken(_: T.RightBrace, _, _)
          if leftOwner.is[Term.EnumeratorsBlock] &&
            leftOwner.parent.is[Term.For] &&
            !nextNonComment(ft).right.is[T.KwDo] =>
        val body = leftOwner.parent.get.asInstanceOf[Term.For].body
        def nlSplit(cost: Int) = Split(Newline2x(ft), cost)
          .withIndent(style.indent.main, getLastToken(body), After)
        CtrlBodySplits.get(body)(null)(nlSplit)

      // After comment
      case FormatToken(_: T.Comment, _, _) => Seq(Split(getMod(ft), 0))
      // Inline comment
      case FormatToken(_, _: T.Comment, _) =>
        val forceBlankLine = hasBreak() && blankLineBeforeDocstring(ft)
        val mod = if (forceBlankLine) Newline2x else getMod(ft)
        val nft = nextNonCommentAfter(ft)
        def baseSplit(implicit fileLine: FileLine) = Split(mod, 0)
        def split(implicit fileLine: FileLine) = baseSplit
          .withIndent(style.indent.main, nft.left, ExpiresOn.After)

        nft match {
          case FormatToken(_, _: T.Dot, GetSelectLike.OnRight(t)) =>
            if (findPrevSelect(t, style.encloseSelectChains).isEmpty) Seq(split)
            else Seq(baseSplit, split.onlyFor(SplitTag.SelectChainFirstNL))
          case _ => prevBeforeNonComment(ft) match {
              case FormatToken(_, _: T.Dot, GetSelectLike.OnRight(t)) =>
                if (findPrevSelect(t, style.encloseSelectChains).isEmpty)
                  Seq(split)
                else Seq(baseSplit, split.onlyFor(SplitTag.SelectChainFirstNL))
              case _ => Seq(baseSplit)
            }
        }

      case FormatToken(soft.ImplicitOrUsing(), _, ImplicitUsingOnLeft(params))
          if style.binPack.defnSite == BinPack.Site.Never &&
            !style.verticalMultiline.atDefnSite =>
        val spaceSplit = Split(Space, 0)
          .notIf(style.newlines.forceAfterImplicitParamListModifier).withPolicy(
            SingleLineBlock(params.tokens.last),
            style.newlines.notPreferAfterImplicitParamListModifier,
          )
        Seq(spaceSplit, Split(Newline, if (spaceSplit.isActive) 1 else 0))

      case FormatToken(_, r, _) if optimizationEntities.optionalNL =>
        @tailrec
        def noAnnoLeftFor(tree: Tree): Boolean = tree.parent match {
          case Some(_: Mod.Annot) => false
          case Some(p: Init) => noAnnoLeftFor(p)
          case _ => true
        }
        def noAnnoLeft = leftOwner.is[Mod] || noAnnoLeftFor(leftOwner)
        def newlineOrBoth = {
          val spaceOk = !style.optIn.annotationNewlines
          Seq(Split(Space.orNL(spaceOk), 0), Split(Newline, 1).onlyIf(spaceOk))
        }
        style.newlines.source match {
          case _ if hasBlankLine => Seq(Split(Newline2x, 0))
          case Newlines.unfold =>
            if (r.is[T.At]) newlineOrBoth
            else Seq(Split(Space.orNL(noAnnoLeft), 0))
          case Newlines.fold =>
            if (r.is[T.At]) Seq(Split(Space, 0), Split(Newline, 1))
            else if (noAnnoLeft) Seq(Split(Space, 0))
            else newlineOrBoth
          case _ =>
            val noNL = !style.optIn.annotationNewlines || noBreak()
            Seq(Split(Space.orNL(noNL), 0))
        }

      // Pattern alternatives
      case FormatToken(T.Ident("|"), _, _) if leftOwner.is[Pat.Alternative] =>
        if (style.newlines.keep) Seq(Split(Space.orNL(noBreak()), 0))
        else {
          val end = getLast(leftOwner.asInstanceOf[Pat.Alternative].rhs match {
            case t: Pat.Alternative => t.lhs
            case t => t
          })
          val opt = nextNonComment(end).right match {
            case _: T.KwIf => end.left
            case x => x
          }
          Seq(
            Split(Space, 0).withOptimalToken(opt, killOnFail = false),
            Split(Newline, 1),
          )
        }
      case FormatToken(_, T.Ident("|"), _) if rightOwner.is[Pat.Alternative] =>
        val noNL = !style.newlines.keepBreak(newlines)
        Seq(Split(Space.orNL(noNL), 0))
      case FormatToken(_, T.Ident("*"), _)
          if rightOwner.is[Pat.SeqWildcard] || rightOwner.is[Term.Repeated] ||
            rightOwner.is[Pat.Repeated] => Seq(Split(NoSplit, 0))
      case FormatToken(
            T.Ident(_) | T.Literal() | T.Interpolation.End() | T.Xml.End(),
            T.Ident(_) | T.Literal() | T.Xml.Start(),
            _,
          ) => Seq(Split(Space, 0))

      // Case
      case FormatToken(left, _: T.KwMatch, _) =>
        // do not split `.match`
        val noSplit = left.is[T.Dot] && dialect.allowMatchAsOperator
        Seq(Split(Space(!noSplit), 0))

      // Protected []
      case FormatToken(_, T.LeftBracket(), _)
          if isModPrivateProtected(leftOwner) => Seq(Split(NoSplit, 0))
      case FormatToken(T.LeftBracket(), _, _)
          if isModPrivateProtected(leftOwner) => Seq(Split(NoSplit, 0))

      // Term.ForYield
      case FormatToken(_, _: T.KwIf, _) if rightOwner.is[Enumerator.Guard] =>
        /* this covers the first guard only; second and later consecutive guards
         * are considered to start a new statement and are handled early, in the
         * "if startsStatement" matches */
        style.newlines.source match {
          case Newlines.fold =>
            val endOfGuard = getLastToken(rightOwner)
            val exclude = insideBracesBlock(ft, endOfGuard, true)
            Seq(
              Split(Space, 0).withSingleLine(endOfGuard, exclude = exclude),
              Split(Newline, 1),
            )
          case Newlines.unfold => Seq(Split(Newline, 0))
          case _ => Seq(Split(Space, 0).onlyIf(noBreak()), Split(Newline, 1))
        }
      // Interpolation
      case FormatToken(_, _: T.Interpolation.Id, _) => Seq(Split(Space, 0))
      // Throw exception
      case FormatToken(T.KwThrow(), _, _) => Seq(Split(Space, 0))

      // Singleton types
      case FormatToken(_, T.KwType(), _) if rightOwner.is[Type.Singleton] =>
        Seq(Split(NoSplit, 0))
      // seq to var args foo(seq:_*)
      case FormatToken(T.Colon(), T.Underscore(), _)
          if next(ft).meta.right.text == "*" => Seq(Split(Space, 0))
      case FormatToken(T.Underscore(), T.Ident("*"), _)
          if prev(ft).left.is[T.Colon] => Seq(Split(NoSplit, 0))

      // Xml
      case FormatToken(_, _: T.Xml.Start, _) => Seq(Split(Space, 0))
      case FormatToken(open: T.Xml.Start, _, _) =>
        val splits = Seq(Split(NoSplit, 0))
        if (prev(ft).left.is[T.LeftBrace]) splits
        else withIndentOnXmlStart(open, splits)
      case FormatToken(_: T.Xml.SpliceStart, _, _)
          if style.xmlLiterals.assumeFormatted =>
        withIndentOnXmlSpliceStart(ft, Seq(Split(NoSplit, 0)))
      case FormatToken(T.Xml.Part(_), _, _) => Seq(Split(NoSplit, 0))
      case FormatToken(_, T.Xml.Part(_), _) => Seq(Split(NoSplit, 0))

      // Fallback
      case FormatToken(_, T.Dot(), _) => Seq(Split(NoSplit, 0))
      case FormatToken(left, T.Hash(), _) =>
        Seq(Split(Space(endsWithSymbolIdent(left)), 0))
      case FormatToken(T.Hash(), ident: T.Ident, _) =>
        Seq(Split(Space(isSymbolicIdent(ident)), 0))
      case FormatToken(T.Dot(), T.Ident(_) | T.KwThis() | T.KwSuper(), _) =>
        Seq(Split(NoSplit, 0))
      case FormatToken(_, T.RightBracket(), _) => Seq(Split(NoSplit, 0))
      case FormatToken(_, _: T.RightParen, _) =>
        val ok = !style.newlines.keepBreak(newlines) ||
          style.binPack.siteFor(rightOwner)
            .fold(!rightOwner.is[Pat.Alternative])(_._1 eq BinPack.Site.Never)
        Seq(Split(if (ok) getNoSplitBeforeClosing(ft, Newline) else Newline, 0))

      case FormatToken(left, _: T.KwCatch | _: T.KwFinally, _)
          if style.newlines.alwaysBeforeElseAfterCurlyIf ||
            !left.is[T.RightBrace] ||
            leftOwner.ne(rightOwner) &&
            !leftOwner.parent.contains(rightOwner) =>
        Seq(Split(Newline2x(hasBlankLine), 0))

      case FormatToken(_, Reserved(), _) => Seq(Split(Space, 0))

      case FormatToken(Reserved(), _, _) => Seq(Split(Space, 0))
      case FormatToken(T.LeftBracket(), _, _) => Seq(Split(NoSplit, 0))
      case FormatToken(_, _: T.Symbolic, _) => Seq(Split(Space, 0))
      case FormatToken(T.Underscore(), T.Ident("*"), _) => Seq(Split(NoSplit, 0))
      case FormatToken(T.RightArrow(), _, _) if leftOwner.is[Type.ByName] =>
        val mod = Space(style.spaces.inByNameTypes)
        Seq(Split(mod, 0))
      case FormatToken(_: T.Colon, _, _)
          if style.spaces.notAfterColon(leftOwner) => Seq(Split(NoSplit, 0))
      case FormatToken(_: T.Symbolic, _, _) => Seq(Split(Space, 0))
      case tok =>
        logger.debug(s"MISSING CASE: $tok")
        Seq() // No solution available, partially format tree.
    }
  }

  /** Assigns possible splits to a FormatToken.
    *
    * The FormatToken can be considered as a node in a graph and the splits as
    * edges. Given a format token (a node in the graph), Route determines which
    * edges lead out from the format token.
    */
  def getSplits(ft: FormatToken): Seq[Split] = {
    val splits = getSplitsImpl(ft).filter(!_.isIgnored)
    require(
      splits.nonEmpty,
      s"""|
          |FT: ${log2(ft)}
          |LO: ${log(ft.leftOwner)}
          |RO: ${log(ft.rightOwner)}
          |""".stripMargin,
    )
    def splitsAsNewlines(splits: Seq[Split]): Seq[Split] = {
      val filtered = Decision.onlyNewlineSplits(splits)
      if (filtered.nonEmpty) filtered else splits.map(_.withMod(Newline))
    }
    ft match {
      case FormatToken(_: T.BOF, _, _) => splits
      case FormatToken(_, _: T.Comment, _) if rhsIsCommentedOutIfComment(ft) =>
        splitsAsNewlines(splits.map(_.withNoIndent))
      case FormatToken(_: T.Comment, _, _) =>
        if (ft.noBreak) splits else splitsAsNewlines(splits)
      case FormatToken(_, _: T.Comment, _)
          if hasBreakAfterRightBeforeNonComment(ft) =>
        if (ft.noBreak) splits.map(_.withMod(Space))
        else splitsAsNewlines(splits)
      case ft if ft.meta.formatOff && ft.hasBreak => splitsAsNewlines(splits)
      case FormatToken(_: T.Equals, r: T.KwMacro, _)
          if dialect.allowSignificantIndentation =>
        // scala3 compiler doesn't allow newline before `macro`
        splits.map { s =>
          if (!s.isNL) s
          else s.withMod(Space).andPolicy(decideNewlinesOnlyAfterClose(r))
        }
      case _ => splits
    }
  }

  private implicit def int2num(n: Int): Num = Num(n)

  private def getSplitsDefValEquals(
      body: Tree,
      endFt: => FormatToken,
      spaceIndents: Seq[Indent] = Seq.empty,
  )(
      splits: => Seq[Split],
  )(implicit style: ScalafmtConfig, ft: FormatToken): Seq[Split] =
    if (ft.right.is[T.LeftBrace]) // The block will take care of indenting by 2
      Seq(Split(Space, 0).withIndents(spaceIndents))
    else if (isRightCommentWithBreak(ft))
      Seq(CtrlBodySplits.withIndent(Split(Space.orNL(ft), 0), body, endFt))
    else if (isJsNative(body))
      Seq(Split(Space, 0).withSingleLineNoOptimal(endFt.left))
    else if (
      style.dialect.allowSignificantIndentation &&
      (style.newlines.sourceIgnored || ft.noBreak) && body.parent.exists {
        case p: Enumerator.Assign => (p.body eq body) && p.parent.exists {
            case pp: Term.EnumeratorsBlock => isEnclosedInParens(pp)
            case _ => false
          }
        case _ => false
      }
    ) Seq(Split(Space, 0).withIndents(spaceIndents))
    else if (style.newlines.forceBeforeAssign(ft.meta.leftOwner))
      Seq(CtrlBodySplits.withIndent(Split(Newline2x(ft), 0), body, endFt))
    else if (style.newlines.shouldForceBeforeMultilineAssign(ft.meta.leftOwner))
      CtrlBodySplits.slbOnly(body, spaceIndents) { x =>
        CtrlBodySplits.withIndent(Split(Newline2x(ft), x), body, endFt)
      }
    else splits

  private def getSplitsDefEquals(body: Tree, endFt: FormatToken)(implicit
      style: ScalafmtConfig,
      ft: FormatToken,
  ): Seq[Split] = {
    val expire = endFt.left
    def baseSplit = Split(Space, 0)
    def newlineSplit(cost: Int)(implicit fileLine: FileLine) = CtrlBodySplits
      .withIndent(Split(Newline2x(ft), cost), body, endFt)

    def getClassicSplits(implicit fileLine: FileLine) =
      if (ft.hasBreak) Seq(newlineSplit(0)) else Seq(baseSplit, newlineSplit(1))

    style.newlines.beforeMultilineDef.fold {
      getSplitsValEquals(body, endFt)(getClassicSplits)
    } {
      case Newlines.classic => getClassicSplits

      case Newlines.keep if ft.hasBreak => Seq(newlineSplit(0))

      case Newlines.unfold =>
        Seq(baseSplit.withSingleLine(expire), newlineSplit(1))

      case x => CtrlBodySplits.folded(body, x eq Newlines.keep)(newlineSplit)
    }
  }

  private def getSplitsValEquals(body: Tree, endFt: => FormatToken)(
      classicSplits: => Seq[Split],
  )(implicit style: ScalafmtConfig, ft: FormatToken): Seq[Split] =
    if (style.newlines.getBeforeMultiline eq Newlines.classic) classicSplits
    else CtrlBodySplits.getWithIndent(body, endFt)(null)(Split(Newline2x(ft), _))

  private def getSplitsValEqualsClassic(
      rawBody: Tree,
      endFt: FormatToken,
  )(implicit style: ScalafmtConfig, ft: FormatToken): Seq[Split] = {
    def wouldDangle = ft.meta.leftOwner.parent
      .exists(style.danglingParentheses.atSite(_, false))

    val expire = endFt.left
    // rhsOptimalToken is too aggressive here
    val optimalFt = endFt.right match {
      case _: T.Comma => next(endFt)
      case RightParenOrBracket() if !wouldDangle => next(endFt)
      case _ => endFt
    }
    val optimal = optimalFt.left
    def optimalWithComment = optimalFt.right match {
      case x: T.Comment if optimalFt.noBreak => x
      case _ => optimal
    }

    val penalty = ft.meta.leftOwner match {
      case _: Term.Assign if style.binPack.callSite != BinPack.Site.Never =>
        Constants.BinPackAssignmentPenalty
      case _: Term.Param if style.binPack.defnSite != BinPack.Site.Never =>
        Constants.BinPackAssignmentPenalty
      case _ => 0
    }

    def baseSpaceSplit(implicit fileLine: FileLine) =
      Split(isRightCommentThenBreak(ft), 0)(Space)
    def twoBranches(implicit fileLine: FileLine) = baseSpaceSplit
      .withOptimalToken(optimal, killOnFail = false).withPolicy {
        val exclude = insideBracesBlock(ft, expire)
        policyWithExclude(exclude, Policy.End.On, Policy.End.After)(
          PenalizeAllNewlines(expire, Constants.ShouldBeSingleLine),
        )
      }
    val body = CtrlBodySplits.getBlockStat(rawBody)
    val spaceSplit = body match {
      case _ if ft.hasBreak && ft.meta.leftOwner.is[Defn] => Split.ignored
      case _: Term.If => twoBranches
      case _: Term.ForYield => twoBranches
      // we force newlines in try/catch/finally
      case _: Term.TryClause => Split.ignored
      case _ => baseSpaceSplit
          .withOptimalToken(optimalWithComment, killOnFail = false)
    }
    Seq(
      spaceSplit,
      CtrlBodySplits.withIndent(Split(Newline, 1 + penalty), body, endFt),
    )
  }

  private def getSplitsEnumerator(body: Tree)(implicit
      style: ScalafmtConfig,
      ft: FormatToken,
  ): Seq[Split] = maybeGetInfixSplitsBeforeLhs() {
    val endFt = getLastNonTrivial(body)
    val expire = endFt.left
    val spaceIndents =
      if (!style.align.arrowEnumeratorGenerator) Seq.empty
      else Seq(Indent(StateColumn, expire, After))
    getSplitsDefValEquals(body, endFt, spaceIndents) {
      CtrlBodySplits.get(body, spaceIndents) {
        if (spaceIndents.nonEmpty) Split(Space, 0).withIndents(spaceIndents)
        else {
          val noSlb = body match {
            case _: Term.TryClause => false
            case t: Term.If => ifWithoutElse(t)
            case _ => true
          }
          if (noSlb) Split(Space, 0)
            .withOptimalToken(ft.right, killOnFail = false)
          else Split(Space, 0).withSingleLine(expire)
        }
      }(cost => CtrlBodySplits.withIndent(Split(Newline2x(ft), cost), endFt))
    }
  }

}
