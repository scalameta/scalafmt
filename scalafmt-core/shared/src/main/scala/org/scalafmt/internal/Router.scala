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
import org.scalafmt.util._

import org.scalameta.FileLine
import scala.meta._
import scala.meta.classifiers.Classifier
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
        val end = matching(start)
        val okNewlines = style.newlines.inInterpolation
          .ne(Newlines.InInterpolation.avoid) && isTripleQuote(m.left.text)
        def policy = PenalizeAllNewlines(end, BreakSingleLineInterpolatedString)
        val split = Split(NoSplit, 0).withPolicy(policy, okNewlines)
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
        val close = matching(open)
        val beforeClose = justBefore(close)
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
        val close = matching(open)
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
        def isSimpleInterpolate = (leftOwner match {
          case t: Pat.Interpolate => findArgAfter(open.end, t.args)
          case t: Term.Interpolate => findArgAfter(open.end, t.args)
          case t: Term.Block => getBlockSingleStat(t)
          case _ => None
        }).exists(!_.is[Term.If])

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
            val afterClose = tokens(close, 3)
            val lastPart = afterClose.left.is[T.Interpolation.End]
            val slbEnd = if (lastPart) afterClose.left else afterClose.right
            Seq(spaceSplit.withSingleLine(slbEnd), newlineSplit(1))
        }

      case FormatToken(_, _: T.RightBrace, _) if isInterpolate(rightOwner) =>
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
        maybeGetInfixSplitsBeforeLhs(ft) {
          getSplitsDefValEquals(ft, rhs) {
            if (leftOwner.is[Tree.WithParamClauses]) getSplitsDefEquals(ft, rhs)
            else getSplitsValEquals(ft, rhs)(getSplitsValEqualsClassic(ft, rhs))
          }
        }

      // { ... } Blocks
      case FormatToken(open: T.LeftBrace, right, m) =>
        val close = matching(open)
        val closeFT = tokens(close)
        val newlineBeforeClosingCurly = decideNewlinesOnlyBeforeClose(close)
        val selfAnnotationLast: Option[T] = leftOwner match {
          // Self type: trait foo { self => ... }
          case t: Template => t.self.tokens.lastOption
          case _ => None
        }
        val isSelfAnnotationNL = selfAnnotationLast.nonEmpty &&
          style.optIn.selfAnnotationNewline &&
          (hasBreak() || style.newlines.sourceIgnored)
        val rightIsComment = right.is[T.Comment]
        val nl: Modification =
          if (rightIsComment && noBreak()) Space
          else Newline2x(
            hasBlankLine ||
              !isSelfAnnotationNL &&
              rightIsComment && blankLineBeforeDocstring(ft),
          )

        // lambdaNLOnly: None for single line only
        val (lambdaExpire, lambdaArrow, lambdaIndent, lambdaNLOnly) =
          statementStarts.get(m.idx + 1) match {
            case Some(owner: Term.FunctionTerm) if (leftOwner match {
                  case t: Template => t.parent
                      .exists(_.is[Term.NewAnonymous]) &&
                    isSingleElement(t.stats, owner)
                  case _ => true
                }) =>
              val arrow = getFuncArrow(lastLambda(owner))
              val expire = arrow.getOrElse(getLast(owner))
              val nlOnly =
                if (style.newlines.alwaysBeforeCurlyLambdaParams) Some(true)
                else if (
                  style.newlines.beforeCurlyLambdaParams eq
                    Newlines.BeforeCurlyLambdaParams.multiline
                ) None
                else Some(false)
              (expire, arrow.map(_.left), 0, nlOnly)
            case Some(t: Case)
                if t.cond.isEmpty &&
                  (leftOwner match {
                    case x: Term.PartialFunction => isSingleElement(x.cases, t)
                    case x: Term.Match => isSingleElement(x.cases, t) &&
                      dialect.allowMatchAsOperator &&
                      tokenAfter(x.expr).right.is[T.Dot]
                    case _: Term.Try => false
                    case _ => tokenAfter(t).right eq close
                  }) =>
              val arrow = getCaseArrow(t)
              val nlOnly =
                if (style.newlines.alwaysBeforeCurlyLambdaParams) Some(true)
                else if (
                  style.newlines.beforeCurlyLambdaParams ne
                    Newlines.BeforeCurlyLambdaParams.never
                ) None
                else Some(false)
              (arrow, Some(arrow.left), 0, nlOnly)
            case _ =>
              val annoOpt = selfAnnotationLast.map { anno =>
                val indent = style.indent.main
                val annoFT = tokens(anno)
                val arrow = annoFT.left.is[T.RightArrow]
                val expire = if (arrow) annoFT else nextAfterNonComment(annoFT)
                (expire, Some(expire.left), indent, Some(isSelfAnnotationNL))
              }
              annoOpt.getOrElse((null, None, 0, None))
          }
        val lambdaPolicy =
          if (lambdaExpire == null) null
          else {
            val arrowOptimal = getOptimalTokenFor(lambdaExpire)
            newlineBeforeClosingCurly & SingleLineBlock(arrowOptimal) &
              decideNewlinesOnlyAfterToken(arrowOptimal)
          }

        def getSingleLinePolicy = {
          type Classifiers = Seq[Classifier[T, _]]
          def classifiersByParent: Classifiers = leftOwner.parent match {
            case Some(_: Term.If) => Seq(T.KwElse.classifier)
            case Some(_: Term.Try | _: Term.TryWithHandler) =>
              Seq(T.KwCatch.classifier, T.KwFinally.classifier)
            case _ => Seq.empty
          }
          val classifiers: Classifiers = leftOwner match {
            // for catch with case, we should go up only one level
            case _: Term.Try if rightOwner.is[Case] =>
              Seq(T.KwFinally.classifier)
            case _ => classifiersByParent
          }

          Policy ? classifiers.exists(_(closeFT.right)) &&
          decideNewlinesOnlyAfterClose(close)
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
            val isTopLevelBlock = leftOwner.parent.exists(_.parent.isEmpty) ||
              (leftOwner match {
                case t: Template =>
                  // false for
                  // new A { () =>
                  //   println("A")
                  // }
                  // but true for
                  // new A {
                  //   def f = x
                  // }
                  !t.parent.exists(_.is[Term.NewAnonymous]) ||
                  t.stats.exists(_.is[Defn])
                case _ => false
              })

            // do not fold top-level blocks
            if (isTopLevelBlock) None
            else if (lambdaPolicy != null) getSingleLineLambdaDecisionOpt
            else Some(false)
          // old behaviour
          case _ =>
            if (lambdaPolicy == null) getClassicSingleLineDecisionOpt
            else getSingleLineLambdaDecisionOpt
        }

        val singleLineSplit = singleLineDecisionOpt.fold(Split.ignored) { sld =>
          val sldPolicy = getSingleLinePolicy
          val expire = leftOwner match {
            case _: Term.ForYield
                if !sld && sldPolicy.isEmpty &&
                  (style.newlines.source eq Newlines.fold) =>
              getLastToken(leftOwner)
            case _ => endOfSingleLineBlock(closeFT)
          }
          Split(xmlSpace(leftOwner), 0)
            .withSingleLine(expire, noSyntaxNL = true, killOnFail = true)
            .andPolicy(sldPolicy)
        }

        val splits = Seq(
          singleLineSplit,
          Split(nl, 1).withPolicy(newlineBeforeClosingCurly)
            .withIndent(style.indent.main, close, Before),
          Split(Space, 0)
            .onlyIf(lambdaNLOnly.contains(false) && lambdaPolicy != null)
            .notIf(style.newlines.keepBreak(newlines))
            .withOptimalTokenOpt(lambdaArrow)
            .withIndent(lambdaIndent, close, Before).withPolicy(lambdaPolicy),
        )
        right match {
          case t: T.Xml.Start => withIndentOnXmlStart(t, splits)
          case _ => splits
        }

      case FormatToken(
            T.RightArrow() | T.ContextArrow(),
            _,
            StartsStatementRight(stmt),
          ) if leftOwner.isInstanceOf[Term.FunctionTerm] =>
        val leftFunc = leftOwner.asInstanceOf[Term.FunctionTerm]
        val (afterCurlySpace, afterCurlyNewlines) =
          getSpaceAndNewlineAfterCurlyLambda(newlines)
        val spaceSplit =
          if (stmt.isInstanceOf[Term.FunctionTerm]) Split(Space, 0)
          else if (
            afterCurlySpace &&
            (!rightOwner.is[Defn] || style.newlines.source.eq(Newlines.fold))
          ) Split(Space, 0).withSingleLineNoOptimal(
            getOptimalTokenFor(getLastNonTrivial(leftFunc.body).left),
            noSyntaxNL = true,
          )
          else Split.ignored
        val (endIndent, expiresOn) = functionExpire(leftFunc)
        Seq(
          spaceSplit,
          Split(afterCurlyNewlines, 1)
            .withIndent(style.indent.main, endIndent, expiresOn),
        )

      case FormatToken(_: T.RightArrow | _: T.ContextArrow, right, _)
          if (leftOwner match {
            case _: Term.FunctionTerm | _: Term.PolyFunction => true
            case t: Template => t.parent.exists(_.is[Term.NewAnonymous])
            case t: Self =>
              t.parent.exists(_.parent.exists(_.is[Term.NewAnonymous]))
            case _ => false
          }) =>
        val (endOfFunction, expiresOn) = leftOwner match {
          case t: Term.FunctionTerm => functionExpire(t)
          case t => getLastNonTrivialToken(t) -> ExpiresOn.Before
        }

        val hasSingleLineComment = isRightCommentThenBreak(ft)
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
          val isCurlyLambda = leftOwner.is[Template] ||
            leftOwner.parent.exists(_.is[Template])

          def noSquash = style.newlines.afterCurlyLambdaParams ne
            Newlines.AfterCurlyLambdaParams.squash

          style.newlines.source match {
            case Newlines.fold => false
            case Newlines.unfold => isCurlyLambda && noSquash
            case Newlines.keep => hasBreak()
            case Newlines.classic => isCurlyLambda && hasBreak() && noSquash
          }
        }
        val singleLineSplit = Split(Space, 0)
          .notIf(hasSingleLineComment || noSingleLine)
          .withSingleLineNoOptimal(endOfFunction)
        def newlineSplit = Split(Newline, 1 + nestedApplies(leftOwner))
          .withIndent(indent, endOfFunction, expiresOn)
        val multiLineSplits =
          if (hasSingleLineComment) Seq(newlineSplit)
          else {
            // 2020-01: break after same-line comments, and any open brace
            val nonComment = nextNonCommentSameLine(ft)
            val hasBlock = nonComment.right.is[T.LeftBrace] &&
              (matching(nonComment.right) eq endOfFunction)
            if (!hasBlock && (nonComment eq ft)) Seq(newlineSplit)
            else {
              // break after the brace or comment if fits, or now if doesn't
              // if brace, don't add indent, the LeftBrace rule will do that
              val spaceIndent = if (hasBlock) 0 else indent
              Seq(
                Split(Space, 0)
                  .withIndent(spaceIndent, endOfFunction, expiresOn)
                  .withOptimalToken(getOptimalTokenFor(next(nonComment))),
                newlineSplit,
              )
            }
          }
        singleLineSplit +: multiLineSplits

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
        def baseSplit = Split(Space, 0)
        def nlSplit(ft: FormatToken)(cost: Int)(implicit l: FileLine) =
          Split(Newline2x(isDouble = ft.hasBlankLine && bodyIsEmpty), cost)
        CtrlBodySplits.checkComment(ft, nlSplit(ft)) { ft =>
          def withSlbSplit(implicit l: FileLine) = Seq(
            baseSplit.withSingleLine(getLastNonTrivialToken(body)),
            nlSplit(ft)(1)(nextLine),
          )
          val beforeMultiline = style.newlines.getBeforeMultiline
          if (isCaseBodyABlock(ft, owner)) Seq(baseSplit)
          else if (isCaseBodyEnclosedAsBlock(ft, owner)) Seq(baseSplit)
          else if (ft.right.is[T.KwCase]) Seq(nlSplit(ft)(0))
          else if (hasBreak() && !beforeMultiline.ignoreSourceSplit)
            Seq(nlSplit(ft)(0))
          else if (bodyIsEmpty) Seq(baseSplit, nlSplit(ft)(1))
          else if (beforeMultiline eq Newlines.unfold)
            if (style.newlines.source ne Newlines.unfold) withSlbSplit
            else Seq(nlSplit(ft)(0))
          else if (
            condIsDefined || beforeMultiline.eq(Newlines.classic) ||
            getSingleStatExceptEndMarker(body).isEmpty
          ) withSlbSplit
          else {
            val isKeep = beforeMultiline.eq(Newlines.keep)
            CtrlBodySplits.foldedNonEmptyNonComment(body, nlSplit(ft), isKeep)
          }
        }
      // New statement
      case FormatToken(_: T.Semicolon, _, StartsStatementRight(stmt)) => Seq(
          if (style.newlines.okSpaceForSource(newlines)) {
            val expire = endOfSingleLineBlock(getLast(stmt))
            Split(Space, 0).withSingleLine(expire)
          } else Split.ignored,
          // For some reason, this newline cannot cost 1.
          Split(Newline2x(ft), 0),
        )

      case FormatToken(
            _: T.RightParen,
            _,
            ParamClauseParentLeft(extGroup: Defn.ExtensionGroup),
          ) if !LeftParenOrBrace.unapply(nextNonComment(ft).right) =>
        val expireToken = getLastToken(extGroup)
        def nlSplit(cost: Int = 0)(implicit fileLine: FileLine) =
          Split(Newline, cost).withIndent(style.indent.main, expireToken, After)
        style.newlines.source match {
          case Newlines.unfold => Seq(nlSplit())
          case Newlines.keep if hasBreak() => Seq(nlSplit())
          case _ =>
            Seq(Split(Space, 0).withSingleLine(expireToken), nlSplit(cost = 1))
        }

      case FormatToken(left, right, StartsStatementRight(_)) =>
        val expire = rightOwner.tokens.find(_.is[T.Equals])
          .fold(getLastToken(rightOwner)) { equalsToken =>
            val equalsFormatToken = tokens(equalsToken)
            if (equalsFormatToken.right.is[T.LeftBrace]) equalsFormatToken.right
            else equalsToken
          }

        val annoRight = right.is[T.At]
        val annoLeft = isSingleIdentifierAnnotation(prev(ft))

        if (
          (annoRight || annoLeft) && style.optIn.annotationNewlines &&
          !style.newlines.sourceIgnored
        ) Seq(Split(getMod(ft), 0))
        else maybeGetInfixSplitsBeforeLhs(
          ft,
          Some(if (left.is[T.Comment] && noBreak()) Space else Newline2x(ft)),
        ) {
          val spaceCouldBeOk = annoLeft &&
            (style.newlines.source match {
              case Newlines.unfold => right.is[T.Comment] ||
                !style.optIn.annotationNewlines && annoRight
              case Newlines.fold => right.is[T.Comment] || annoRight ||
                !style.optIn.annotationNewlines && Reserved.unapply(right)
              case Newlines.keep => noBreak() &&
                (annoRight || Reserved.unapply(right))
              case _ => noBreak() && Reserved.unapply(right)
            })
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
          case t: Init => t.parent.forall(_.is[Mod.Annot])
          case Term.Name(name) => style.spaces.afterTripleEquals &&
            name == "===" ||
            (rightOwner match {
              case _: Term.ArgClause => style.spaces.beforeApplyArgInParens(name)
              case _: Member.ParamClause => style.spaces.afterSymbolicDefs &&
                isSymbolicName(name)
              case _ => false
            })
          case _: Defn.ExtensionGroup => style.spaces.afterKeywordBeforeParen &&
            soft.KwExtension.unapply(left)
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
          val close = matching(open)
          val indent = Indent(indentLen, close, ExpiresOn.After)
          val isAlignFirstParen = shouldAlignBefore(style.align) &&
            !prevNonComment(ft).left.is[T.RightParen]
          def noSplitSplit(implicit fileLine: FileLine) =
            if (isAlignFirstParen) baseNoSplit
            else baseNoSplit.withSingleLine(close)
          def afterClose: Option[T] = {
            val ftAfterClose = tokenAfter(close)
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
            val beforeBody = defRhs.flatMap {
              case t: Template => templateCurlyFt(t)
              case _ => beforeDefRhs
            }
            val indent = beforeBody.fold(style.indent.main) { y =>
              val ob = OptionalBraces.get(y).nonEmpty
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
            val expire = templateDerivesOrCurlyOrLastNonTrivial(template)
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

      // Parameter opening for one parameter group. This format works
      // on the WHOLE defnSite (via policies)
      case FormatToken(LeftParenOrBracket(), _, _)
          if style.verticalMultiline.atDefnSite &&
            isParamClauseSite(leftOwner) => verticalMultiline(ft)(style)

      // Term.Apply and friends
      case FormatToken(lp: T.LeftParen, _, LambdaAtSingleArgCallSite(lambda)) =>
        val close = matching(lp)
        val newlinePolicy = Policy ? style.danglingParentheses.callSite &&
          decideNewlinesOnlyBeforeClose(close)
        val noSplitMod =
          if (
            style.newlines.alwaysBeforeCurlyLambdaParams ||
            getMustDangleForTrailingCommas(justBefore(close))
          ) null
          else getNoSplit(ft, true)

        def multilineSpaceSplit(implicit fileLine: FileLine): Split = {
          val lambdaLeft: Option[T] = matchingOpt(functionExpire(lambda)._1)
            .filter(_.is[T.LeftBrace])

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
            .withOptimalToken(lambdaToken)
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
            Split(noSplitMod, 0).withSingleLine(close),
            if (noMultiline) Split.ignored else multilineSpaceSplit,
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
        val close = matching(open)
        val afterClose = tokens(close)
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
        val onlyConfigStyle = mustForceConfigStyle(ft) ||
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

        val indent =
          if (anyDefnSite) Num(style.indent.getDefnSite(leftOwner))
          else Num(style.indent.callSite)

        val isBeforeOpenParen =
          if (defnSite) style.newlines.isBeforeOpenParenDefnSite
          else style.newlines.isBeforeOpenParenCallSite
        val optimal: T =
          if (isBeforeOpenParen) close
          else if (!defnSite || isBracket) rhsOptimalToken(afterClose)
          else defnSiteLastToken(afterClose, leftOwner)

        val wouldDangle = onlyConfigStyle || mustDangleForTrailingCommas ||
          clauseSiteFlags.dangleCloseDelim ||
          closeBreak && beforeClose.left.is[T.Comment]
        val optimalIsComment = optimal.is[T.Comment]

        val newlinePolicy: Policy = Policy ?
          (wouldDangle || optimalIsComment) &&
          decideNewlinesOnlyBeforeClose(close)

        // covers using as well
        val handleImplicit = !tupleSite &&
          (if (onlyConfigStyle) opensConfigStyleImplicitParamList(ft)
           else hasImplicitParamList(rightOwner))

        val noSplitMod =
          if (
            style.newlines.keepBreak(newlines) || {
              if (!handleImplicit) onlyConfigStyle
              else style.newlines.forceBeforeImplicitParamListModifier
            }
          ) null
          else getNoSplit(ft, !isBracket)

        val rightIsComment = right.is[T.Comment]
        val noSplitIndent = if (rightIsComment) indent else Num(0)

        val align = !rightIsComment && clauseSiteFlags.alignOpenDelim &&
          (!handleImplicit || style.newlines.forceAfterImplicitParamListModifier)
        val alignTuple = align && tupleSite && !onlyConfigStyle

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
                .withPolicy(newlinePolicy).withIndent(indent, close, Before),
              Split(NoSplit, nestedPenalty).withSingleLineNoOptimal(breakToken)
                .andPolicy(newlinePolicy & newlineAfterAssignDecision),
            )
          }

        def isExcludedTree(tree: Tree): Boolean = tree match {
          case t: Init => t.argClauses.nonEmpty
          case t: Term.Apply => t.argClause.nonEmpty
          case t: Term.ApplyType => t.argClause.nonEmpty
          case t: Term.Match => t.cases.nonEmpty
          case t: Type.Match => t.cases.nonEmpty
          case t: Term.New => t.init.argClauses.nonEmpty
          case _: Term.NewAnonymous => true
          case _ => false
        }

        val excludeBlocks =
          if (isBracket) {
            val excludeBeg = if (align) getHead(args.last) else ft
            insideBlock[T.LeftBracket](excludeBeg, close)
          } else if (
            multipleArgs ||
            (!isSingleEnclosedArgument || leftOwnerIsEnclosed) &&
            style.newlines.source.eq(Newlines.unfold)
          ) TokenRanges.empty
          else if (
            style.newlines.source.eq(Newlines.fold) && {
              isSingleEnclosedArgument ||
              singleArgument && isExcludedTree(onlyArgument)
            }
          )
            if (onlyArgument eq leftOwner) TokenRanges(TokenRange(open, close))
            else parensTuple(getLast(onlyArgument).left)
          else insideBracesBlock(ft, close)

        def singleLine(newlinePenalty: Int)(implicit fileLine: FileLine): Policy =
          if (multipleArgs && (isBracket || excludeBlocks.ranges.isEmpty))
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

        val keepNoNL = style.newlines.keepBreak(noBreak())
        val preferNoSplit = keepNoNL && singleArgument
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
              .withIndent(extraOneArgPerLineIndent)
              .withIndent(indent, close, Before),
          )
          else {
            val noSplitPolicy =
              if (preferNoSplit && splitsForAssign.isEmpty) singleLine(2)
              else if (
                wouldDangle || optimalIsComment && isBracket ||
                sourceIgnored && configStyleFlag && !isSingleEnclosedArgument
              ) SingleLineBlock(
                close,
                exclude = excludeBlocks,
                noSyntaxNL = multipleArgs,
              )
              else if (splitsForAssign.isDefined) singleLine(3)
              else singleLine(10)
            Seq(
              Split(noSplitMod, 0, policy = noSplitPolicy)
                .notIf(mustDangleForTrailingCommas).withOptimalToken(optimal)
                .withIndent(noSplitIndent, close, Before),
              Split(noSplitMod, (implicitPenalty + lhsPenalty) * bracketCoef)
                .withPolicy(oneArgOneLine & implicitPolicy).onlyIf(
                  (notTooManyArgs && align) ||
                    (handleImplicit &&
                      style.newlines.notBeforeImplicitParamListModifier),
                ).withIndents(
                  if (align) getOpenParenAlignIndents(close)
                  else Seq(Indent(indent, close, ExpiresOn.Before)),
                ),
            )
          }

        val splitsNL =
          if (
            alignTuple ||
            !(onlyConfigStyle || multipleArgs || splitsForAssign.isEmpty)
          ) Seq.empty
          else {
            val cost =
              if (onlyConfigStyle) if (splitsNoNL.isEmpty) 0 else 1
              else (if (preferNoSplit) Constants.ExceedColumnPenalty else 0) +
                bracketCoef * (nestedPenalty + (if (multipleArgs) 2 else 0))
            val split =
              if (multipleArgs) Split(Newline, cost, policy = oneArgOneLine)
                .withIndent(extraOneArgPerLineIndent)
              else {
                val noSplit = !onlyConfigStyle && right.is[T.LeftBrace]
                val noConfigStyle = noSplit || newlinePolicy.isEmpty ||
                  !configStyleFlag
                Split(NoSplit.orNL(noSplit), cost, policy = newlinePolicy)
                  .andPolicy(Policy ? noConfigStyle && singleLine(4)).andPolicy(
                    Policy ? singleArgument && asInfixApp(args.head)
                      .map(InfixSplits(_, ft).nlPolicy),
                  )
              }
            Seq(split.withIndent(indent, close, Before))
          }

        splitsNoNL ++ splitsNL ++ splitsForAssign.getOrElse(Seq.empty)

      case FormatToken(open @ LeftParenOrBracket(), right, _)
          if style.binPack.defnSiteFor(open) != BinPack.Site.Never &&
            isParamClauseSite(leftOwner) =>
        val close = matching(open)
        val noSplitMod = Space(style.spaces.inParentheses)
        if (close eq right) Seq(Split(noSplitMod, 0))
        else {
          implicit val clauseSiteFlags = ClauseSiteFlags.atDefnSite(leftOwner)

          val isBracket = open.is[T.LeftBracket]
          val bracketPenalty =
            if (isBracket) Some(Constants.BracketPenalty) else None
          val penalizeBrackets = bracketPenalty
            .map(p => PenalizeAllNewlines(close, p + 3))
          val afterClose = after(close)

          val nextCommaOneline = argumentStarts.get(ft.meta.idx).flatMap { x =>
            val noNeed = isSeqSingle(getArgs(leftOwner)) ||
              !style.binPack.defnSiteFor(isBracket).isOneline
            if (noNeed) None else findFirstOnRight[T.Comma](getLast(x), close)
          }

          val flags =
            getBinpackSiteFlags(ft, prev(afterClose), literalArgList = false)
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
                      if isParamClauseSite(m.leftOwner) && styleMap.at(o)
                        .binPack.bracketDefnSite != BinPack.Site.Never =>
                    if (isRightCommentThenBreak(ftd)) s
                    else s.map(x => if (x.isNL) x.withPenalty(p) else x)
                }
              }
              getNoSplit(nextCommaOneline.map(endOfSingleLineBlock))
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
            if (clauseSiteFlags.alignOpenDelim) getOpenParenAlignIndents(close)
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
        val close = matching(open)
        val beforeClose = justBefore(close)
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

        def findComma(ft: FormatToken) = findFirstOnRight[T.Comma](ft, close)
          .map(_.right)

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
          policyWithExclude(exclude, Policy.End.Before, Policy.End.On)(
            new PenalizeAllNewlines(Policy.End == close, newlinesPenalty),
          )

        val (onelineCurryToken, onelinePolicy) = afterFirstArgOneline
          .map(BinPackOneline.getPolicy(true, sjsExclude))
          .getOrElse((None, NoPolicy))

        def baseNoSplit(implicit fileLine: FileLine) =
          Split(Space(style.spaces.inParentheses), 0)
        val noNLPolicy = flags.noNLPolicy

        val noSplit =
          if (nlOnly) Split.ignored
          else if (singleLineOnly || noNLPolicy == null) baseNoSplit
            .withSingleLine(close, sjsExclude, noSyntaxNL = true)
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
              } else if (clauseSiteFlags.alignOpenDelim)
                getOpenParenAlignIndents(close)
              else Seq(indent)

            val nextComma =
              if (!oneline) findComma(ft)
              else if (isSingleArg) None
              else afterFirstArgOneline.map(_.right)
            val opt = nextComma.getOrElse(scalaJsOptClose(beforeClose, flags))

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
            baseNoSplit.withOptimalToken(opt)
              .withPolicy(noSplitPolicy & indentPolicy & noNLPolicy())
              .withIndents(noSplitIndents)
          }

        val nlClosedOnOpenEffective = {
          val nlClosedOnOpenOk = onelineCurryToken.forall(x =>
            if (x.is[T.Dot]) onelinePolicy.nonEmpty else flags.scalaJsStyle,
          )
          val res =
            if (nlClosedOnOpenOk) nlCloseOnOpen
            else if (clauseSiteFlags.configStyle.prefer) NlClosedOnOpen.Cfg
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
          val ob = OptionalBraces.get(nextAfterNonComment(expireFt)).nonEmpty
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
          val penalizeNewlines =
            PenalizeAllNewlines(expire, Constants.BracketPenalty)
          val indent = style.indent.getDefnSite(leftOwner)
          Seq(
            Split(sameLineSplit, 0).withPolicy(penalizeNewlines),
            // Spark style guide allows this:
            // https://github.com/databricks/scala-style-guide#indent
            Split(Newline, Constants.SparkColonNewline)
              .withIndent(indent, expire, After).withPolicy(penalizeNewlines),
          )
        }
      case FormatToken(_: T.Colon, _, ColonDeclTpeLeft(returnType))
          if style.newlines.avoidInResultType =>
        val expire = getLastNonTrivialToken(returnType)
        val policy = PenalizeAllNewlines(expire, Constants.ShouldBeNewline)
        Seq(Split(Space, 0).withPolicy(policy).withOptimalToken(expire))

      case FormatToken(T.LeftParen(), T.LeftBrace(), _) => Seq(Split(NoSplit, 0))

      case FormatToken(_, T.LeftBrace(), _) if isXmlBrace(rightOwner) =>
        withIndentOnXmlSpliceStart(ft, Seq(Split(NoSplit, 0)))

      case FormatToken(T.RightBrace(), _, _) if isXmlBrace(leftOwner) =>
        Seq(Split(NoSplit, 0))
      // non-statement starting curly brace
      case FormatToken(_: T.Comma, open: T.LeftBrace, _)
          if !style.poorMansTrailingCommasInConfigStyle &&
            isArgClauseSite(leftOwner) =>
        val close = matching(open)
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
            val breakAfter = rhsOptimalToken(next(nextNonCommentSameLine(ft)))
            val multiLine = decideNewlinesOnlyAfterToken(breakAfter) ==>
              decideNewlinesOnlyBeforeClose(close)
            Seq(
              Split(Newline, 0).withSingleLine(close, killOnFail = true),
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
      case FormatToken(_: T.KwMatch, _, _) =>
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
                  .exists(isTokenLastOrAfter(_, roPos))
            }
          } => Seq(Split(Space, 0))

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
          else argumentStarts.get(ft.meta.idx).map { nextArg =>
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
                  .withOptimalToken(end)
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
                else style.newlines.source eq Newlines.unfold
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

      case FormatToken(_, T.Semicolon(), _) => Seq(Split(NoSplit, 0))
      case FormatToken(_: T.KwReturn, _, _) =>
        val mod =
          if (hasBlankLine) Newline2x
          else leftOwner match {
            case Term.Return(unit: Lit.Unit) if unit.tokens.isEmpty =>
              // Always force blank line for Unit "return".
              Newline
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
        getSplitsForTypeBounds(ft, noNLMod, tp, _.cbounds)
      case FormatToken(_, _: T.Viewbound, _) if rightOwner.is[Type.Param] =>
        val tp = rightOwner.asInstanceOf[Type.Param]
        getSplitsForTypeBounds(ft, Space, tp, _.vbounds)
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
        getSplitsForTypeBounds(ft, Space, typeOwner, boundEnd)

      case FormatToken(left, _: T.Colon, _) =>
        val mod = left match {
          case ident: T.Ident => identModification(ident)
          case _ => NoSplit
        }
        Seq(Split(mod, 0))

      case FormatToken(_, _: T.Dot, _)
          if style.newlines.source.ne(Newlines.keep) &&
            rightOwner.is[Term.Select] && findTreeWithParent(rightOwner) {
              case _: Term.ArgClause => None
              case _: Type.Select | _: Importer | _: Pkg => Some(true)
              case _: Term.Select | _: Member.Apply => None
              case _ => Some(false)
            }.isDefined => Seq(Split(NoSplit, 0))

      case FormatToken(left, _: T.Dot, GetSelectLike.OnRight(thisSelect)) =>
        val enclosed = style.encloseSelectChains
        val (expireTree, nextSelect) =
          findLastApplyAndNextSelect(rightOwner, enclosed)
        val (prevSelect, prevApply) =
          findPrevSelectAndApply(thisSelect.qual, enclosed)
        val nlOnly = prevApply.exists(_.argClause.tokens.head.is[T.Colon])
        val expire = getLastEnclosedToken(expireTree)
        val indentLen = style.indent.main

        def checkFewerBraces(tree: Tree) = tree match {
          case p: Term.Apply => isFewerBraces(p)
          case p: Term.Match => !tokenBefore(p.cases).left.is[T.LeftBrace]
          case p: Term.NewAnonymous => templateCurly(p.templ)
              .exists(_.is[T.Colon])
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
                val minCost = math.max(0, filtered.map(_.cost).min - 1)
                filtered.map { x =>
                  val p = x.policy.filter(!_.isInstanceOf[PenalizeAllNewlines])
                  x.copy(cost = x.cost - minCost, policy = p)
                }
              }
          }
        }
        val ftAfterRight = tokens(ft, 2)
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
              NewlineT(alt = Some(ModExt(NoSplit).withIndentOpt(altIndent)))
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
              def slbPolicy(implicit fileLine: FileLine) = {
                val exclude = // allow newlines in final {} block
                  if (chainExpire.is[T.RightBrace]) parensTuple(chainExpire)
                  else TokenRanges.empty
                SingleLineBlock(chainExpire, exclude, noSyntaxNL = true)
              }
              val newlinePolicy = breakOnNextDot & penalizeBreaks
              val ignoreNoSplit = nlOnly ||
                hasBreak &&
                (left.is[T.Comment] || style.optIn.breakChainOnFirstMethodDot)
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
                if (style.optIn.breakChainOnFirstMethodDot && noBreak) 3 else 2
              val nlCost = nlBaseCost + nestedPenalty + chainLengthPenalty
              val nlMod = getNlMod
              Seq(
                Split(!prevChain, 1) { // must come first, for backwards compat
                  if (style.optIn.breaksInsideChains) NoSplit.orNL(noBreak)
                  else nlMod
                }.withPolicy(newlinePolicy)
                  .onlyFor(SplitTag.SelectChainSecondNL),
                Split(ignoreNoSplit, 0)(NoSplit)
                  .withPolicy(slbPolicy, prevChain).andPolicy(penalizeBreaks),
                Split(if (ignoreNoSplit) Newline else nlMod, nlCost)
                  .withPolicy(newlinePolicy),
              )
            } else {
              val isComment = left.is[T.Comment]
              val doBreak = nlOnly || isComment && hasBreak
              Seq(
                Split(!prevChain, 1) {
                  if (style.optIn.breaksInsideChains) NoSplit.orNL(noBreak)
                  else if (doBreak) Newline
                  else getNlMod
                }.withPolicy(breakOnNextDot)
                  .onlyFor(SplitTag.SelectChainSecondNL),
                Split(if (doBreak) Newline else Space(isComment), 0),
              )
            }

          case _ if left.is[T.Comment] => Seq(Split(Space.orNL(noBreak), 0))

          case Newlines.keep =>
            if (noBreak) Seq(
              Split(NoSplit, 0),
              Split(Newline, Constants.ExceedColumnPenalty * 3),
            )
            else Seq(Split(Newline, 0))

          case Newlines.unfold =>
            val nlCost = if (nlOnly) 0 else 1
            if (prevSelect.isEmpty && nextSelect.isEmpty)
              Seq(Split(nlOnly, 0)(NoSplit), Split(Newline, nlCost))
            else Seq(
              Split(nlOnly, 0)(NoSplit)
                .withSingleLine(expire, noSyntaxNL = true),
              Split(NewlineT(alt = Some(NoSplit)), nlCost)
                .withPolicy(forcedBreakOnNextDotPolicy),
            )

          case Newlines.fold =>
            val nlCost = if (nlOnly) 0 else 1
            val noSplitBase = Split(nlOnly, 0)(NoSplit)
            val nlSplitBase = Split(NewlineT(alt = Some(NoSplit)), nlCost)
            if (nextDotIfSig.isEmpty) {
              val noSplit =
                if (nlOnly) noSplitBase
                else {
                  val end = nextSelect
                    .fold(expire)(x => getLastNonTrivialToken(x.qual))
                  def exclude = insideBracesBlock(ft, end, true)
                  noSplitBase.withSingleLine(end, exclude)
                }
              Seq(noSplit, nlSplitBase)
            } else {
              val policy: Policy = forcedBreakOnNextDotPolicy
              Seq(noSplitBase.withPolicy(policy), nlSplitBase.withPolicy(policy))
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
          getLastToken(rightOwner),
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
          if (template.early.nonEmpty) template.inits.nonEmpty
          else template.inits.lengthCompare(1) > 0,
        )

      // trait A extends B, C, D, E
      case FormatToken(_: T.Comma, _, _) if leftOwner.is[Template] =>
        val template = leftOwner.asInstanceOf[Template]
        typeTemplateSplits(template, style.indent.commaSiteRelativeToExtends)

      case FormatToken(_, r: T.KwWith, _) => rightOwner match {
          // something like new A with B with C
          case template: Template if template.parent.exists { p =>
                p.is[Term.New] || p.is[Term.NewAnonymous] || p.is[Defn.Given]
              } =>
            binPackParentConstructorSplits(
              template.inits.lift(1).exists(_.pos.start > r.start),
              Set(template),
              findTemplateGroupOnRight(_.superType)(template),
              templateCurlyOrLastNonTrivial(template),
              style.indent.main,
              template.inits.lengthCompare(1) > 0,
            )
          // trait A extends B with C with D with E
          case template: Template =>
            typeTemplateSplits(template, style.indent.withSiteRelativeToExtends)
          case t @ WithChain(top) => binPackParentConstructorSplits(
              !t.lhs.is[Type.With],
              withChain(top).toSet,
              Some(t.rhs),
              top.tokens.last,
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
            case _: Term.If | _: Term.While | _: Term.For | _: Term.ForYield =>
              !isTokenHeadOrBefore(open, leftOwner)
            case _ => false
          }) =>
        val close = matching(open)
        val indentLen = style.indent.ctrlSite.getOrElse(style.indent.callSite)
        def indents =
          if (style.align.openParenCtrlSite) getOpenParenAlignIndents(close)
          else Seq(Indent(indentLen, close, ExpiresOn.Before))
        val penalizeNewlines = penalizeNewlineByNesting(open, close)
        if (style.danglingParentheses.ctrlSite) {
          val noSplit =
            if (style.align.openParenCtrlSite) Split(NoSplit, 0)
              .withIndents(indents).withOptimalToken(close)
              .withPolicy(penalizeNewlines)
              .andPolicy(decideNewlinesOnlyBeforeCloseOnBreak(close))
            else Split(NoSplit, 0).withSingleLine(close)
          Seq(
            noSplit,
            Split(Newline, 1).withIndent(indentLen, close, Before)
              .withPolicy(penalizeNewlines)
              .andPolicy(decideNewlinesOnlyBeforeClose(close)),
          )
        } else Seq(
          Split(NoSplit, 0).withIndents(indents).withPolicy(penalizeNewlines),
        )
      case FormatToken(_: T.KwIf, right, _) if leftOwner.is[Term.If] =>
        val owner = leftOwner.asInstanceOf[Term.If]
        val expire = getLastToken(owner)
        val isKeep = style.newlines.source eq Newlines.keep
        val mod =
          if (isKeep && hasBreak()) Newline
          else Space(style.spaces.isSpaceAfterKeyword(right))
        val slb = Split(mod.isNL, 0)(mod)
          .withSingleLine(expire, killOnFail = true)
        val mlSplitBase = Split(mod, if (slb.isIgnored) 0 else 1).withPolicy(
          if (isKeep) getBreakBeforeElsePolicy(owner)
          else getBreaksBeforeElseChainPolicy(owner),
        )
        val mlSplitOpt = OptionalBraces
          .indentAndBreakBeforeCtrl[T.KwThen](owner.cond, mlSplitBase)
        Seq(slb, mlSplitOpt.getOrElse(mlSplitBase))
      case FormatToken(_: T.KwWhile | _: T.KwFor, right, _) =>
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
          case Term.For(List(enum), _) => OptionalBraces
              .indentAndBreakBeforeCtrl[T.KwDo](enum, splitBase)
          case Term.ForYield(List(enum), _) => OptionalBraces
              .indentAndBreakBeforeCtrl[T.KwYield](enum, splitBase)
          case _ => None
        }).getOrElse(Split(spaceMod, 0))
        Seq(split)
      case FormatToken(close: T.RightParen, right, _) if (leftOwner match {
            case _: Term.If => !nextNonComment(ft).right.is[T.KwThen]
            case _: Term.ForYield => style.indentYieldKeyword
            case _: Term.While | _: Term.For =>
              !nextNonComment(ft).right.is[T.KwDo]
            case _ => false
          }) && !isTokenLastOrAfter(close, leftOwner) =>
        val body = leftOwner match {
          case t: Term.If => t.thenp
          case t: Term.For => t.body
          case t: Term.ForYield => t.body
          case t: Term.While => t.body
        }
        val expire = getLastToken(body)
        def nlSplitFunc(cost: Int)(implicit l: sourcecode.Line) =
          Split(Newline, cost).withIndent(style.indent.main, expire, After)
        if (style.newlines.getBeforeMultiline eq Newlines.unfold) CtrlBodySplits
          .checkComment(ft, nlSplitFunc) { ft =>
            if (ft.right.is[T.LeftBrace]) {
              val nextFt = nextNonCommentSameLineAfter(ft)
              val policy = decideNewlinesOnlyAfterToken(nextFt.left)
              Seq(Split(Space, 0, policy = policy))
            } else Seq(nlSplitFunc(0))
          }
        else CtrlBodySplits.get(ft, body) {
          Split(Space, 0).withSingleLineNoOptimal(
            expire,
            insideBracesBlock(ft, expire),
            noSyntaxNL = leftOwner.is[Term.ForYield] && right.is[T.KwYield],
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
        if (style.newlines.sourceIgnored || noBreak()) Seq(
          Split(Space, 0)
            .withOptimalToken(nextNonCommentSameLineAfter(ft).left),
          Split(Newline, 1),
        )
        else Seq(Split(Newline, 0))
      // Last else branch
      case FormatToken(_: T.KwElse, _, _) if (leftOwner match {
            case t: Term.If => t.elsep match {
                case _: Term.If => false
                case b @ Term.Block(List(_: Term.If)) =>
                  matchingOpt(nextNonComment(ft).right)
                    .exists(_.end >= b.pos.end)
                case _ => true
              }
            case x => throw new UnexpectedTree[Term.If](x)
          }) =>
        val body = leftOwner.asInstanceOf[Term.If].elsep
        val expire = getLastToken(body)
        def nlSplitFunc(cost: Int) = Split(Newline, cost)
          .withIndent(style.indent.main, expire, After)
        if (style.newlines.getBeforeMultiline eq Newlines.unfold)
          Seq(nlSplitFunc(0))
        else CtrlBodySplits.get(ft, body) {
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
        val close = matching(open)
        val beforeClose = justBefore(close)
        implicit val clauseSiteFlags = ClauseSiteFlags.atCallSite(leftOwner)
        val isConfig = couldPreserveConfigStyle(ft, beforeClose.hasBreak)

        val enclosed = findEnclosedBetweenParens(open, close, leftOwner)
        def spaceSplitWithoutPolicy(implicit fileLine: FileLine) = {
          val indent: Length = right match {
            case T.KwIf() => StateColumn
            case T.KwFor() if !style.indentYieldKeyword => StateColumn
            case _ =>
              val isInfix = enclosed.exists {
                case _: Term.ApplyInfix => true
                case Term.ArgClause((_: Term.ApplyInfix) :: Nil, _) => true
                case _ => false
              }
              if (isInfix) Num(0)
              else {
                val willBreak = beforeClose.left.is[T.Comment] &&
                  prevNonCommentSameLine(beforeClose).hasBreak
                Num(if (willBreak) style.indent.main else 0)
              }
          }
          val useSpace = style.spaces.inParentheses || right.is[T.Comment]
          Split(Space(useSpace), 0).withIndent(indent, close, Before)
        }
        def spaceSplit(implicit fileLine: FileLine) = spaceSplitWithoutPolicy
          .withPolicy(PenalizeAllNewlines(close, 1))
        def newlineSplit(cost: Int, forceDangle: Boolean)(implicit
            fileLine: FileLine,
        ) = {
          val shouldDangle = forceDangle || clauseSiteFlags.dangleCloseDelim
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
              style.newlines.source.eq(Newlines.unfold) && x.parent.exists {
                case _: Template | _: Defn | _: Member.Infix => false
                case _ => true
              }
            }
            Seq(
              if (!singleLine) spaceSplit
              else spaceSplitWithoutPolicy.withSingleLine(close).andPolicy(
                Policy ? !enclosed.exists(isInfixApp) ||
                  getSingleLineInfixPolicy(close),
              ),
              newlineSplit(10, true),
            )
        }

      // Infix operator.
      case FormatToken(_: T.Ident, _, FormatToken.LeftOwner(AsInfixOp(app))) =>
        insideInfixSplit(app, ft)
      case FormatToken(_, _: T.Ident, FormatToken.RightOwner(AsInfixOp(app))) =>
        insideInfixSplit(app, ft)

      // Case
      case FormatToken(_: T.KwCase, _, _)
          if leftOwner.is[Defn.RepeatedEnumCase] =>
        val indent =
          Indent(style.indent.main, leftOwner.tokens.last, ExpiresOn.After)
        Seq(Split(Space, 0).withIndent(indent))

      case FormatToken(_: T.KwCase, _, _) if leftOwner.is[CaseTree] =>
        val owner = leftOwner.asInstanceOf[CaseTree]
        val body = owner.body
        val arrowFt = leftOwner match {
          case c: Case => getCaseArrow(c)
          case tc: TypeCase => getCaseArrow(tc)
        }
        val arrow = arrowFt.left
        val postArrowFt = nextNonCommentSameLine(arrowFt)
        val bodyEnd = getLastNonTrivialOpt(body)
        val expire = bodyEnd.fold(postArrowFt)(nextNonCommentSameLine).left

        val bodyBlock = isCaseBodyABlock(arrowFt, owner)
        def defaultPolicy = decideNewlinesOnlyAfterToken(postArrowFt.left)
        val policy =
          if (bodyBlock || isAttachedCommentThenBreak(arrowFt)) NoPolicy
          else if (isCaseBodyEnclosedAsBlock(postArrowFt, owner)) {
            val postParenFt = nextNonCommentSameLineAfter(postArrowFt)
            val lparen = postParenFt.left
            val rparen = matching(lparen)
            if (postParenFt.right.start >= rparen.start) defaultPolicy
            else {
              val indent = style.indent.main
              val lindents = Seq(
                Indent(indent, rparen, Before),
                Indent(-indent, expire, After),
              )
              val lmod = NewlineT(noIndent = rhsIsCommentedOut(postParenFt))
              val lsplit = Seq(Split(lmod, 0).withIndents(lindents))
              val rsplit = Seq(Split(Newline, 0))
              Policy.after(lparen, prefix = "CASE[(]") {
                case d: Decision if d.formatToken eq postParenFt => lsplit
              } ==> Policy.on(rparen, prefix = "CASE[)]") {
                case d: Decision if d.formatToken.right eq rparen => rsplit
              }
            }
          } else Policy ? style.newlines.getBeforeMultiline.in(
            Newlines.fold,
            Newlines.keep,
          ) || defaultPolicy

        val bodyIndent = if (bodyBlock) 0 else style.indent.main
        val arrowIndent = style.indent.caseSite - bodyIndent
        val indents = Seq(
          Indent(bodyIndent, expire, After),
          Indent(arrowIndent, arrow, After),
        )
        val mod = ModExt(Space, indents)
        Seq(
          Split(mod, 0).withSingleLine(expire, killOnFail = true),
          Split(mod, 0, policy = policy),
        )

      case FormatToken(_, cond: T.KwIf, _) if rightOwner.is[Case] =>
        if (style.newlines.keepBreak(newlines)) Seq(Split(Newline, 0))
        else {
          val arrow = getCaseArrow(rightOwner.asInstanceOf[Case]).left
          val afterIf = nextNonCommentSameLineAfter(ft)
          val noSplit =
            if (style.newlines.keepBreak(afterIf)) {
              val indent = Indent(style.indent.main, arrow, ExpiresOn.Before)
              Split(Space, 0).withSingleLine(afterIf.left).withIndent(indent)
            } else {
              val exclude = insideBracesBlock(ft, arrow)
              Split(Space, 0).withSingleLineNoOptimal(arrow, exclude = exclude)
            }
          Seq(
            noSplit,
            Split(Newline, 1).withPolicy(penalizeNewlineByNesting(cond, arrow)),
          )
        }

      case FormatToken(_: T.KwIf, _, _) if leftOwner.is[Case] =>
        val useNL = style.newlines.keepBreak(nextNonCommentSameLine(ft))
        Seq(Split(Space.orNL(!useNL), 0))

      // ForYield
      case FormatToken(
            _: T.LeftArrow | _: T.Equals,
            _,
            EnumeratorAssignRhsOnLeft(rhs),
          ) => getSplitsEnumerator(ft, rhs)

      case FormatToken(
            kw @ (_: T.KwTry | _: T.KwCatch | _: T.KwFinally),
            _,
            _,
          ) =>
        val body = leftOwner match {
          case t: Term.Try => kw match {
              case _: T.KwTry => t.expr
              case _: T.KwCatch => t
              case _: T.KwFinally => t.finallyp.getOrElse(t)
              case _ => t
            }
          case t: Term.TryWithHandler => kw match {
              case _: T.KwTry => t.expr
              case _: T.KwCatch => t.catchp
              case _: T.KwFinally => t.finallyp.getOrElse(t)
              case _ => t
            }
          case t => t
        }
        val end = getLastToken(body)
        val indent = Indent(style.indent.main, end, ExpiresOn.After)
        CtrlBodySplits.get(ft, body, Seq(indent)) {
          Split(Space, 0).withSingleLineNoOptimal(end)
        }(Split(Newline, _).withIndent(indent))

      // Term.ForYield
      case FormatToken(T.KwYield(), _, _) if leftOwner.is[Term.ForYield] =>
        val lastToken = getLastToken(leftOwner.asInstanceOf[Term.ForYield].body)
        val indent = Indent(style.indent.main, lastToken, ExpiresOn.After)
        if (style.newlines.avoidAfterYield && !rightOwner.is[Term.If]) {
          val nextFt = nextNonCommentSameLine(ft)
          val noIndent = nextFt.eq(ft) || nextFt.noBreak
          Seq(Split(Space, 0).withIndent(indent, noIndent))
        } else Seq(
          // Either everything fits in one line or break on =>
          Split(Space, 0).withSingleLineNoOptimal(lastToken),
          Split(Newline, 1).withIndent(indent),
        )

      // Term.For
      case FormatToken(rb: T.RightBrace, _, _)
          if leftOwner.is[Term.For] && !isTokenLastOrAfter(rb, leftOwner) &&
            !nextNonComment(ft).right.is[T.KwDo] =>
        val body = leftOwner.asInstanceOf[Term.For].body
        def nlSplit(cost: Int) = Split(Newline, cost)
          .withIndent(style.indent.main, getLastToken(body), After)
        CtrlBodySplits.get(ft, body)(null)(nlSplit)

      // After comment
      case FormatToken(_: T.Comment, _, _) => Seq(Split(getMod(ft), 0))
      // Inline comment
      case FormatToken(_, _: T.Comment, _) =>
        val forceBlankLine = hasBreak() && blankLineBeforeDocstring(ft)
        val mod = if (forceBlankLine) Newline2x else getMod(ft)
        val baseSplit = Split(mod, 0)
        rightOwner match {
          case GetSelectLike(t) =>
            val expire = nextNonCommentAfter(ft).left
            val indent = Indent(style.indent.main, expire, ExpiresOn.After)
            val split = baseSplit.withIndent(indent)
            if (findPrevSelect(t, style.encloseSelectChains).isEmpty) Seq(split)
            else Seq(baseSplit, split.onlyFor(SplitTag.SelectChainFirstNL))
          case _ => Seq(baseSplit)
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

      case FormatToken(_, r, _) if optionalNewlines(ft.meta.idx) =>
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
        if (style.newlines.source eq Newlines.keep)
          Seq(Split(Space.orNL(noBreak()), 0))
        else Seq(Split(Space, 0), Split(Newline, 1))
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
      case FormatToken(_, close: T.RightParen, _) =>
        def modNoNL = {
          def allowSpace = rightOwner match {
            case _: Term.If | _: Term.While | _: Term.For | _: Term.ForYield =>
              isTokenLastOrAfter(close, rightOwner)
            case _ => true
          }
          Space(style.spaces.inParentheses && allowSpace)
        }
        val isNL = rightOwner.is[Pat.Alternative] &&
          style.newlines.keepBreak(newlines)
        Seq(Split(if (isNL) Newline else modNoNL, 0))

      case FormatToken(left, _: T.KwCatch | _: T.KwFinally, _)
          if style.newlines.alwaysBeforeElseAfterCurlyIf ||
            !left.is[T.RightBrace] || !(leftOwner.eq(rightOwner) ||
              leftOwner.is[Term.Block] &&
              leftOwner.parent.contains(rightOwner)) =>
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
      ft: FormatToken,
      body: Tree,
      spaceIndents: Seq[Indent] = Seq.empty,
  )(splits: => Seq[Split])(implicit style: ScalafmtConfig): Seq[Split] = {
    def expire = getLastToken(body)
    if (ft.right.is[T.LeftBrace]) // The block will take care of indenting by 2
      Seq(Split(Space, 0).withIndents(spaceIndents))
    else if (isRightCommentWithBreak(ft))
      Seq(CtrlBodySplits.withIndent(Split(Space.orNL(ft.noBreak), 0), ft, body))
    else if (isJsNative(body)) Seq(Split(Space, 0).withSingleLine(expire))
    else if (style.newlines.forceBeforeAssign(ft.meta.leftOwner))
      Seq(CtrlBodySplits.withIndent(Split(Newline, 0), ft, body))
    else if (style.newlines.shouldForceBeforeMultilineAssign(ft.meta.leftOwner))
      CtrlBodySplits.slbOnly(ft, body, spaceIndents) { x =>
        CtrlBodySplits.withIndent(Split(Newline, x), ft, body)
      }
    else splits
  }

  private def getSplitsDefEquals(ft: FormatToken, body: Tree)(implicit
      style: ScalafmtConfig,
  ): Seq[Split] = {
    val expire = getLastToken(body)
    def baseSplit = Split(Space, 0)
    def newlineSplit(cost: Int)(implicit fileLine: FileLine) = CtrlBodySplits
      .withIndent(Split(Newline, cost), ft, body)

    def getClassicSplits =
      if (ft.hasBreak) Seq(newlineSplit(0)) else Seq(baseSplit, newlineSplit(1))

    style.newlines.beforeMultilineDef.fold {
      getSplitsValEquals(ft, body)(getClassicSplits)
    } {
      case Newlines.classic => getClassicSplits

      case Newlines.keep if ft.hasBreak => Seq(newlineSplit(0))

      case Newlines.unfold =>
        Seq(baseSplit.withSingleLine(expire), newlineSplit(1))

      case x => CtrlBodySplits.folded(ft, body, x eq Newlines.keep)(newlineSplit)
    }
  }

  private def getSplitsValEquals(ft: FormatToken, body: Tree)(
      classicSplits: => Seq[Split],
  )(implicit style: ScalafmtConfig): Seq[Split] =
    if (style.newlines.getBeforeMultiline eq Newlines.classic) classicSplits
    else CtrlBodySplits.getWithIndent(ft, body)(null)(Split(Newline, _))

  private def getSplitsValEqualsClassic(ft: FormatToken, body: Tree)(implicit
      style: ScalafmtConfig,
  ): Seq[Split] = {
    def wouldDangle = ft.meta.leftOwner.parent.exists {
      case p: Member.ParamClause => style.danglingParentheses.atDefnSite(p)
      case _: Member.Tuple => style.danglingParentheses.atTupleSite
      case _: Type.ArgClause => style.danglingParentheses.atBracketCallSite
      case p: Member.ArgClause => style.danglingParentheses.callSite &&
        (p.parent match {
          case Some(_: Term.ApplyInfix) => style.newlines.formatInfix &&
            p.values.lengthCompare(1) > 0
          case _ => true
        })
      case _ => false
    }

    val expireFt = getLast(body)
    val expire = expireFt.left
    // rhsOptimalToken is too aggressive here
    val optimalFt = expireFt.right match {
      case _: T.Comma => next(expireFt)
      case RightParenOrBracket() if !wouldDangle => next(expireFt)
      case _ => expireFt
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
      .withOptimalToken(optimal).withPolicy {
        val exclude = insideBracesBlock(ft, expire)
        policyWithExclude(exclude, Policy.End.On, Policy.End.After)(
          PenalizeAllNewlines(expire, Constants.ShouldBeSingleLine),
        )
      }
    val spaceSplit = body match {
      case _ if ft.hasBreak && ft.meta.leftOwner.is[Defn] => Split.ignored
      case _: Term.If => twoBranches
      case _: Term.ForYield => twoBranches
      // we force newlines in try/catch/finally
      case _: Term.Try | _: Term.TryWithHandler => Split.ignored
      case t: Term.Apply if t.argClause.nonEmpty =>
        baseSpaceSplit.withOptimalToken(optimalWithComment)
      case _ => baseSpaceSplit.withOptimalToken(optimal)
    }
    Seq(
      spaceSplit,
      CtrlBodySplits.withIndent(Split(Newline, 1 + penalty), ft, body),
    )
  }

  private def getSplitsEnumerator(ft: FormatToken, body: Tree)(implicit
      style: ScalafmtConfig,
  ): Seq[Split] = maybeGetInfixSplitsBeforeLhs(ft) {
    val expire = getLastNonTrivialToken(body)
    val spaceIndents =
      if (!style.align.arrowEnumeratorGenerator) Seq.empty
      else Seq(Indent(StateColumn, expire, After))
    getSplitsDefValEquals(ft, body, spaceIndents) {
      CtrlBodySplits.get(ft, body, spaceIndents) {
        if (spaceIndents.nonEmpty) Split(Space, 0).withIndents(spaceIndents)
        else {
          val noSlb = body match {
            case _: Term.Try | _: Term.TryWithHandler => false
            case t: Term.If => ifWithoutElse(t)
            case _ => true
          }
          if (noSlb) Split(Space, 0).withOptimalToken(ft.right)
          else Split(Space, 0).withSingleLine(expire)
        }
      }(Split(Newline, _).withIndent(style.indent.main, expire, After))
    }
  }

}
