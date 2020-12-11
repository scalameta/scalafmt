package org.scalafmt.internal

import org.scalafmt.Error.UnexpectedTree
import org.scalafmt.config.{ImportSelectors, Newlines, ScalafmtConfig}
import org.scalafmt.internal.ExpiresOn.{After, Before}
import org.scalafmt.internal.Length.{Num, StateColumn}
import org.scalafmt.internal.Policy.NoPolicy
import org.scalafmt.util._

import scala.language.implicitConversions
import scala.meta.classifiers.Classifier
import scala.meta.tokens.{Token, Tokens}
import scala.meta.tokens.{Token => T}
import scala.meta.{
  Case,
  Defn,
  Enumerator,
  Import,
  Importer,
  Init,
  Lit,
  Mod,
  Pat,
  Pkg,
  Template,
  Term,
  Tree,
  Type
}

object Constants {
  val ShouldBeNewline = 100000
  val ShouldBeSingleLine = 30
  val BinPackAssignmentPenalty = 10
  val SparkColonNewline = 10
  val BracketPenalty = 20
  val ExceedColumnPenalty = 1000
  // Breaking a line like s"aaaaaaa${111111 + 22222}" should be last resort.
  val BreakSingleLineInterpolatedString = 10 * ExceedColumnPenalty
  // when converting new A with B with C to
  // new A
  //   with B
  //   with C
  val IndentForWithChains = 2
}

/** Assigns splits to format tokens.
  *
  * NOTE(olafurpg). The pattern match in this file has gotten out of hand. It's
  * difficult even for myself to keep track of what's going on in some cases,
  * especially around applications and lambdas. I'm hoping to sunset this file
  * along with BestFirstSearch in favor of https://github.com/scalameta/scalafmt/issues/917
  */
class Router(formatOps: FormatOps) {

  import Constants._
  import LoggerOps._
  import PolicyOps._
  import TokenOps._
  import TreeOps._
  import formatOps._

  private def getSplitsImpl(formatToken: FormatToken): Seq[Split] = {
    implicit val style = styleMap.at(formatToken)
    val leftOwner = formatToken.meta.leftOwner
    val rightOwner = formatToken.meta.rightOwner
    val newlines = formatToken.newlinesBetween

    formatToken match {
      case FormatToken(_: T.BOF, right, _) =>
        val policy = right match {
          case T.Ident(name) // shebang in .sc files
              if FileOps.isAmmonite(filename) && name.startsWith("#!") =>
            val nl = findFirst(next(formatToken), Int.MaxValue) { x =>
              x.hasBreak || x.right.is[T.EOF]
            }
            nl.fold(Policy.noPolicy) { ft =>
              Policy.on(ft.left) { case Decision(t, _) =>
                Seq(Split(Space(t.between.nonEmpty), 0))
              }
            }
          case _ => Policy.NoPolicy
        }
        Seq(
          Split(NoSplit, 0).withPolicy(policy)
        )
      case FormatToken(_, _: T.EOF, _) =>
        Seq(
          Split(Newline, 0) // End files with trailing newline
        )
      case FormatToken(start: T.Interpolation.Start, _, _) =>
        val end = matching(start)
        val policy =
          if (isTripleQuote(formatToken.meta.left.text)) NoPolicy
          else PenalizeAllNewlines(end, BreakSingleLineInterpolatedString)
        val split = Split(NoSplit, 0).withPolicy(policy)
        Seq(
          if (getStripMarginChar(formatToken).isEmpty) split
          else if (!style.align.stripMargin) split.withIndent(2, end, After)
          else // statecolumn - 1 because of margin characters |
            split
              .withIndent(StateColumn, end, After)
              .withIndent(-1, end, After)
        )
      // Interpolation
      case FormatToken(
            _: T.Interpolation.Id | _: T.Interpolation.Part |
            _: T.Interpolation.Start | _: T.Interpolation.SpliceStart,
            _,
            _
          ) =>
        Seq(
          Split(NoSplit, 0)
        )
      case FormatToken(
            _,
            T.Interpolation.Part(_) | T.Interpolation.End() |
            T.Interpolation.SpliceEnd(),
            _
          ) =>
        Seq(
          Split(NoSplit, 0)
        )
      case FormatToken(T.LeftBrace(), T.RightBrace(), _) =>
        Seq(
          Split(NoSplit, 0)
        )
      // Import
      case FormatToken(_: T.Dot, _: T.LeftBrace | _: T.Underscore, _)
          if existsParentOfType[Import](rightOwner) =>
        Seq(Split(NoSplit, 0))
      // Import left brace
      case FormatToken(open: T.LeftBrace, _, _)
          if existsParentOfType[Import](leftOwner) =>
        val close = matching(open)
        val policy = SingleLineBlock(
          close,
          okSLC = style.importSelectors eq ImportSelectors.singleLine
        )
        val newlineBeforeClosingCurly = decideNewlinesOnlyBeforeClose(close)

        val newlinePolicy = style.importSelectors match {
          case ImportSelectors.noBinPack =>
            newlineBeforeClosingCurly & splitOneArgOneLine(close, leftOwner)
          case ImportSelectors.binPack =>
            newlineBeforeClosingCurly
          case ImportSelectors.singleLine =>
            SingleLineBlock(close)
        }

        Seq(
          Split(Space(style.spaces.inImportCurlyBraces), 0)
            .withPolicy(policy),
          Split(Newline, 1)
            .onlyIf(style.importSelectors != ImportSelectors.singleLine)
            .withPolicy(newlinePolicy)
            .withIndent(2, close, Before)
        )
      case FormatToken(_, _: T.RightBrace, _)
          if existsParentOfType[Import](rightOwner) =>
        Seq(Split(Space(style.spaces.inImportCurlyBraces), 0))

      // Interpolated string left brace
      case FormatToken(open @ T.LeftBrace(), _, _)
          if leftOwner.is[SomeInterpolate] =>
        Seq(Split(NoSplit, 0))
      case FormatToken(_, close @ T.RightBrace(), _)
          if rightOwner.is[SomeInterpolate] =>
        Seq(Split(NoSplit, 0))

      // { ... } Blocks
      case tok @ FormatToken(open @ T.LeftBrace(), right, between) =>
        val close = matching(open)
        val closeFT = tokens(close)
        val newlineBeforeClosingCurly = decideNewlinesOnlyBeforeClose(close)
        val selfAnnotation: Option[Tokens] = leftOwner match {
          // Self type: trait foo { self => ... }
          case t: Template => Some(t.self.name.tokens).filter(_.nonEmpty)
          case _ => None
        }
        val isSelfAnnotationNL =
          style.optIn.selfAnnotationNewline && selfAnnotation.nonEmpty && (
            formatToken.hasBreak || style.newlines.sourceIgnored
          )
        val nl: Modification =
          if (right.is[T.Comment] && tok.noBreak) Space
          else if (isSelfAnnotationNL)
            getModCheckIndent(formatToken, math.max(newlines, 1))
          else
            NewlineT(tok.hasBlankLine || blankLineBeforeDocstring(tok))

        // lambdaNLOnly: None for single line only
        val (lambdaExpire, lambdaArrow, lambdaIndent, lambdaNLOnly) =
          startsStatement(right) match {
            case Some(owner: Term.Function) =>
              val arrow = getFuncArrow(lastLambda(owner))
              val expire = arrow.getOrElse(tokens(owner.tokens.last))
              val nlOnly =
                if (style.newlines.alwaysBeforeCurlyLambdaParams) Some(true)
                else if (
                  style.newlines.beforeCurlyLambdaParams eq
                    Newlines.BeforeCurlyLambdaParams.multiline
                ) None
                else Some(false)
              (expire, arrow.map(_.left), 0, nlOnly)
            case Some(t: Case) if t.cond.isEmpty && (leftOwner match {
                  case Term.PartialFunction(List(`t`)) => true
                  case _ => false
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
              selfAnnotation match {
                case Some(anno) =>
                  val arrow = leftOwner.tokens.find(_.is[T.RightArrow])
                  val expire = arrow.getOrElse(anno.last)
                  (tokens(expire), arrow, 2, Some(isSelfAnnotationNL))
                case _ =>
                  (null, None, 0, None)
              }
          }
        val lambdaPolicy =
          if (lambdaExpire == null) null
          else {
            val arrowOptimal = getOptimalTokenFor(lambdaExpire)
            newlineBeforeClosingCurly &
              SingleLineBlock(arrowOptimal) &
              decideNewlinesOnlyAfterToken(arrowOptimal)
          }

        def getSingleLineDecision = {
          type Classifiers = Seq[Classifier[Token, _]]
          def classifiersByParent: Classifiers =
            leftOwner.parent match {
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

          val breakSingleLineAfterClose = classifiers.nonEmpty && {
            val afterClose = closeFT.right
            classifiers.exists(_(afterClose))
          }
          if (!breakSingleLineAfterClose) Policy.NoPolicy
          else decideNewlinesOnlyAfterClose(Split(Newline, 0))(close)
        }
        def getClassicSingleLineDecisionOpt =
          if (newlines > 0) None
          else Some(getSingleLineDecision)

        def getSingleLineLambdaDecisionOpt = {
          val ok = !lambdaNLOnly.contains(true) &&
            getSpaceAndNewlineAfterCurlyLambda(newlines)._1
          if (ok) Some(getSingleLineDecision) else None
        }

        // null if skipping
        val singleLineDecisionOpt = style.newlines.source match {
          case Newlines.keep if newlines != 0 => None
          case Newlines.unfold => None
          case Newlines.fold =>
            val isTopLevelBlock =
              leftOwner.parent.exists(_.parent.isEmpty) || (leftOwner match {
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
            else Some(getSingleLineDecision)
          // old behaviour
          case _ =>
            if (lambdaPolicy == null) getClassicSingleLineDecisionOpt
            else getSingleLineLambdaDecisionOpt
        }

        val singleLineSplit =
          singleLineDecisionOpt.fold(Split.ignored) { sld =>
            val expire = endOfSingleLineBlock(closeFT)
            Split(xmlSpace(leftOwner), 0)
              .withSingleLine(expire, noSyntaxNL = true, killOnFail = true)
              .andPolicy(sld)
          }

        val splits = Seq(
          singleLineSplit,
          Split(nl, 1)
            .withPolicy(newlineBeforeClosingCurly)
            .withIndent(2, close, Before),
          Split(Space, 0)
            .onlyIf(lambdaNLOnly.contains(false) && lambdaPolicy != null)
            .notIf(style.newlines.source.eq(Newlines.keep) && newlines != 0)
            .withOptimalTokenOpt(lambdaArrow)
            .withIndent(lambdaIndent, close, Before)
            .withPolicy(lambdaPolicy)
        )
        right match {
          case t: T.Xml.Start => withIndentOnXmlStart(t, splits)
          case _ => splits
        }

      case FormatToken(arrow @ T.RightArrow(), right, _)
          if startsStatement(right).isDefined &&
            leftOwner.isInstanceOf[Term.Function] =>
        val endOfFunction = lastToken(
          leftOwner.asInstanceOf[Term.Function].body
        )
        val canBeSpace =
          startsStatement(right).get.isInstanceOf[Term.Function]
        val (afterCurlySpace, afterCurlyNewlines) =
          getSpaceAndNewlineAfterCurlyLambda(newlines)
        val spaceSplit =
          if (canBeSpace) Split(Space, 0)
          else if (
            afterCurlySpace &&
            (!rightOwner.is[Defn] || style.newlines.source.eq(Newlines.fold))
          )
            Split(Space, 0).withSingleLineNoOptimal(
              getOptimalTokenFor(endOfFunction),
              noSyntaxNL = true
            )
          else Split.ignored
        Seq(
          spaceSplit,
          Split(afterCurlyNewlines, 1).withIndent(2, endOfFunction, After)
        )

      case FormatToken(T.RightArrow(), right, _)
          if leftOwner.is[Term.Function] ||
            (leftOwner.is[Template] &&
              leftOwner.parent.exists(_.is[Term.NewAnonymous])) =>
        val (endOfFunction, expiresOn) = leftOwner match {
          case t: Term.Function => functionExpire(t)
          case t => lastToken(t) -> ExpiresOn.Before
        }

        val hasSingleLineComment = isSingleLineComment(right)
        val indent = // don't indent if the body is empty `{ x => }`
          if (isEmptyFunctionBody(leftOwner) && !right.is[T.Comment]) 0
          else if (leftOwner.is[Template]) 0 // { applied the indent
          else 2

        def noSingleLine = {
          // for constructors with empty args lambda
          // new Foo { () =>
          //   println("wow")
          // }
          val isCurlyLambda =
            leftOwner.is[Template] || leftOwner.parent.exists(_.is[Template])

          def noSquash =
            style.newlines.afterCurlyLambdaParams ne
              Newlines.AfterCurlyLambdaParams.squash

          isCurlyLambda && (style.newlines.source match {
            case Newlines.fold => false
            case Newlines.unfold => noSquash
            case Newlines.keep => newlines != 0
            case Newlines.classic => newlines != 0 && noSquash
          })
        }
        val singleLineSplit =
          Split(Space, 0)
            .notIf(hasSingleLineComment || noSingleLine)
            .withSingleLineNoOptimal(endOfFunction)
        def newlineSplit =
          Split(Newline, 1 + nestedApplies(leftOwner))
            .withIndent(indent, endOfFunction, expiresOn)
        val multiLineSplits =
          if (hasSingleLineComment)
            Seq(newlineSplit)
          else {
            // 2020-01: break after same-line comments, and any open brace
            val nonComment = nextNonCommentSameLine(formatToken)
            val hasBlock = nonComment.right.is[T.LeftBrace] &&
              (matching(nonComment.right) eq endOfFunction)
            if (!hasBlock && (nonComment eq formatToken))
              Seq(newlineSplit)
            else {
              // break after the brace or comment if fits, or now if doesn't
              // if brace, don't add indent, the LeftBrace rule will do that
              val spaceIndent = if (hasBlock) 0 else indent
              Seq(
                Split(Space, 0)
                  .withIndent(spaceIndent, endOfFunction, expiresOn)
                  .withOptimalToken(getOptimalTokenFor(next(nonComment))),
                newlineSplit
              )
            }
          }
        singleLineSplit +: multiLineSplits

      // Case arrow
      case tok @ FormatToken(_: T.RightArrow, _, _) if leftOwner.is[Case] =>
        val caseStat = leftOwner.asInstanceOf[Case]
        val body = caseStat.body
        val bodyIsEmpty = body.tokens.isEmpty
        def baseSplit = Split(Space, 0)
        def nlSplit(ft: FormatToken)(cost: Int)(implicit l: sourcecode.Line) = {
          val noIndent = rhsIsCommentedOut(ft)
          val isDouble = ft.hasBlankLine && bodyIsEmpty
          Split(NewlineT(isDouble = isDouble, noIndent = noIndent), cost)
        }
        CtrlBodySplits.checkComment(tok, nlSplit(tok)) { ft =>
          val beforeMultiline = style.newlines.getBeforeMultiline
          if (isCaseBodyABlock(ft, caseStat)) Seq(baseSplit)
          else if (isCaseBodyEnclosedAsBlock(ft, caseStat)) Seq(baseSplit)
          else if (ft.right.is[T.KwCase]) Seq(nlSplit(ft)(0))
          else if (beforeMultiline eq Newlines.unfold) {
            if (ft.right.is[T.Semicolon]) Seq(baseSplit, nlSplit(ft)(1))
            else Seq(nlSplit(ft)(0))
          } else if (
            ft.hasBreak &&
            beforeMultiline.in(Newlines.classic, Newlines.keep)
          ) Seq(nlSplit(ft)(0))
          else if (bodyIsEmpty) Seq(baseSplit, nlSplit(ft)(1))
          else if (
            caseStat.cond.isDefined ||
            beforeMultiline.eq(Newlines.classic) ||
            (body match {
              case t: Term.Block => t.stats.lengthCompare(1) > 0
              case _ => false
            })
          ) Seq(baseSplit.withSingleLine(lastToken(body)), nlSplit(ft)(1))
          else CtrlBodySplits.folded(ft, body)(nlSplit(ft))
        }
      // New statement
      case tok @ FormatToken(T.Semicolon(), right, _)
          if newlines == 0 && startsStatement(right).isDefined =>
        val spaceSplit =
          if (style.newlines.source eq Newlines.unfold) Split.ignored
          else {
            val expire = startsStatement(right).get.tokens.last
            Split(Space, 0).withSingleLine(expire)
          }
        Seq(
          spaceSplit,
          // For some reason, this newline cannot cost 1.
          Split(NewlineT(isDouble = tok.hasBlankLine), 0)
        )

      case tok @ FormatToken(left, right, _)
          if startsStatement(right).isDefined =>
        val expire = rightOwner.tokens
          .find(_.is[T.Equals])
          .map { equalsToken =>
            val equalsFormatToken = tokens(equalsToken)
            if (equalsFormatToken.right.is[T.LeftBrace]) {
              equalsFormatToken.right
            } else {
              equalsToken
            }
          }
          .getOrElse(rightOwner.tokens.last)

        val annoRight = right.is[T.At]
        val annoLeft = isSingleIdentifierAnnotation(prev(tok))

        if (
          (annoRight || annoLeft) &&
          style.optIn.annotationNewlines && !style.newlines.sourceIgnored
        )
          Seq(Split(getMod(formatToken), 0))
        else {
          asInfixApp(rightOwner, style.newlines.formatInfix).fold {
            val spaceCouldBeOk = annoLeft && (style.newlines.source match {
              case Newlines.unfold =>
                right.is[T.Comment] ||
                  !style.optIn.annotationNewlines && annoRight
              case Newlines.fold =>
                right.is[T.Comment] || annoRight ||
                  !style.optIn.annotationNewlines && right.is[Keyword]
              case _ =>
                newlines == 0 && right.is[Keyword]
            })
            Seq(
              // This split needs to have an optimalAt field.
              Split(Space, 0)
                .onlyIf(spaceCouldBeOk)
                .withSingleLine(expire),
              // For some reason, this newline cannot cost 1.
              Split(NewlineT(isDouble = tok.hasBlankLine), 0)
            )
          } { app =>
            val mod =
              if (left.is[T.Comment] && tok.noBreak) Space
              else NewlineT(isDouble = tok.hasBlankLine)
            getInfixSplitsBeforeLhs(app, tok, Some(mod))
          }
        }

      case FormatToken(_, T.RightBrace(), _) =>
        Seq(
          Split(xmlSpace(rightOwner), 0),
          Split(NewlineT(isDouble = formatToken.hasBlankLine), 0)
        )
      case FormatToken(left @ T.KwPackage(), _, _) if leftOwner.is[Pkg] =>
        Seq(
          Split(Space, 0)
        )
      // Opening [ with no leading space.
      // Opening ( with no leading space.
      case FormatToken(
            RightParenOrBracket() | T.KwSuper() | T.KwThis() | T.Ident(_) |
            T.RightBrace() | T.Underscore() | T.MacroQuotedIdent(_) |
            T.MacroSplicedIdent(_),
            LeftParenOrBracket(),
            _
          ) if noSpaceBeforeOpeningParen(rightOwner) && {
            leftOwner.parent.forall {
              // infix applications have no space.
              case _: Type.ApplyInfix | _: Term.ApplyInfix => false
              case parent => true
            }
          } =>
        val modification: Modification = leftOwner match {
          case _: Mod => Space
          // Add a space between constructor annotations and their parameter lists
          // see:
          // https://github.com/scalameta/scalafmt/pull/1516
          // https://github.com/scalameta/scalafmt/issues/1528
          case init: Init if init.parent.forall(_.is[Mod.Annot]) => Space
          case t: Term.Name
              if style.spaces.afterTripleEquals && t.value == "===" =>
            Space
          case name: Term.Name
              if style.spaces.afterSymbolicDefs &&
                isSymbolicName(name.value) && name.parent.exists(isDefDef) =>
            Space
          case _ => NoSplit
        }
        Seq(
          Split(modification, 0)
        )
      // Defn.{Object, Class, Trait, Enum}
      case FormatToken(
            _: T.KwObject | _: T.KwClass | _: T.KwTrait | _: T.KwEnum,
            r,
            _
          ) =>
        val expire = defnTemplate(leftOwner)
          .flatMap(templateCurly)
          .getOrElse(leftOwner.tokens.last)
        val forceNewlineBeforeExtends = Policy.before(expire) {
          case Decision(t @ FormatToken(_, _: T.KwExtends, _), s)
              if t.meta.rightOwner == leftOwner =>
            s.filter(x => x.isNL && !x.isActiveFor(SplitTag.OnelineWithChain))
        }
        val policyExpire = defnBeforeTemplate(leftOwner).fold(r)(_.tokens.last)
        val policyEnd = Policy.End.After(policyExpire)
        val policy = delayedBreakPolicy(policyEnd)(forceNewlineBeforeExtends)
        Seq(Split(Space, 0).withPolicy(policy))
      // DefDef
      case tok @ FormatToken(T.KwDef(), name @ T.Ident(_), _) =>
        Seq(
          Split(Space, 0)
        )
      case ft @ FormatToken(_: T.Equals, _, _)
          if defBody(leftOwner).isDefined =>
        val body = defBody(leftOwner).get
        asInfixApp(rightOwner, style.newlines.formatInfix).fold {
          getSplitsDefValEquals(ft, body)(getSplitsDefEquals(ft, body))
        }(getInfixSplitsBeforeLhs(_, ft))

      // Parameter opening for one parameter group. This format works
      // on the WHOLE defnSite (via policies)
      case ft @ FormatToken(LeftParenOrBracket(), _, _)
          if style.verticalMultiline.atDefnSite &&
            isDefnSiteWithParams(leftOwner) =>
        verticalMultiline(leftOwner, ft)(style)

      // Term.Apply and friends
      case FormatToken(T.LeftParen(), _, _)
          if getLambdaAtSingleArgCallSite(formatToken).isDefined =>
        val lambda = getLambdaAtSingleArgCallSite(formatToken).get
        val close = matching(formatToken.left)
        val newlinePolicy =
          if (!style.danglingParentheses.callSite) None
          else Some(decideNewlinesOnlyBeforeClose(close))
        val noSplitMod =
          if (style.newlines.alwaysBeforeCurlyLambdaParams) null
          else getNoSplit(formatToken, true)

        def multilineSpaceSplit(implicit line: sourcecode.Line): Split = {
          val lambdaLeft: Option[Token] =
            matchingOpt(functionExpire(lambda)._1).filter(_.is[T.LeftBrace])

          val arrowFt = getFuncArrow(lambda).get
          val lambdaIsABlock = lambdaLeft.contains(arrowFt.right)
          val lambdaToken =
            getOptimalTokenFor(if (lambdaIsABlock) next(arrowFt) else arrowFt)

          val spacePolicy = SingleLineBlock(lambdaToken) | {
            if (lambdaIsABlock) None
            else
              newlinePolicy.map(
                delayedBreakPolicy(Policy.End.On(lambdaLeft.getOrElse(close)))
              )
          }
          Split(noSplitMod, 0)
            .withPolicy(spacePolicy)
            .withOptimalToken(lambdaToken)
        }

        if (noSplitMod == null)
          Seq(
            Split(Newline, 0)
              .withPolicyOpt(newlinePolicy)
              .withIndent(style.continuationIndent.callSite, close, Before)
          )
        else {
          val newlinePenalty = 3 + nestedApplies(leftOwner)
          val noMultiline = style.newlines.beforeCurlyLambdaParams eq
            Newlines.BeforeCurlyLambdaParams.multiline
          Seq(
            Split(noSplitMod, 0).withSingleLine(close),
            if (noMultiline) Split.ignored else multilineSpaceSplit,
            Split(Newline, newlinePenalty)
              .withPolicyOpt(newlinePolicy)
              .withIndent(style.continuationIndent.callSite, close, Before)
          )
        }

      case FormatToken(T.LeftParen(), T.RightParen(), _) =>
        val noNL = style.newlines.sourceIgnored || formatToken.noBreak
        Seq(Split(NoSplit.orNL(noNL), 0))

      case tok @ FormatToken(open @ LeftParenOrBracket(), right, _)
          if !isSuperfluousParenthesis(formatToken.left, leftOwner) &&
            (!style.binPack.unsafeCallSite && isCallSite(leftOwner)) ||
            (!style.binPack.unsafeDefnSite && isDefnSite(leftOwner)) =>
        val close = matching(open)
        val (lhs, args) = getApplyArgs(formatToken, false)
        // In long sequence of select/apply, we penalize splitting on
        // parens furthest to the right.
        val lhsPenalty = treeDepth(lhs)

        // XXX: sometimes we have zero args, so multipleArgs != !singleArgument
        val singleArgument = args.length == 1
        val multipleArgs = args.length > 1
        val notTooManyArgs = multipleArgs && args.length <= 100

        val isBracket = open.is[T.LeftBracket]
        val bracketCoef = if (isBracket) Constants.BracketPenalty else 1

        val onlyConfigStyle = mustUseConfigStyle(formatToken)

        val sourceIgnored = style.newlines.sourceIgnored
        val isSingleEnclosedArgument =
          singleArgument && isEnclosedInMatching(args(0))
        val useConfigStyle = onlyConfigStyle || (sourceIgnored &&
          style.optIn.configStyleArguments && !isSingleEnclosedArgument)

        def isExcludedTree(tree: Tree): Boolean =
          tree match {
            case t: Init => t.argss.nonEmpty
            case t: Term.Apply => t.args.nonEmpty
            case t: Term.ApplyType => t.targs.nonEmpty
            case t: Term.Match => t.cases.nonEmpty
            case t: Term.New => t.init.argss.nonEmpty
            case _: Term.NewAnonymous => true
            case _ => false
          }

        val nestedPenalty = 1 + nestedApplies(leftOwner) + lhsPenalty
        val excludeBlocks =
          if (isBracket) insideBlock[T.LeftBracket](tok, close)
          else if (
            multipleArgs ||
            !isSingleEnclosedArgument &&
            style.newlines.source.eq(Newlines.unfold)
          )
            TokenRanges.empty
          else if (
            style.newlines.source.eq(Newlines.fold) && {
              isSingleEnclosedArgument ||
              singleArgument && isExcludedTree(args(0))
            }
          )
            parensTuple(args(0).tokens.last)
          else insideBlock[T.LeftBrace](tok, close)

        val indent = getApplyIndent(leftOwner, onlyConfigStyle)

        def singleLine(
            newlinePenalty: Int
        )(implicit line: sourcecode.Line): Policy = {
          val baseSingleLinePolicy = if (isBracket) {
            if (!multipleArgs)
              PenalizeAllNewlines(
                close,
                newlinePenalty,
                penalizeLambdas = false
              )
            else SingleLineBlock(close, noSyntaxNL = true)
          } else {
            val penalty =
              if (!multipleArgs) newlinePenalty
              else Constants.ShouldBeNewline
            policyWithExclude(excludeBlocks, Policy.End.On, Policy.End.On)(
              Policy.End.Before(close),
              new PenalizeAllNewlines(
                _,
                penalty = penalty,
                penalizeLambdas = multipleArgs,
                noSyntaxNL = multipleArgs
              )
            )
          }

          baseSingleLinePolicy
        }

        val defnSite = isDefnSite(leftOwner)
        val closeFormatToken = tokens(close)
        val expirationToken: Token =
          if (defnSite && !isBracket)
            defnSiteLastToken(closeFormatToken, leftOwner)
          else
            rhsOptimalToken(closeFormatToken)

        val mustDangle = onlyConfigStyle || expirationToken.is[T.Comment]
        val shouldDangle =
          if (defnSite) !shouldNotDangleAtDefnSite(leftOwner, false)
          else style.danglingParentheses.callSite
        val wouldDangle = shouldDangle || {
          val beforeClose = prev(closeFormatToken)
          beforeClose.hasBreak && beforeClose.left.is[T.Comment]
        }

        val newlinePolicy: Policy =
          if (wouldDangle || mustDangle) {
            decideNewlinesOnlyBeforeClose(close)
          } else {
            Policy.NoPolicy
          }

        val handleImplicit =
          if (onlyConfigStyle) opensConfigStyleImplicitParamList(formatToken)
          else opensImplicitParamList(formatToken, args)

        val noSplitMod =
          if (
            style.newlines.source.eq(Newlines.keep) && tok.hasBreak || {
              if (!handleImplicit) onlyConfigStyle
              else style.newlines.forceBeforeImplicitParamListModifier
            }
          )
            null
          else getNoSplit(formatToken, !isBracket)
        val noSplitIndent = if (right.is[T.Comment]) indent else Num(0)

        val align = {
          if (defnSite) style.align.openParenDefnSite
          else style.align.openParenCallSite
        } && (!handleImplicit ||
          style.newlines.forceAfterImplicitParamListModifier)
        val alignTuple = align && isTuple(leftOwner) && !onlyConfigStyle

        val keepConfigStyleSplit = !sourceIgnored &&
          style.optIn.configStyleArguments && newlines != 0
        val splitsForAssign =
          if (defnSite || isBracket || keepConfigStyleSplit) None
          else
            getAssignAtSingleArgCallSite(leftOwner).map { assign =>
              val assignToken = assign.rhs match {
                case b: Term.Block => b.tokens.head
                case _ => assign.tokens.find(_.is[T.Equals]).get
              }
              val breakToken = getOptimalTokenFor(assignToken)
              val newlineAfterAssignDecision =
                if (newlinePolicy.isEmpty) Policy.NoPolicy
                else decideNewlinesOnlyAfterToken(breakToken)
              Seq(
                Split(Newline, nestedPenalty + Constants.ExceedColumnPenalty)
                  .withPolicy(newlinePolicy)
                  .withIndent(indent, close, Before),
                Split(NoSplit, nestedPenalty)
                  .withSingleLine(breakToken)
                  .andPolicy(newlinePolicy & newlineAfterAssignDecision)
              )
            }

        val preferNoSplit = singleArgument &&
          style.newlines.source.eq(Newlines.keep) && tok.noBreak
        val oneArgOneLine = newlinePolicy & splitOneArgOneLine(close, leftOwner)
        val extraOneArgPerLineIndent =
          if (multipleArgs && style.poorMansTrailingCommasInConfigStyle)
            Indent(Num(2), right, After)
          else Indent.Empty
        val (implicitPenalty, implicitPolicy) =
          if (!handleImplicit) (2, Policy.NoPolicy)
          else (0, decideNewlinesOnlyAfterToken(right))

        val splitsNoNL =
          if (noSplitMod == null) Seq.empty
          else if (onlyConfigStyle)
            Seq(
              Split(noSplitMod, 0)
                .withPolicy(oneArgOneLine & implicitPolicy)
                .withOptimalToken(right, killOnFail = true)
                .withIndent(extraOneArgPerLineIndent)
                .withIndent(indent, close, Before)
            )
          else {
            val noSplitPolicy =
              if (preferNoSplit) singleLine(2)
              else if (wouldDangle || mustDangle && isBracket || useConfigStyle)
                SingleLineBlock(
                  close,
                  exclude = excludeBlocks,
                  noSyntaxNL = multipleArgs
                )
              else if (splitsForAssign.isDefined)
                singleLine(3)
              else
                singleLine(10)
            Seq(
              Split(noSplitMod, 0, policy = noSplitPolicy)
                .withOptimalToken(expirationToken)
                .withIndent(noSplitIndent, close, Before),
              Split(noSplitMod, (implicitPenalty + lhsPenalty) * bracketCoef)
                .withPolicy(oneArgOneLine & implicitPolicy)
                .onlyIf(
                  (notTooManyArgs && align) || (handleImplicit &&
                    style.newlines.notBeforeImplicitParamListModifier)
                )
                .withIndent(if (align) StateColumn else indent, close, Before)
            )
          }

        val splitsNL =
          if (
            alignTuple || !(onlyConfigStyle ||
              multipleArgs || splitsForAssign.isEmpty)
          )
            Seq.empty
          else {
            val cost =
              if (onlyConfigStyle)
                if (splitsNoNL.isEmpty) 0 else 1
              else
                (if (preferNoSplit) Constants.ExceedColumnPenalty else 0) +
                  bracketCoef * (nestedPenalty + (if (multipleArgs) 2 else 0))
            val split =
              if (multipleArgs)
                Split(Newline, cost, policy = oneArgOneLine)
                  .withIndent(extraOneArgPerLineIndent)
              else {
                val noSplit = !onlyConfigStyle && right.is[T.LeftBrace]
                val noConfigStyle = noSplit ||
                  newlinePolicy.isEmpty || !style.optIn.configStyleArguments
                Split(NoSplit.orNL(noSplit), cost, policy = newlinePolicy)
                  .andPolicy(singleLine(4), !noConfigStyle)
              }
            Seq(split.withIndent(indent, close, Before))
          }

        splitsNoNL ++ splitsNL ++ splitsForAssign.getOrElse(Seq.empty)

      case FormatToken(open @ LeftParenOrBracket(), right, between)
          if style.binPack.unsafeDefnSite && isDefnSite(leftOwner) =>
        val close = matching(open)
        val isBracket = open.is[T.LeftBracket]
        val indent = Num(style.continuationIndent.getDefnSite(leftOwner))
        if (isTuple(leftOwner)) {
          Seq(
            Split(NoSplit, 0).withPolicy(SingleLineBlock(close, okSLC = true))
          )
        } else {
          def penalizeBrackets(penalty: Int): Policy =
            if (isBracket)
              PenalizeAllNewlines(close, Constants.BracketPenalty * penalty + 3)
            else NoPolicy
          val bracketCoef = if (isBracket) Constants.BracketPenalty else 1
          val bracketPenalty = if (isBracket) 1 else 0
          val nestingPenalty = nestedApplies(leftOwner)
          val onlyConfigStyle = mustUseConfigStyle(formatToken)

          val mustDangle = onlyConfigStyle ||
            style.newlines.sourceIgnored && style.danglingParentheses.defnSite
          val noSplitPolicy: Policy =
            if (mustDangle) SingleLineBlock(close)
            else {
              val noSplitPenalizeNewlines = penalizeBrackets(1 + bracketPenalty)
              argumentStarts.get(hash(right)) match {
                case Some(arg) =>
                  val singleLine = SingleLineBlock(arg.tokens.last)
                  if (isBracket) {
                    noSplitPenalizeNewlines & singleLine
                  } else {
                    singleLine
                  }
                case _ => noSplitPenalizeNewlines
              }
            }
          val noSplitModification =
            if (right.is[T.Comment]) getMod(formatToken)
            else NoSplit
          val nlDanglePolicy =
            if (mustDangle) decideNewlinesOnlyBeforeClose(close) else NoPolicy

          Seq(
            Split(noSplitModification, 0 + (nestingPenalty * bracketCoef))
              .notIf(onlyConfigStyle)
              .withPolicy(noSplitPolicy)
              .withIndent(indent, close, Before),
            Split(Newline, (1 + nestingPenalty * nestingPenalty) * bracketCoef)
              .notIf(right.is[T.RightParen])
              .withPolicy(penalizeBrackets(1))
              .andPolicy(nlDanglePolicy)
              .withIndent(indent, close, Before)
          )
        }

      case FormatToken(LeftParenOrBracket(), _, _)
          if style.binPack.unsafeCallSite && isCallSite(leftOwner) =>
        val open = formatToken.left
        val close = matching(open)
        val indent = getApplyIndent(leftOwner)
        def baseNoSplit(implicit line: sourcecode.Line) =
          Split(NoSplit, 0).withIndent(indent, close, Before)
        val opensLiteralArgumentList =
          styleMap.opensLiteralArgumentList(formatToken)
        val singleLineOnly =
          style.binPack.literalsSingleLine && opensLiteralArgumentList
        val onlyConfigStyle =
          mustUseConfigStyle(formatToken, !opensLiteralArgumentList)

        val noSplit =
          if (singleLineOnly || style.newlines.sourceIgnored)
            baseNoSplit.withSingleLine(close)
          else if (onlyConfigStyle) Split.ignored
          else {
            val opt = leftOwner.tokens.find(_.is[T.Comma]).orElse(Some(close))
            val isBracket = open.is[T.LeftBracket]
            // TODO(olafur) DRY. Same logic as in default.
            val exclude =
              if (isBracket)
                insideBlock[T.LeftBracket](formatToken, close)
              else
                insideBlock[T.LeftBrace](formatToken, close)
            val policy =
              policyWithExclude(exclude, Policy.End.Before, Policy.End.On)(
                Policy.End.Before(close),
                new PenalizeAllNewlines(_, 3)
              ) & Policy.on(close) {
                val excludeOpen = exclude.ranges.map(_.lt).toSet
                UnindentAtExclude(excludeOpen, Num(-indent.n))
              }
            baseNoSplit.withOptimalTokenOpt(opt).withPolicy(policy)
          }

        def newlineBeforeClose = decideNewlinesOnlyBeforeClose(close)
        val nlPolicy =
          if (onlyConfigStyle) {
            if (styleMap.forcedBinPack(leftOwner)) newlineBeforeClose
            else splitOneArgOneLine(close, leftOwner) | newlineBeforeClose
          } else if (
            style.newlines.sourceIgnored &&
            style.danglingParentheses.callSite
          )
            newlineBeforeClose
          else NoPolicy
        Seq(
          noSplit,
          Split(NewlineT(alt = if (singleLineOnly) Some(NoSplit) else None), 2)
            .withIndent(indent, close, Before)
            .withSingleLineOpt(if (singleLineOnly) Some(close) else None)
            .andPolicy(nlPolicy)
        )

      // If configured to skip the trailing space after `if` and other keywords, do so.
      case FormatToken(T.KwIf() | T.KwFor() | T.KwWhile(), T.LeftParen(), _)
          if !style.spaces.afterKeywordBeforeParen =>
        Seq(Split(NoSplit, 0))

      // Closing def site ): ReturnType
      case FormatToken(left, T.Colon(), _)
          if style.newlines.sometimesBeforeColonInMethodReturnType &&
            defDefReturnType(leftOwner).isDefined =>
        val expire = lastToken(defDefReturnType(rightOwner).get)
        val penalizeNewlines =
          PenalizeAllNewlines(expire, Constants.BracketPenalty)
        val sameLineSplit = Space(endsWithSymbolIdent(left))
        val indent = style.continuationIndent.getDefnSite(leftOwner)
        Seq(
          Split(sameLineSplit, 0).withPolicy(penalizeNewlines),
          // Spark style guide allows this:
          // https://github.com/databricks/scala-style-guide#indent
          Split(Newline, Constants.SparkColonNewline)
            .withIndent(indent, expire, After)
            .withPolicy(penalizeNewlines)
        )
      case FormatToken(T.Colon(), _, _)
          if style.newlines.avoidInResultType &&
            defDefReturnType(leftOwner).isDefined =>
        val expire = defDefReturnType(leftOwner).get match {
          case Type.Refine(_, headStat :: _) =>
            tokens(headStat.tokens.head, -1).left
          case t => lastToken(t)
        }
        Seq(Split(Space, 0).withPolicy(SingleLineBlock(expire, okSLC = true)))

      case FormatToken(T.LeftParen(), T.LeftBrace(), between) =>
        Seq(
          Split(NoSplit, 0)
        )

      case FormatToken(_, T.LeftBrace(), _) if isXmlBrace(rightOwner) =>
        withIndentOnXmlSpliceStart(
          formatToken,
          Seq(Split(NoSplit, 0))
        )

      case FormatToken(T.RightBrace(), _, _) if isXmlBrace(leftOwner) =>
        Seq(
          Split(NoSplit, 0)
        )
      // non-statement starting curly brace
      case FormatToken(_: T.Comma, open: T.LeftBrace, _)
          if !style.poorMansTrailingCommasInConfigStyle && {
            if (isCallSite(leftOwner)) !style.binPack.unsafeCallSite
            else isDefnSite(leftOwner) && !style.binPack.unsafeDefnSite
          } =>
        val close = matching(open)
        val oneArgPerLineSplits =
          (rightOwner match {
            case _: Term.PartialFunction | Term.Block(
                  List(_: Term.Function | _: Term.PartialFunction)
                ) =>
              Seq(Split(Newline, 0))
            case _ =>
              val breakAfter =
                rhsOptimalToken(next(nextNonCommentSameLine(formatToken)))
              val multiLine =
                decideNewlinesOnlyBeforeClose(close) |
                  decideNewlinesOnlyAfterToken(breakAfter)
              Seq(
                Split(Newline, 0).withSingleLine(close, killOnFail = true),
                Split(Space, 1, policy = multiLine)
              )
          }).map(_.onlyFor(SplitTag.OneArgPerLine))
        val sourceIsKeep = style.newlines.source eq Newlines.keep
        Seq(
          Split(Space, 0).onlyIf(newlines == 0 || !sourceIsKeep),
          Split(Newline, 0)
            .onlyIf(oneArgPerLineSplits.isEmpty)
            .onlyIf(newlines != 0)
            .onlyIf(sourceIsKeep || open.pos.endLine == close.pos.startLine)
            .withSingleLine(close, killOnFail = true)
        ) ++ oneArgPerLineSplits

      case FormatToken(_: T.MacroSplice | _: T.MacroQuote, _: T.LeftBrace, _) =>
        Seq(Split(NoSplit, 0))
      case FormatToken(_, _: T.LeftBrace, _) =>
        Seq(Split(Space, 0))

      // Delim
      case FormatToken(_, T.Comma(), _) =>
        Seq(
          Split(NoSplit, 0)
        )
      // These are mostly filtered out/modified by policies.
      case tok @ FormatToken(_: T.Comma, c: T.Comment, _) =>
        if (isSingleLineComment(c)) Seq(Split(Space.orNL(tok.noBreak), 0))
        else if (tok.meta.right.firstNL >= 0) Seq(Split(Newline, 0))
        else {
          val noNewline = newlines == 0 &&
            // perhaps left is a trailing comma
            nextNonComment(next(tok)).right.is[RightParenOrBracket]
          Seq(Split(Space, 0), Split(Newline, 1).notIf(noNewline))
        }
      case tok @ FormatToken(T.Comma(), right, _) =>
        // TODO(olafur) DRY, see OneArgOneLine.
        argumentStarts.get(hash(right)) match {
          case Some(nextArg) if isBinPack(leftOwner) =>
            val lastFT = tokens(nextArg.tokens.last)
            Seq(
              Split(Space, 0).withSingleLine(rhsOptimalToken(lastFT)),
              Split(Newline, 1)
            )
          case _
              if !style.newlines.formatInfix &&
                leftOwner.isInstanceOf[Term.ApplyInfix] =>
            Seq(
              // Do whatever the user did if infix.
              Split(Space.orNL(newlines == 0), 0)
            )
          case _ =>
            val indent = leftOwner match {
              case _: Defn.Val | _: Defn.Var =>
                style.continuationIndent.getDefnSite(leftOwner)
              case _ =>
                0
            }
            Seq(
              Split(Space, 0),
              Split(Newline, 1).withIndent(indent, right, After)
            )
        }
      case FormatToken(_, T.Semicolon(), _) =>
        Seq(
          Split(NoSplit, 0)
        )
      case FormatToken(_: T.KwReturn, _, _) =>
        val mod =
          if (formatToken.hasBlankLine) Newline2x
          else
            leftOwner match {
              case Term.Return(unit @ Lit.Unit()) if unit.tokens.isEmpty =>
                // Always force blank line for Unit "return".
                Newline
              case _ =>
                Space
            }
        Seq(
          Split(mod, 0)
        )
      case FormatToken(left, T.Colon(), _) =>
        val mod: Modification = rightOwner match {
          case tp: Type.Param =>
            val contextOption = style.spaces.beforeContextBoundColon
            val summaryTypeBoundsCount =
              tp.tbounds.lo.size + tp.tbounds.hi.size + tp.cbounds.size
            val useSpace = contextOption.isAlways ||
              contextOption.isIfMultipleBounds && summaryTypeBoundsCount > 1
            Space(useSpace)

          case _ =>
            left match {
              case ident: T.Ident => identModification(ident)
              case _ => NoSplit
            }
        }
        Seq(
          Split(mod, 0)
        )
      // Only allow space after = in val if rhs is a single line or not
      // an infix application or an if. For example, this is allowed:
      // val x = function(a,
      //                  b)
      case ft @ FormatToken(_: T.Equals, _, _) if (leftOwner match {
            case _: Defn.Type | _: Defn.Val | _: Defn.Var => true
            case _: Term.Assign => true
            case t: Term.Param => t.default.isDefined
            case _ => false
          }) =>
        val rhs: Tree = leftOwner match {
          case l: Term.Assign => l.rhs
          case l: Term.Param => l.default.get
          case l: Defn.Type => l.body
          case l: Defn.Val => l.rhs
          case r: Defn.Var =>
            r.rhs match {
              case Some(x) => x
              case None => r // var x: Int = _, no policy
            }
        }

        asInfixApp(rightOwner, style.newlines.formatInfix).fold {
          getSplitsDefValEquals(ft, rhs) {
            val classic = style.newlines.getBeforeMultiline eq Newlines.classic
            if (classic) getSplitsValEquals(ft, rhs)
            else CtrlBodySplits.getWithIndent(ft, rhs)(null)(Split(Newline, _))
          }
        }(getInfixSplitsBeforeLhs(_, ft))

      case FormatToken(_, _: T.Dot, _)
          if style.newlines.source.ne(Newlines.keep) &&
            rightOwner.is[Term.Select] && findTreeWithParent(rightOwner) {
              case _: Type.Select | _: Importer | _: Pkg => Some(true)
              case _: Term.Select | SplitCallIntoParts(_, _) => None
              case _ => Some(false)
            }.isDefined =>
        Seq(Split(NoSplit, 0))

      case t @ FormatToken(left, _: T.Dot, _) if rightOwner.is[Term.Select] =>
        val enclosed = style.encloseSelectChains
        val (expireTree, nextSelect) =
          findLastApplyAndNextSelect(rightOwner, enclosed)
        val thisSelect = rightOwner.asInstanceOf[Term.Select]
        val prevSelect = findPrevSelect(thisSelect, enclosed)
        val expireDropRight = if (isEnclosedInMatching(expireTree)) 1 else 0
        val expire = lastToken(expireTree.tokens.dropRight(expireDropRight))

        def breakOnNextDot: Policy =
          nextSelect.fold(Policy.noPolicy) { tree =>
            val end = tree.name.tokens.head
            Policy.before(end) {
              case Decision(t @ FormatToken(_, _: T.Dot, _), s)
                  if t.meta.rightOwner eq tree =>
                val filtered = s.flatMap { x =>
                  val y = x.activateFor(SplitTag.SelectChainSecondNL)
                  if (y.isActive) Some(y) else None
                }
                if (filtered.isEmpty) Seq.empty
                else {
                  val minCost = math.max(0, filtered.map(_.cost).min - 1)
                  filtered.map { x =>
                    val p =
                      x.policy.filter(!_.isInstanceOf[PenalizeAllNewlines])
                    x.copy(cost = x.cost - minCost, policy = p)
                  }
                }
            }
          }
        val baseSplits = style.newlines.source match {
          case Newlines.classic =>
            def getNlMod = {
              val endSelect = nextSelect.fold(expire)(x => lastToken(x.qual))
              val nlAlt = ModExt(NoSplit).withIndent(-2, endSelect, After)
              NewlineT(alt = Some(nlAlt))
            }

            val prevChain = inSelectChain(prevSelect, thisSelect, expireTree)
            if (canStartSelectChain(thisSelect, nextSelect, expireTree)) {
              val chainExpire =
                if (nextSelect.isEmpty) lastToken(thisSelect) else expire
              val nestedPenalty =
                nestedSelect(rightOwner) + nestedApplies(leftOwner)
              // This policy will apply to both the space and newline splits, otherwise
              // the newline is too cheap even it doesn't actually prevent other newlines.
              val penalizeBreaks = PenalizeAllNewlines(chainExpire, 2)
              def slbPolicy =
                SingleLineBlock(
                  chainExpire,
                  getExcludeIf(chainExpire),
                  noSyntaxNL = true
                )
              val newlinePolicy = breakOnNextDot & penalizeBreaks
              val ignoreNoSplit = t.hasBreak &&
                (left.is[T.Comment] || style.optIn.breakChainOnFirstMethodDot)
              val chainLengthPenalty =
                if (
                  style.newlines.penalizeSingleSelectMultiArgList &&
                  nextSelect.isEmpty
                ) {
                  // penalize by the number of arguments in the rhs open apply.
                  // I know, it's a bit arbitrary, but my manual experiments seem
                  // to show that it produces OK output. The key insight is that
                  // many arguments on the same line can be hard to read. By not
                  // putting a newline before the dot, we force the argument list
                  // to break into multiple lines.
                  splitCallIntoParts.lift(tokens(t, 2).meta.rightOwner) match {
                    case Some((_, Left(args))) =>
                      Math.max(0, args.length - 1)
                    case Some((_, Right(argss))) =>
                      Math.max(0, argss.map(_.length).sum - 1)
                    case _ => 0
                  }
                } else 0
              // when the flag is on, penalize break, to avoid idempotence issues;
              // otherwise, after the break is chosen, the flag prohibits nosplit
              val nlBaseCost =
                if (style.optIn.breakChainOnFirstMethodDot && t.noBreak) 3
                else 2
              val nlCost = nlBaseCost + nestedPenalty + chainLengthPenalty
              val nlMod = getNlMod
              Seq(
                Split(!prevChain, 1) { // must come first, for backwards compat
                  if (style.optIn.breaksInsideChains) NoSplit.orNL(t.noBreak)
                  else nlMod
                }
                  .withPolicy(newlinePolicy)
                  .onlyFor(SplitTag.SelectChainSecondNL),
                Split(ignoreNoSplit, 0)(NoSplit)
                  .withPolicy(slbPolicy, prevChain)
                  .andPolicy(penalizeBreaks),
                Split(if (ignoreNoSplit) Newline else nlMod, nlCost)
                  .withPolicy(newlinePolicy)
              )
            } else {
              val isComment = left.is[T.Comment]
              val doBreak = isComment && t.hasBreak
              Seq(
                Split(!prevChain, 1) {
                  if (style.optIn.breaksInsideChains) NoSplit.orNL(t.noBreak)
                  else if (doBreak) Newline
                  else getNlMod
                }
                  .withPolicy(breakOnNextDot)
                  .onlyFor(SplitTag.SelectChainSecondNL),
                Split(if (doBreak) Newline else Space(isComment), 0)
              )
            }

          case _ if left.is[T.Comment] =>
            Seq(Split(Space.orNL(t.noBreak), 0))

          case Newlines.keep =>
            Seq(Split(NoSplit.orNL(t.noBreak), 0))

          case Newlines.unfold =>
            if (prevSelect.isEmpty && nextSelect.isEmpty)
              Seq(Split(NoSplit, 0), Split(Newline, 1))
            else {
              val forcedBreakPolicy = nextSelect.map { tree =>
                Policy.before(tree.name.tokens.head) {
                  case Decision(t @ FormatToken(_, _: T.Dot, _), s)
                      if t.meta.rightOwner eq tree =>
                    s.filter(_.isNL)
                }
              }
              Seq(
                Split(NoSplit, 0).withSingleLine(expire, noSyntaxNL = true),
                Split(NewlineT(alt = Some(NoSplit)), 1)
                  .withPolicyOpt(forcedBreakPolicy)
              )
            }

          case Newlines.fold =>
            val end = nextSelect.fold(expire)(x => lastToken(x.qual))
            def exclude = insideBlock[LeftParenOrBrace](t, end)
            Seq(
              Split(NoSplit, 0).withSingleLine(end, exclude),
              Split(NewlineT(alt = Some(NoSplit)), 1)
            )
        }

        val delayedBreakPolicyOpt = nextSelect.map { tree =>
          Policy.before(tree.name.tokens.head) {
            case Decision(t @ FormatToken(_, _: T.Dot, _), s)
                if t.meta.rightOwner eq tree =>
              SplitTag.SelectChainFirstNL.activateOnly(s)
          }
        }

        // trigger indent only on the first newline
        val indent = Indent(Num(2), expire, After)
        val willBreak = nextNonCommentSameLine(tokens(t, 2)).right.is[T.Comment]
        val splits = baseSplits.map { s =>
          if (willBreak || s.isNL) s.withIndent(indent)
          else s.andFirstPolicyOpt(delayedBreakPolicyOpt)
        }

        if (prevSelect.isEmpty) splits
        else baseSplits ++ splits.map(_.onlyFor(SplitTag.SelectChainFirstNL))

      // ApplyUnary
      case tok @ FormatToken(T.Ident(_), Literal(), _)
          if leftOwner == rightOwner =>
        Seq(
          Split(NoSplit, 0)
        )
      case FormatToken(op @ T.Ident(_), right, _) if leftOwner.parent.exists {
            case unary: Term.ApplyUnary =>
              unary.op.tokens.head == op
            case _ => false
          } =>
        Seq(
          Split(Space(isSymbolicIdent(right)), 0)
        )
      // Annotations, see #183 for discussion on this.
      case FormatToken(_, bind @ T.At(), _) if rightOwner.is[Pat.Bind] =>
        Seq(
          Split(Space, 0)
        )
      case FormatToken(bind @ T.At(), _, _) if leftOwner.is[Pat.Bind] =>
        Seq(
          Split(Space, 0)
        )
      case FormatToken(T.At(), right @ Delim(), _) =>
        Seq(Split(NoSplit, 0))
      case FormatToken(T.At(), right @ T.Ident(_), _) =>
        // Add space if right starts with a symbol
        Seq(Split(identModification(right), 0))

      // Enum Case
      case FormatToken(_, T.KwExtends(), _)
          if rightOwner.isInstanceOf[Defn.EnumCase] =>
        val lastToken = rightOwner.tokens.last
        val enumCase = rightOwner.asInstanceOf[Defn.EnumCase]
        binPackParentConstructorSplits(
          Set(rightOwner),
          lastToken,
          style.continuationIndent.extendSite,
          enumCase.inits.length > 1
        )
      // Template
      case FormatToken(_, right @ T.KwExtends(), _) =>
        val template = defnTemplate(rightOwner)
        val lastToken = template
          .flatMap(templateCurly)
          .orElse(template.map(_.tokens.last))
          .getOrElse(rightOwner.tokens.last)
        binPackParentConstructorSplits(
          template.toSet,
          lastToken,
          style.continuationIndent.extendSite,
          template.exists(_.inits.length > 1)
        )
      case FormatToken(_, T.KwWith(), _) =>
        def isFirstWith(t: Template) =
          t.inits.headOption.exists { init =>
            // [init.tpe == leftOwner] part is about expressions like [new A with B]
            // [leftOwner.is[Init] && init == leftOwner] part is about expressions like [new A(x) with B]
            leftOwner.is[Init] && init == leftOwner || init.tpe == leftOwner
          }
        rightOwner match {
          // something like new A with B with C
          case template: Template if template.parent.exists { p =>
                p.is[Term.New] || p.is[Term.NewAnonymous]
              } =>
            splitWithChain(
              isFirstWith(template),
              Set(template),
              templateCurly(template).getOrElse(template.tokens.last),
              template.inits.length > 1
            )

          case template: Template =>
            val hasSelfAnnotation = template.self.tokens.nonEmpty
            val expire = templateCurly(rightOwner)
            val indent =
              if (!isFirstWith(template)) 0
              else style.continuationIndent.withSiteRelativeToExtends
            val policy =
              if (hasSelfAnnotation) NoPolicy
              else
                Policy.after(expire) {
                  // Force template to be multiline.
                  case d @ Decision(
                        t @ FormatToken(_: T.LeftBrace, right, _),
                        _
                      )
                      if !hasSelfAnnotation &&
                        !right.is[T.RightBrace] && // corner case, body is {}
                        childOf(template, t.meta.leftOwner) =>
                    d.forceNewline
                }
            Seq(
              Split(Space, 0).withIndent(indent, expire, ExpiresOn.After),
              Split(Newline, 1)
                .withPolicy(policy)
                .withIndent(indent, expire, ExpiresOn.After)
            )
          // trait A extends B with C with D with E
          case t @ WithChain(top) =>
            splitWithChain(
              !t.lhs.is[Type.With],
              withChain(top).toSet,
              top.tokens.last
            )

          case enumCase: Defn.EnumCase =>
            val indent = style.continuationIndent.withSiteRelativeToExtends
            val expire = enumCase.tokens.last
            Seq(
              Split(Space, 0).withIndent(indent, expire, ExpiresOn.After),
              Split(Newline, 1)
                .withIndent(indent, expire, ExpiresOn.After)
            )
          case _ =>
            Seq(Split(Space, 0))
        }
      // If/For/While/For with (
      case FormatToken(open: T.LeftParen, _, _) if (leftOwner match {
            case _: Term.If | _: Term.While | _: Term.For | _: Term.ForYield =>
              !isSuperfluousParenthesis(open, leftOwner)
            case _ => false
          }) =>
        val close = matching(open)
        val penalizeNewlines = penalizeNewlineByNesting(open, close)
        if (style.danglingParentheses.ctrlSite)
          Seq(
            Split(NoSplit, 0).withSingleLine(close),
            Split(Newline, 1)
              .withIndent(style.continuationIndent.callSite, close, Before)
              .withPolicy(penalizeNewlines)
              .andPolicy(decideNewlinesOnlyBeforeClose(close))
          )
        else {
          val indent: Length =
            if (style.align.ifWhileOpenParen) StateColumn
            else style.continuationIndent.callSite
          Seq(
            Split(NoSplit, 0)
              .withIndent(indent, close, Before)
              .withPolicy(penalizeNewlines)
          )
        }
      case FormatToken(T.KwIf(), _, _) if leftOwner.is[Term.If] =>
        val owner = leftOwner.asInstanceOf[Term.If]
        val expire = rhsOptimalToken(
          tokens(
            owner.elsep.tokens.lastOption.getOrElse(owner.tokens.last)
          )
        )
        val elses = getElseChain(owner)
        val breakOnlyBeforeElse =
          if (elses.isEmpty) Policy.NoPolicy
          else
            Policy.on(elses.last) {
              case d @ Decision(FormatToken(_, r: T.KwElse, _), _)
                  if elses.contains(r) =>
                d.onlyNewlinesWithFallback(Split(Newline, 0))
            }
        Seq(
          Split(Space, 0).withSingleLine(expire, killOnFail = true),
          Split(Space, 1).withPolicy(breakOnlyBeforeElse)
        )
      case FormatToken(close: T.RightParen, right, _) if (leftOwner match {
            case _: Term.If | _: Term.For => true
            case _: Term.ForYield => style.indentYieldKeyword
            case _: Term.While => true
            case _ => false
          }) && !isFirstOrLastToken(close, leftOwner) =>
        val body = leftOwner match {
          case t: Term.If => t.thenp
          case t: Term.For => t.body
          case t: Term.ForYield => t.body
          case t: Term.While => t.body
        }
        val expire = body.tokens.last
        def nlSplitFunc(cost: Int)(implicit l: sourcecode.Line) =
          Split(Newline, cost).withIndent(2, expire, After)
        if (style.newlines.getBeforeMultiline eq Newlines.unfold)
          CtrlBodySplits.checkComment(formatToken, nlSplitFunc) { ft =>
            if (ft.right.is[T.LeftBrace]) {
              val nextFt = nextNonCommentSameLine(next(ft))
              val policy = decideNewlinesOnlyAfterToken(nextFt.left)
              Seq(Split(Space, 0, policy = policy))
            } else
              Seq(nlSplitFunc(0))
          }
        else
          CtrlBodySplits.get(formatToken, body) {
            Split(Space, 0).withSingleLineNoOptimal(
              expire,
              insideBlock[T.LeftBrace](formatToken, expire),
              noSyntaxNL = leftOwner.is[Term.ForYield] && right.is[T.KwYield]
            )
          }(nlSplitFunc)
      case FormatToken(T.RightBrace(), T.KwElse(), _) =>
        val nlOnly = style.newlines.alwaysBeforeElseAfterCurlyIf ||
          !leftOwner.is[Term.Block] || !leftOwner.parent.forall(_ == rightOwner)
        Seq(
          Split(Space.orNL(!nlOnly), 0)
        )

      case FormatToken(T.RightBrace(), T.KwYield(), _) =>
        Seq(
          Split(Space, 0)
        )
      case FormatToken(_, T.KwElse() | T.KwYield(), _) =>
        val expire = rhsOptimalToken(tokens(rightOwner.tokens.last))
        val noSpace = shouldBreak(formatToken)
        def exclude = insideBlock[T.LeftBrace](formatToken, expire)
        val noSyntaxNL = formatToken.right.is[T.KwYield]
        Seq(
          Split(Space, 0)
            .notIf(noSpace)
            .withSingleLineNoOptimal(expire, exclude, noSyntaxNL = noSyntaxNL),
          Split(Newline, 1)
        )
      // Last else branch
      case FormatToken(_: T.KwElse, _, _) if (leftOwner match {
            case t: Term.If => !t.elsep.is[Term.If]
            case x => throw new UnexpectedTree[Term.If](x)
          }) =>
        val body = leftOwner.asInstanceOf[Term.If].elsep
        val expire = body.tokens.last
        def nlSplitFunc(cost: Int) =
          Split(Newline, cost).withIndent(2, expire, After)
        if (style.newlines.getBeforeMultiline eq Newlines.unfold)
          Seq(nlSplitFunc(0))
        else
          CtrlBodySplits.get(formatToken, body) {
            Split(Space, 0).withSingleLineNoOptimal(expire)
          }(nlSplitFunc)

      // Type variance
      case tok @ FormatToken(T.Ident(_), T.Ident(_) | T.Underscore(), _)
          if isTypeVariant(leftOwner) =>
        Seq(Split(Space(isSymbolicIdent(tok.right)), 0))

      // Var args
      case FormatToken(_, T.Ident("*"), _) if rightOwner.is[Type.Repeated] =>
        Seq(
          Split(NoSplit, 0)
        )

      case FormatToken(open: T.LeftParen, right, _) =>
        val isConfig = couldUseConfigStyle(formatToken)
        val close = matching(open)
        def spaceSplitWithoutPolicy(implicit line: sourcecode.Line) = {
          val indent: Length = right match {
            case T.KwIf() => StateColumn
            case T.KwFor() if !style.indentYieldKeyword => StateColumn
            case _ =>
              if (leftOwner.is[Term.ApplyInfix]) Num(0)
              else {
                val closeFt = tokens(close, -1)
                val willBreak = closeFt.left.is[T.Comment] &&
                  prevNonCommentSameLine(closeFt).hasBreak
                Num(if (willBreak) 2 else 0)
              }
          }
          val useSpace = style.spaces.inParentheses
          Split(Space(useSpace), 0).withIndent(indent, close, Before)
        }
        def spaceSplit(implicit line: sourcecode.Line) =
          spaceSplitWithoutPolicy.withPolicy(PenalizeAllNewlines(close, 1))
        def newlineSplit(
            cost: Int,
            forceDangle: Boolean
        )(implicit line: sourcecode.Line) = {
          val shouldDangle = forceDangle ||
            style.danglingParentheses.callSite
          val policy =
            if (!shouldDangle) NoPolicy
            else decideNewlinesOnlyBeforeClose(close)
          Split(Newline, cost)
            .withPolicy(policy)
            .withIndent(style.continuationIndent.callSite, close, Before)
        }
        if (isSingleLineComment(right))
          Seq(newlineSplit(0, isConfig))
        else
          style.newlines.source match {
            case Newlines.classic =>
              Seq(if (isConfig) newlineSplit(0, true) else spaceSplit)
            case Newlines.keep =>
              Seq(if (newlines != 0) newlineSplit(0, isConfig) else spaceSplit)
            case _ =>
              val singleLine = !isSuperfluousParenthesis(open, leftOwner) ||
                style.newlines.source.eq(Newlines.unfold) &&
                leftOwner.parent.exists {
                  case _: Template | _: Defn => false
                  case InfixApp(_) => false
                  case _ => true
                }
              Seq(
                if (!singleLine) spaceSplit
                else {
                  val singleLineInfixPolicy =
                    if (!isInfixApp(leftOwner)) None
                    else Some(getSingleLineInfixPolicy(close))
                  spaceSplitWithoutPolicy
                    .withSingleLine(close)
                    .andPolicyOpt(singleLineInfixPolicy)
                },
                newlineSplit(10, true)
              )
          }

      // Infix operator.
      case FormatToken(_: T.Ident, _, _) if isInfixOp(leftOwner) =>
        val InfixApp(app) = leftOwner.parent.get
        insideInfixSplit(app, formatToken)
      case FormatToken(_, _: T.Ident, _) if isInfixOp(rightOwner) =>
        val InfixApp(app) = rightOwner.parent.get
        insideInfixSplit(app, formatToken)

      // Case
      case FormatToken(_: T.KwCase, _, _) if leftOwner.is[Case] =>
        val owner = leftOwner.asInstanceOf[Case]
        val arrowFt = getCaseArrow(owner)
        val arrow = arrowFt.left
        val postArrowFt = nextNonCommentSameLine(arrowFt)
        val expire = lastTokenOpt(owner.body.tokens)
          .fold(postArrowFt)(x => nextNonCommentSameLine(tokens(x)))
          .left

        val bodyBlock = isCaseBodyABlock(arrowFt, owner)
        def defaultPolicy = decideNewlinesOnlyAfterToken(postArrowFt.left)
        val policy =
          if (bodyBlock || isAttachedSingleLineComment(arrowFt)) NoPolicy
          else if (isCaseBodyEnclosedAsBlock(postArrowFt, owner)) {
            val postParenFt = nextNonCommentSameLine(next(postArrowFt))
            val lparen = postParenFt.left
            val rparen = matching(lparen)
            if (postParenFt.right.start >= rparen.start) defaultPolicy
            else {
              val lindents = Seq(
                Indent(2, rparen, Before),
                Indent(-2, expire, After)
              )
              val split = Split(Newline, 0)
              val lsplit = Seq(split.withIndents(lindents))
              val rsplit = Seq(split)
              val open = Policy.after(lparen) {
                case d: Decision if d.formatToken eq postParenFt => lsplit
              }
              val close = Policy.on(rparen) {
                case d: Decision if d.formatToken.right eq rparen => rsplit
              }
              new Policy.Relay(open, close)
            }
          } else if (
            style.newlines.getBeforeMultiline.in(Newlines.fold, Newlines.keep)
          ) NoPolicy
          else defaultPolicy

        Seq(
          Split(Space, 0).withSingleLine(expire, killOnFail = true),
          Split(Space, 0, policy = policy)
            .withIndent(if (bodyBlock) 0 else 2, expire, After)
            .withIndent(if (bodyBlock) 4 else 2, arrow, After)
        )

      case tok @ FormatToken(_, cond @ T.KwIf(), _) if rightOwner.is[Case] =>
        val arrow = getCaseArrow(rightOwner.asInstanceOf[Case]).left
        val exclude = insideBlock[T.LeftBrace](tok, arrow)

        Seq(
          Split(Space, 0).withSingleLineNoOptimal(arrow, exclude = exclude),
          Split(Newline, 1).withPolicy(penalizeNewlineByNesting(cond, arrow))
        )

      // ForYield
      case tok @ FormatToken(_: T.LeftArrow, _, _)
          if leftOwner.is[Enumerator.Generator] =>
        val enumerator = leftOwner.asInstanceOf[Enumerator.Generator]
        getSplitsEnumerator(tok, enumerator.rhs)
      case tok @ FormatToken(_: T.Equals, _, _)
          if leftOwner.is[Enumerator.Val] =>
        val enumerator = leftOwner.asInstanceOf[Enumerator.Val]
        getSplitsEnumerator(tok, enumerator.rhs)

      // Inline comment
      case FormatToken(left, _: T.Comment, _) =>
        val forceBlankLine = formatToken.hasBreak &&
          blankLineBeforeDocstring(formatToken)
        val mod = if (forceBlankLine) Newline2x else getMod(formatToken)
        val indent = formatToken.meta.rightOwner match {
          case ts: Term.Select
              if !left.is[T.Comment] &&
                findPrevSelect(ts, style.encloseSelectChains).isEmpty =>
            Indent(2, nextNonComment(next(formatToken)).left, ExpiresOn.After)
          case _ => Indent.Empty
        }
        Seq(Split(mod, 0).withIndent(indent))
      // Commented out code should stay to the left
      case FormatToken(c: T.Comment, _, _) if isSingleLineComment(c) =>
        Seq(Split(Newline, 0))
      case FormatToken(c: T.Comment, _, _) =>
        Seq(Split(getMod(formatToken), 0))

      case FormatToken(_: T.KwImplicit, _, _)
          if !style.binPack.unsafeDefnSite &&
            !style.verticalMultiline.atDefnSite =>
        opensImplicitParamList(prevNonComment(prev(formatToken))).fold {
          Seq(Split(Space, 0))
        } { params =>
          val spaceSplit = Split(Space, 0)
            .notIf(style.newlines.forceAfterImplicitParamListModifier)
            .withPolicy(
              SingleLineBlock(params.last.tokens.last),
              style.newlines.notPreferAfterImplicitParamListModifier
            )
          Seq(
            spaceSplit,
            Split(Newline, if (spaceSplit.isActive) 1 else 0)
          )
        }

      case FormatToken(_, r, _) if optionalNewlines(hash(r)) =>
        def noAnnoLeft =
          leftOwner.is[Mod] ||
            !leftOwner.parent.exists(_.parent.exists(_.is[Mod.Annot]))
        def newlineOrBoth = {
          val spaceOk = !style.optIn.annotationNewlines
          Seq(Split(Space.orNL(spaceOk), 0), Split(Newline, 1).onlyIf(spaceOk))
        }
        style.newlines.source match {
          case _ if formatToken.hasBlankLine => Seq(Split(Newline2x, 0))
          case Newlines.unfold =>
            if (r.is[T.At]) newlineOrBoth
            else Seq(Split(Space.orNL(noAnnoLeft), 0))
          case Newlines.fold =>
            if (r.is[T.At]) Seq(Split(Space, 0), Split(Newline, 1))
            else if (noAnnoLeft) Seq(Split(Space, 0))
            else newlineOrBoth
          case _ =>
            val noNL = !style.optIn.annotationNewlines || formatToken.noBreak
            Seq(Split(Space.orNL(noNL), 0))
        }

      // Pat
      case FormatToken(T.Ident("|"), _, _) if leftOwner.is[Pat.Alternative] =>
        if (style.newlines.source eq Newlines.keep)
          Seq(Split(Space.orNL(newlines == 0), 0))
        else
          Seq(Split(Space, 0), Split(Newline, 1))
      case FormatToken(_, T.Ident("|"), _) if rightOwner.is[Pat.Alternative] =>
        val noNL = style.newlines.source.ne(Newlines.keep) || newlines == 0
        Seq(Split(Space.orNL(noNL), 0))

      case FormatToken(
            T.Ident(_) | Literal() | T.Interpolation.End() | T.Xml.End(),
            T.Ident(_) | Literal() | T.Xml.Start(),
            _
          ) =>
        Seq(
          Split(Space, 0)
        )

      // Case
      case FormatToken(_, T.KwMatch(), _) =>
        Seq(
          Split(Space, 0)
        )

      // Protected []
      case tok @ FormatToken(_, T.LeftBracket(), _)
          if isModPrivateProtected(leftOwner) =>
        Seq(
          Split(NoSplit, 0)
        )
      case tok @ FormatToken(T.LeftBracket(), _, _)
          if isModPrivateProtected(leftOwner) =>
        Seq(
          Split(NoSplit, 0)
        )

      // Term.ForYield
      case FormatToken(_, _: T.KwIf, _) if rightOwner.is[Enumerator.Guard] =>
        /* this covers the first guard only; second and later consecutive guards
         * are considered to start a new statement and are handled early, in the
         * "if startsStatement" matches */
        style.newlines.source match {
          case Newlines.fold =>
            val endOfGuard = rightOwner.tokens.last
            val exclude =
              insideBlock[LeftParenOrBrace](formatToken, endOfGuard)
            Seq(
              Split(Space, 0).withSingleLine(endOfGuard, exclude = exclude),
              Split(Newline, 1)
            )
          case Newlines.unfold =>
            Seq(Split(Newline, 0))
          case _ =>
            Seq(
              Split(Space, 0).onlyIf(newlines == 0),
              Split(Newline, 1)
            )
        }
      case FormatToken(T.KwYield(), _, _) if leftOwner.is[Term.ForYield] =>
        if (style.newlines.avoidAfterYield && !rightOwner.is[Term.If]) {
          Seq(Split(Space, 0))
        } else {
          val lastToken = leftOwner.asInstanceOf[Term.ForYield].body.tokens.last
          Seq(
            // Either everything fits in one line or break on =>
            Split(Space, 0).withSingleLineNoOptimal(lastToken),
            Split(Newline, 1).withIndent(2, lastToken, After)
          )
        }
      // Interpolation
      case FormatToken(_, _: T.Interpolation.Id, _) =>
        Seq(
          Split(Space, 0)
        )
      // Throw exception
      case FormatToken(T.KwThrow(), _, _) =>
        Seq(
          Split(Space, 0)
        )

      // Singleton types
      case FormatToken(_, T.KwType(), _) if rightOwner.is[Type.Singleton] =>
        Seq(
          Split(NoSplit, 0)
        )
      // seq to var args foo(seq:_*)
      case FormatToken(T.Colon(), T.Underscore(), _)
          if next(formatToken).meta.right.text == "*" =>
        Seq(
          Split(Space, 0)
        )
      case FormatToken(T.Underscore(), asterisk @ T.Ident("*"), _)
          if prev(formatToken).left.is[T.Colon] =>
        Seq(
          Split(NoSplit, 0)
        )

      // Xml
      case FormatToken(_, _: T.Xml.Start, _) =>
        Seq(
          Split(Space, 0)
        )
      case FormatToken(open: T.Xml.Start, _, _) =>
        val splits = Seq(Split(NoSplit, 0))
        if (prev(formatToken).left.is[T.LeftBrace])
          splits
        else
          withIndentOnXmlStart(open, splits)
      case FormatToken(_: T.Xml.SpliceStart, _, _)
          if style.xmlLiterals.assumeFormatted =>
        withIndentOnXmlSpliceStart(
          formatToken,
          Seq(Split(NoSplit, 0))
        )
      case FormatToken(T.Xml.Part(_), _, _) =>
        Seq(
          Split(NoSplit, 0)
        )
      case FormatToken(_, T.Xml.Part(_), _) =>
        Seq(
          Split(NoSplit, 0)
        )

      // Fallback
      case FormatToken(_, T.Dot(), _) =>
        Seq(
          Split(NoSplit, 0)
        )
      case FormatToken(left, T.Hash(), _) =>
        Seq(
          Split(Space(endsWithSymbolIdent(left)), 0)
        )
      case FormatToken(T.Hash(), ident: T.Ident, _) =>
        Seq(
          Split(Space(isSymbolicIdent(ident)), 0)
        )
      case FormatToken(T.Dot(), T.Ident(_) | T.KwThis() | T.KwSuper(), _) =>
        Seq(
          Split(NoSplit, 0)
        )
      case FormatToken(_, T.RightBracket(), _) =>
        Seq(
          Split(NoSplit, 0)
        )
      case FormatToken(_, close: T.RightParen, _) =>
        def modNoNL = {
          def allowSpace = rightOwner match {
            case _: Term.If | _: Term.While | _: Term.For | _: Term.ForYield =>
              isLastToken(close, rightOwner)
            case _ => true
          }
          Space(style.spaces.inParentheses && allowSpace)
        }
        val isNL = rightOwner.is[Pat.Alternative] &&
          style.newlines.source.eq(Newlines.keep) && newlines != 0
        Seq(Split(if (isNL) Newline else modNoNL, 0))

      case FormatToken(left, _: T.KwCatch | _: T.KwFinally, _)
          if style.newlines.alwaysBeforeElseAfterCurlyIf
            || !left.is[T.RightBrace] =>
        Seq(
          Split(Newline, 0)
        )

      case FormatToken(_, Keyword(), _) =>
        Seq(
          Split(Space, 0)
        )

      case FormatToken(Keyword() | Modifier(), _, _) =>
        Seq(
          Split(Space, 0)
        )
      case FormatToken(T.LeftBracket(), _, _) =>
        Seq(
          Split(NoSplit, 0)
        )
      case FormatToken(_, Delim(), _) =>
        Seq(
          Split(Space, 0)
        )
      case FormatToken(T.Underscore(), T.Ident("*"), _) =>
        Seq(
          Split(NoSplit, 0)
        )
      case FormatToken(T.RightArrow(), _, _) if leftOwner.is[Type.ByName] =>
        val mod = Space(style.spaces.inByNameTypes)
        Seq(
          Split(mod, 0)
        )
      case FormatToken(Delim(), _, _) =>
        Seq(
          Split(Space, 0)
        )
      case tok =>
        logger.debug("MISSING CASE:\n" + log(tok))
        Seq() // No solution available, partially format tree.
    }
  }

  /** Assigns possible splits to a FormatToken.
    *
    * The FormatToken can be considered as a node in a graph and the
    * splits as edges. Given a format token (a node in the graph), Route
    * determines which edges lead out from the format token.
    */
  def getSplits(formatToken: FormatToken): Seq[Split] = {
    val splits =
      getSplitsImpl(formatToken).filter(!_.isIgnored).map(_.adapt(formatToken))
    formatToken match {
      // TODO(olafur) refactor into "global policy"
      // Only newlines after inline comments.
      case FormatToken(c: T.Comment, _, _) if isSingleLineComment(c) =>
        val newlineSplits = splits.filter(_.isNL)
        if (newlineSplits.isEmpty) Seq(Split(Newline, 0))
        else newlineSplits
      case FormatToken(_, c: T.Comment, _)
          if isAttachedSingleLineComment(formatToken) =>
        splits.map(x => if (x.isNL) x.withMod(Space) else x)
      case _ => splits
    }
  }

  private implicit def int2num(n: Int): Num = Num(n)

  private def splitWithChain(
      isFirstWith: Boolean,
      owners: => Set[Tree],
      lastToken: => Token,
      extendsThenWith: => Boolean = false
  )(implicit line: sourcecode.Line, style: ScalafmtConfig): Seq[Split] =
    if (isFirstWith) {
      binPackParentConstructorSplits(
        owners,
        lastToken,
        IndentForWithChains,
        extendsThenWith
      )
    } else {
      Seq(Split(Space, 0), Split(Newline, 1))
    }

  private def getSplitsDefValEquals(
      ft: FormatToken,
      body: Tree,
      spaceIndents: Seq[Indent] = Seq.empty
  )(splits: => Seq[Split])(implicit
      style: ScalafmtConfig
  ): Seq[Split] = {
    def expire = body.tokens.last
    if (ft.right.is[T.LeftBrace]) // The block will take care of indenting by 2
      Seq(Split(Space, 0).withIndents(spaceIndents))
    else if (
      ft.right.is[T.Comment] &&
      (ft.hasBreak || nextNonCommentSameLine(next(ft)).hasBreak)
    )
      Seq(CtrlBodySplits.withIndent(Split(Space.orNL(ft.noBreak), 0), ft, body))
    else if (isJsNative(body))
      Seq(Split(Space, 0).withSingleLine(expire))
    else
      splits
  }

  private def getSplitsDefEquals(ft: FormatToken, body: Tree)(implicit
      style: ScalafmtConfig
  ): Seq[Split] = {
    val expire = body.tokens.last
    def baseSplit = Split(Space, 0)
    def newlineSplit(cost: Int)(implicit line: sourcecode.Line) =
      CtrlBodySplits.withIndent(Split(Newline, cost), ft, body)

    style.newlines.getBeforeMultilineDef match {
      case Newlines.classic | Newlines.keep if ft.hasBreak =>
        Seq(newlineSplit(0))

      case Newlines.classic =>
        Seq(baseSplit, newlineSplit(1))

      case Newlines.unfold =>
        Seq(baseSplit.withSingleLine(expire), newlineSplit(1))

      case _ => CtrlBodySplits.folded(ft, body)(newlineSplit)
    }
  }

  private def getSplitsValEquals(ft: FormatToken, body: Tree)(implicit
      style: ScalafmtConfig
  ): Seq[Split] = {
    def wouldDangle =
      ft.meta.leftOwner.parent.exists { lop =>
        if (isDefnSite(lop)) !shouldNotDangleAtDefnSite(lop, false)
        else isCallSite(lop) && style.danglingParentheses.callSite
      }

    val expire = body.tokens.last
    // rhsOptimalToken is too aggressive here
    val expireFt = tokens(expire)
    val optimalFt = expireFt.right match {
      case _: T.Comma => next(expireFt)
      case RightParenOrBracket() if !wouldDangle => next(expireFt)
      case _ => expireFt
    }
    val optimal = optimalFt.left
    def optimalWithComment =
      optimalFt.right match {
        case x: T.Comment if optimalFt.noBreak => x
        case _ => optimalFt.left
      }

    val penalty = ft.meta.leftOwner match {
      case l: Term.Assign if style.binPack.unsafeCallSite =>
        Constants.BinPackAssignmentPenalty
      case l: Term.Param if style.binPack.unsafeDefnSite =>
        Constants.BinPackAssignmentPenalty
      case _ => 0
    }

    def baseSpaceSplit(implicit line: sourcecode.Line) =
      Split(Space, 0).notIf(isSingleLineComment(ft.right))
    def twoBranches(implicit line: sourcecode.Line) =
      Left(
        baseSpaceSplit
          .withOptimalToken(optimal)
          .withPolicy {
            val exclude = insideBlock[T.LeftBrace](ft, expire)
            policyWithExclude(exclude, Policy.End.On, Policy.End.After)(
              Policy.End.Before(expire),
              new PenalizeAllNewlines(_, Constants.ShouldBeSingleLine)
            )
          }
      )
    val spaceSplit = (style.newlines.source match {
      case Newlines.classic if ft.hasBreak && ft.meta.leftOwner.is[Defn] =>
        Left(Split.ignored)
      case Newlines.classic =>
        body match {
          case _: Term.If => twoBranches
          case _: Term.ForYield => twoBranches
          case _: Term.Try | _: Term.TryWithHandler =>
            // we force newlines in try/catch/finally
            Left(Split.ignored)
          case t: Term.Apply if t.args.nonEmpty =>
            Left(baseSpaceSplit.withOptimalToken(optimalWithComment))
          case _ => Right(NoPolicy)
        }

      case Newlines.keep if ft.hasBreak => Left(Split.ignored)
      case Newlines.keep =>
        body match {
          case _: Term.If => twoBranches
          case _: Term.ForYield => twoBranches
          // we force newlines in try/catch/finally
          case _: Term.Try | _: Term.TryWithHandler => Left(Split.ignored)
          case _ => Right(NoPolicy)
        }

      case Newlines.fold =>
        body match {
          case t: Term.If =>
            Either.cond(
              !ifWithoutElse(t),
              SingleLineBlock(expire),
              baseSpaceSplit.withSingleLine(t.cond.tokens.last)
            )
          case _: Term.ForYield => twoBranches
          case _: Term.Try | _: Term.TryWithHandler =>
            Right(SingleLineBlock(expire))
          case InfixApp(ia) if style.newlines.formatInfix =>
            val end = getMidInfixToken(findLeftInfix(ia))
            val exclude = insideBlock[LeftParenOrBrace](ft, end)
            Right(SingleLineBlock(end, exclude = exclude))
          case _ =>
            val policy = PenalizeAllNewlines(expire, 1)
            Left(baseSpaceSplit.withOptimalToken(optimal).withPolicy(policy))
        }

      case Newlines.unfold
          if (ft.meta.rightOwner eq body) &&
            isSuperfluousParenthesis(ft.right, body) =>
        Right(NoPolicy)
      case Newlines.unfold =>
        body match {
          case _: Term.ForYield =>
            // unfold policy on yield forces a break
            // revert it if we are attempting a single line
            val noBreakOnYield = Policy.before(expire) {
              case Decision(ft, s) if s.isEmpty && ft.right.is[Token.KwYield] =>
                Seq(Split(Space, 0))
            }
            Right(SingleLineBlock(expire) & noBreakOnYield)
          // we force newlines in try/catch/finally
          case _: Term.Try | _: Term.TryWithHandler => Left(Split.ignored)
          // don't tuck curried apply
          case Term.Apply(_: Term.Apply, _) => Right(SingleLineBlock(expire))
          case EndOfFirstCall(end) => Left(baseSpaceSplit.withSingleLine(end))
          case _ => Right(SingleLineBlock(expire))
        }
    }).fold(
      identity,
      x => baseSpaceSplit.withPolicy(x).withOptimalToken(optimal)
    )
    Seq(
      spaceSplit,
      CtrlBodySplits
        .withIndent(Split(Newline, 1 + penalty), ft, body)
        .andPolicy(
          PenalizeAllNewlines(expire, 1),
          !style.newlines.sourceIgnored
        )
    )
  }

  private def getSplitsEnumerator(
      ft: FormatToken,
      body: => Tree
  )(implicit style: ScalafmtConfig): Seq[Split] =
    asInfixApp(ft.meta.rightOwner, style.newlines.formatInfix).fold {
      val expire = lastToken(body)
      val spaceIndents =
        if (!style.align.arrowEnumeratorGenerator) Seq.empty
        else Seq(Indent(StateColumn, expire, After))
      getSplitsDefValEquals(ft, body, spaceIndents) {
        CtrlBodySplits.get(ft, body, spaceIndents) {
          if (spaceIndents.nonEmpty)
            Split(Space, 0).withIndents(spaceIndents)
          else {
            val noSlb = body match {
              case _: Term.Try | _: Term.TryWithHandler => false
              case t: Term.If => ifWithoutElse(t)
              case _ => true
            }
            if (noSlb) Split(Space, 0).withOptimalToken(ft.right)
            else Split(Space, 0).withSingleLine(expire)
          }
        }(Split(Newline, _).withIndent(2, expire, After))
      }
    }(getInfixSplitsBeforeLhs(_, ft))

}
