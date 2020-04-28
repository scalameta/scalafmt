package org.scalafmt.internal

import org.scalafmt.Error.UnexpectedTree
import org.scalafmt.config.{
  ImportSelectors,
  NewlineCurlyLambda,
  Newlines,
  ScalafmtConfig
}
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

/**
  * Assigns splits to format tokens.
  *
  * NOTE(olafurpg). The pattern match in this file has gotten out of hand. It's
  * difficult even for myself to keep track of what's going on in some cases,
  * especially around applications and lambdas. I'm hoping to sunset this file
  * along with BestFirstSearch in favor of https://github.com/scalameta/scalafmt/issues/917
  */
class Router(formatOps: FormatOps) {

  import Constants._
  import LoggerOps._
  import TokenOps._
  import TreeOps._
  import formatOps._

  private def getSplitsImpl(formatToken: FormatToken): Seq[Split] = {
    implicit val style = styleMap.at(formatToken)
    val leftOwner = formatToken.meta.leftOwner
    val rightOwner = formatToken.meta.rightOwner
    val newlines = formatToken.newlinesBetween

    formatToken match {
      case FormatToken(_: T.BOF, _, _) =>
        Seq(
          Split(NoSplit, 0)
        )
      case FormatToken(_, _: T.EOF, _) =>
        Seq(
          Split(Newline, 0) // End files with trailing newline
        )
      case FormatToken(start: T.Interpolation.Start, _, _) =>
        val end = matching(start)
        val policy =
          if (isTripleQuote(start)) NoPolicy
          else penalizeAllNewlines(end, BreakSingleLineInterpolatedString)
        val split = Split(NoSplit, 0).withPolicy(policy)
        Seq(
          if (getStripMarginChar(formatToken).isEmpty) split
          else if (!style.align.stripMargin) split.withIndent(2, end, After)
          else // statecolumn - 1 because of margin characters |
            split
              .withIndent(StateColumn, end, After)
              .withIndent(-1, end, After)
        )
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
        val disallowSingleLineComments =
          style.importSelectors != ImportSelectors.singleLine
        val policy = SingleLineBlock(
          close,
          disallowSingleLineComments = disallowSingleLineComments
        )
        val newlineBeforeClosingCurly = newlinesOnlyBeforeClosePolicy(close)

        val newlinePolicy = style.importSelectors match {
          case ImportSelectors.noBinPack =>
            newlineBeforeClosingCurly.andThen(OneArgOneLineSplit(formatToken))
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
        val newlineBeforeClosingCurly = newlinesOnlyBeforeClosePolicy(close)
        val selfAnnotation: Option[Tokens] = leftOwner match {
          // Self type: trait foo { self => ... }
          case t: Template => Some(t.self.name.tokens).filter(_.nonEmpty)
          case _ => None
        }
        val isSelfAnnotation =
          style.optIn.selfAnnotationNewline && selfAnnotation.nonEmpty && (
            formatToken.hasBreak || style.newlines.sourceIgnored
          )
        val nl: Modification =
          if (isSelfAnnotation)
            getModCheckIndent(formatToken, math.max(newlines, 1))
          else
            NewlineT(tok.hasBlankLine || blankLineBeforeDocstring(open, right))

        val (lambdaExpire, lambdaArrow, lambdaIndent) =
          startsStatement(right) match {
            case Some(owner: Term.Function) =>
              val arrow = getFuncArrow(lastLambda(owner))
              val expire =
                arrow.getOrElse(tokens(owner.params.last.tokens.last))
              (expire, arrow.map(_.left), 0)
            case _ =>
              selfAnnotation match {
                case Some(anno) =>
                  val arrow = leftOwner.tokens.find(_.is[T.RightArrow])
                  val expire = arrow.getOrElse(anno.last)
                  (tokens(expire), arrow, 2)
                case _ =>
                  (null, None, 0)
              }
          }
        val lambdaPolicy =
          if (lambdaExpire == null) null
          else {
            val arrowOptimal = getOptimalTokenFor(lambdaExpire)
            newlineBeforeClosingCurly
              .andThen(SingleLineBlock(arrowOptimal))
              .andThen(decideNewlinesOnlyAfterToken(arrowOptimal))
          }

        def getSingleLineDecisionPre2019NovOpt =
          leftOwner.parent match {
            case Some(_: Term.If | _: Term.Try | _: Term.TryWithHandler) => None
            case _ => Some(Policy.emptyPf)
          }
        def getSingleLineDecisionFor2019Nov = {
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
            case _ if !style.activeForEdition_2020_01 => Seq.empty
            case _ => classifiersByParent
          }

          val breakSingleLineAfterClose = classifiers.nonEmpty && {
            val afterClose = closeFT.right
            classifiers.exists(_(afterClose))
          }
          if (!breakSingleLineAfterClose) Policy.emptyPf
          else decideNewlinesOnlyAfterClose(Split(Newline, 0))(close)
        }
        def getClassicSingleLineDecisionOpt =
          if (newlines > 0) None
          else if (style.activeForEdition_2019_11)
            Some(getSingleLineDecisionFor2019Nov)
          else
            getSingleLineDecisionPre2019NovOpt

        def getSingleLineLambdaDecisionOpt = {
          val ok = !style.newlines.alwaysBeforeCurlyBraceLambdaParams &&
            getSpaceAndNewlineAfterCurlyLambda(newlines)._1
          if (ok) Some(getSingleLineDecisionFor2019Nov) else None
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
            else Some(getSingleLineDecisionFor2019Nov)
          // old behaviour
          case _ =>
            if (lambdaPolicy == null) getClassicSingleLineDecisionOpt
            else if (!style.activeForEdition_2020_01) None
            else getSingleLineLambdaDecisionOpt
        }

        val singleLineSplit =
          singleLineDecisionOpt.fold(Split.ignored) { sld =>
            val useOpt = lambdaPolicy != null || style.activeForEdition_2020_03
            val expire = if (useOpt) endOfSingleLineBlock(closeFT) else close
            val policy =
              SingleLineBlock(expire, penaliseNewlinesInsideTokens = true)
                .andThen(sld)
            Split(xmlSpace(leftOwner), 0, policy = policy)
              .withOptimalToken(expire, killOnFail = true)
          }

        Seq(
          singleLineSplit,
          Split(Space, 0)
            .notIf(
              style.newlines.alwaysBeforeCurlyBraceLambdaParams ||
                isSelfAnnotation || lambdaPolicy == null ||
                style.newlines.sourceIs(Newlines.keep) && newlines != 0
            )
            .withOptimalTokenOpt(lambdaArrow)
            .withIndent(lambdaIndent, close, Before)
            .withPolicy(lambdaPolicy),
          Split(nl, 1)
            .withPolicy(newlineBeforeClosingCurly)
            .withIndent(2, close, Before)
        )
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
          else if (afterCurlySpace && style.activeForEdition_2020_01 &&
            (!rightOwner.is[Defn] || style.newlines.sourceIs(Newlines.fold)))
            Split(Space, 0)
              .withPolicy(
                SingleLineBlock(
                  getOptimalTokenFor(endOfFunction),
                  penaliseNewlinesInsideTokens = true
                )
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
            style.newlines.afterCurlyLambda != NewlineCurlyLambda.squash

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
            .withPolicy(SingleLineBlock(endOfFunction))
        def newlineSplit =
          Split(Newline, 1 + nestedApplies(leftOwner))
            .withIndent(indent, endOfFunction, expiresOn)
        val multiLineSplits =
          if (hasSingleLineComment)
            Seq(newlineSplit)
          else if (!style.activeForEdition_2020_01) {
            // older: if followed by an open brace, break after it, else now
            val hasBlock = nextNonComment(formatToken).right.is[T.LeftBrace]
            Seq(if (hasBlock) Split(Space, 0) else newlineSplit)
          } else {
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
      case tok @ FormatToken(arrow @ T.RightArrow(), right, between)
          if leftOwner.isInstanceOf[Case] =>
        val caseStat = leftOwner.asInstanceOf[Case]
        if (right.is[T.LeftBrace] && (caseStat.body eq rightOwner))
          // Redundant {} block around case statements.
          Seq(Split(Space, 0).withIndent(-2, rightOwner.tokens.last, After))
        else {
          def newlineSplit(cost: Int) =
            Split(NewlineT(noIndent = rhsIsCommentedOut(tok)), cost)
          def foldedSplits =
            caseStat.body match {
              case _ if right.is[T.KwCase] || isSingleLineComment(right) =>
                Right(Split.ignored)
              case t if t.tokens.isEmpty || caseStat.cond.isDefined =>
                Right(Split(Space, 0).withSingleLineOpt(t.tokens.lastOption))
              case t: Term.If if t.elsep.tokens.isEmpty =>
                // must not use optimal token here, will lead to column overflow
                Right(
                  Split(Space, 0).withSingleLineNoOptimal(t.cond.tokens.last)
                )
              case t @ (_: Term.ForYield | _: Term.Match) =>
                val end = t.tokens.last
                val exclude = insideBlockRanges[T.LeftBrace](tok, end)
                Right(Split(Space, 0).withSingleLine(end, exclude = exclude))
              case t @ SplitCallIntoParts(fun, _) if fun ne t =>
                val end = t.tokens.last
                val exclude = insideBlockRanges[LeftParenOrBrace](tok, end)
                val splits = Seq(
                  Split(Space, 0).withSingleLine(end, exclude),
                  newlineSplit(1).withPolicy(penalizeAllNewlines(end, 1))
                )
                Left(splits)
              case t =>
                Right(Split(Space, 0).withSingleLine(t.tokens.last))
            }
          val result = style.newlines.source match {
            case Newlines.fold => foldedSplits
            case Newlines.unfold =>
              Right(Split(Space, 0).onlyIf(right.is[T.Semicolon]))
            case _ =>
              Right(Split(Space, 0).notIf(formatToken.hasBreak))
          }
          result.fold(
            identity,
            x => {
              val nlCost = if (x.isIgnored) 0 else x.cost + 1
              Seq(x, newlineSplit(nlCost))
            }
          )
        }
      // New statement
      case tok @ FormatToken(T.Semicolon(), right, _)
          if newlines == 0 && startsStatement(right).isDefined =>
        val spaceSplit =
          if (style.newlines.sourceIs(Newlines.unfold)) Split.ignored
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

        if ((annoRight || annoLeft) &&
          style.optIn.annotationNewlines && !style.newlines.sourceIgnored)
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
            getInfixSplitsBeforeLhs(app, tok, Left(mod))
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
            T.RightBrace() | T.Underscore(),
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
              if style.spaces.afterTripleEquals &&
                t.tokens.map(_.syntax) == Seq("===") =>
            Space
          case name: Term.Name
              if style.spaces.afterSymbolicDefs && isSymbolicName(
                name.value
              ) && name.parent
                .exists(isDefDef) =>
            Space
          case _ => NoSplit
        }
        Seq(
          Split(modification, 0)
        )
      // Defn.{Object, Class, Trait}
      case tok @ FormatToken(T.KwObject() | T.KwClass() | T.KwTrait(), _, _) =>
        val expire = defnTemplate(leftOwner)
          .flatMap(templateCurly)
          .getOrElse(leftOwner.tokens.last)
        val forceNewlineBeforeExtends = Policy(expire) {
          case d @ Decision(t @ FormatToken(_, _: T.KwExtends, _), _)
              if t.meta.rightOwner == leftOwner =>
            d.onlyNewlinesWithoutFallback
        }
        Seq(
          Split(Space, 0).withSingleLine(expire, killOnFail = true),
          Split(Space, 1).withPolicy(forceNewlineBeforeExtends)
        )
      // DefDef
      case tok @ FormatToken(T.KwDef(), name @ T.Ident(_), _) =>
        Seq(
          Split(Space, 0)
        )
      case FormatToken(_: T.Equals, _, _) if defBody(leftOwner).isDefined =>
        val body = defBody(leftOwner).get
        asInfixApp(rightOwner, style.newlines.formatInfix).fold {
          getSplitsDefEquals(formatToken, body)
        }(getInfixSplitsBeforeLhs(_, formatToken, Right(body)))

      // Parameter opening for one parameter group. This format works
      // on the WHOLE defnSite (via policies)
      case ft @ FormatToken(LeftParenOrBracket(), _, _)
          if style.verticalMultiline.atDefnSite &&
            isDefnSiteWithParams(leftOwner) =>
        verticalMultiline(leftOwner, ft)(style)

      // Term.Apply and friends
      case FormatToken(T.LeftParen(), _, _)
          if style.optIn.configStyleArguments &&
            !style.newlinesBeforeSingleArgParenLambdaParams &&
            getLambdaAtSingleArgCallSite(formatToken).isDefined => {
        val lambda = getLambdaAtSingleArgCallSite(formatToken).get
        val lambdaLeft: Option[Token] =
          matchingOpt(functionExpire(lambda)._1).filter(_.is[T.LeftBrace])

        val arrowFt = getFuncArrow(lambda).get
        val lambdaIsABlock = lambdaLeft.contains(arrowFt.right)
        val lambdaToken =
          getOptimalTokenFor(if (lambdaIsABlock) next(arrowFt) else arrowFt)

        val close = matching(formatToken.left)
        val newlinePolicy =
          if (!style.danglingParentheses.callSite) None
          else Some(newlinesOnlyBeforeClosePolicy(close))
        val spacePolicy = SingleLineBlock(lambdaToken).orElse {
          if (lambdaIsABlock) None
          else
            newlinePolicy.map(
              delayedBreakPolicy(lambdaLeft.map(open => _.end < open.end))
            )
        }

        val noSplitMod = getNoSplit(formatToken, true)
        val newlinePenalty = 3 + nestedApplies(leftOwner)
        Seq(
          Split(noSplitMod, 0)
            .onlyIf(noSplitMod != null)
            .withSingleLine(close),
          Split(noSplitMod, 0, policy = spacePolicy)
            .onlyIf(noSplitMod != null)
            .withOptimalToken(lambdaToken),
          Split(Newline, newlinePenalty)
            .withPolicyOpt(newlinePolicy)
            .withIndent(style.continuationIndent.callSite, close, Before)
        )
      }

      case FormatToken(LeftParenOrBracket(), right, between)
          if style.optIn.configStyleArguments && isDefnOrCallSite(leftOwner) &&
            (opensConfigStyle(formatToken, false) || {
              forceConfigStyle(leftOwner) && !styleMap.forcedBinPack(leftOwner)
            }) =>
        val open = formatToken.left
        val indent = getApplyIndent(leftOwner, isConfigStyle = true)
        val close = matching(open)
        val newlineBeforeClose = newlinesOnlyBeforeClosePolicy(close)
        val extraIndent: Length =
          if (style.poorMansTrailingCommasInConfigStyle) Num(2)
          else Num(0)
        val isForcedBinPack = styleMap.forcedBinPack.contains(leftOwner)
        val policy =
          if (isForcedBinPack) newlineBeforeClose
          else OneArgOneLineSplit(formatToken).orElse(newlineBeforeClose)
        val implicitSplit =
          if (opensConfigStyleImplicitParamList(formatToken))
            Split(Space(style.spaces.inParentheses), 0)
              .withPolicy(policy.orElse(decideNewlinesOnlyAfterToken(right)))
              .withOptimalToken(right, killOnFail = true)
              .withIndent(indent, close, Before)
              .withIndent(extraIndent, right, Before)
          else Split.ignored
        Seq(
          implicitSplit,
          Split(Newline, if (implicitSplit.isActive) 1 else 0, policy = policy)
            .withIndent(indent, close, Before)
            .withIndent(extraIndent, right, Before)
        )

      case FormatToken(open @ LeftParenOrBracket(), right, between)
          if style.binPack.unsafeDefnSite && isDefnSite(leftOwner) =>
        val close = matching(open)
        val isBracket = open.is[T.LeftBracket]
        val indent = Num(style.continuationIndent.getDefnSite(leftOwner))
        if (isTuple(leftOwner)) {
          Seq(
            Split(NoSplit, 0).withPolicy(
              SingleLineBlock(close, disallowSingleLineComments = false)
            )
          )
        } else {
          def penalizeBrackets(penalty: Int): Policy =
            if (isBracket)
              penalizeAllNewlines(close, Constants.BracketPenalty * penalty + 3)
            else NoPolicy
          val bracketCoef = if (isBracket) Constants.BracketPenalty else 1
          val bracketPenalty = if (isBracket) 1 else 0
          val nestingPenalty = nestedApplies(leftOwner)

          val noSplitPenalizeNewlines = penalizeBrackets(1 + bracketPenalty)
          val mustDangle = style.newlines.sourceIgnored &&
            style.danglingParentheses.defnSite
          val noSplitPolicy: Policy =
            if (mustDangle) SingleLineBlock(close)
            else
              argumentStarts.get(hash(right)) match {
                case Some(arg) =>
                  val singleLine = SingleLineBlock(arg.tokens.last)
                  if (isBracket) {
                    noSplitPenalizeNewlines.andThen(singleLine.f)
                  } else {
                    singleLine
                  }
                case _ => noSplitPenalizeNewlines
              }
          val noSplitModification =
            if (right.is[T.Comment]) getMod(formatToken)
            else NoSplit
          val nlDanglePolicy =
            if (mustDangle) newlinesOnlyBeforeClosePolicy(close) else NoPolicy

          Seq(
            Split(noSplitModification, 0 + (nestingPenalty * bracketCoef))
              .withPolicy(noSplitPolicy)
              .withIndent(indent, close, Before),
            Split(Newline, (1 + nestingPenalty * nestingPenalty) * bracketCoef)
              .notIf(right.is[T.RightParen])
              .withPolicy(penalizeBrackets(1))
              .andThenPolicy(nlDanglePolicy)
              .withIndent(indent, close, Before)
          )
        }
      case FormatToken(LeftParenOrBracket(), _, _)
          if style.binPack.unsafeCallSite && isCallSite(leftOwner) =>
        val open = formatToken.left
        val close = matching(open)
        val indent = getApplyIndent(leftOwner)
        def baseNoSplit = Split(NoSplit, 0).withIndent(indent, close, Before)
        val singleLineOnly = style.binPack.literalsSingleLine &&
          styleMap.opensLiteralArgumentList(formatToken)

        val noSplit =
          if (singleLineOnly || style.newlines.sourceIgnored)
            baseNoSplit.withSingleLine(close)
          else {
            val opt = leftOwner.tokens.find(_.is[T.Comma]).orElse(Some(close))
            val isBracket = open.is[T.LeftBracket]
            // TODO(olafur) DRY. Same logic as in default.
            val exclude =
              if (isBracket)
                insideBlock[T.LeftBracket](formatToken, close)
              else
                insideBlock[T.LeftBrace](formatToken, close)
            val excludeRanges: Set[Range] =
              exclude.map((matchingParensRange _).tupled).toSet
            val unindent = UnindentAtExclude(exclude.keySet, Num(-indent.n))
            def ignoreBlocks(x: FormatToken): Boolean = {
              excludeRanges.exists(_.contains(x.left.end))
            }
            val policy =
              penalizeAllNewlines(close, 3, ignore = ignoreBlocks)
                .andThen(unindent)
            baseNoSplit.withOptimalTokenOpt(opt).withPolicy(policy)
          }

        val nlDanglePolicy =
          if (style.newlines.sourceIgnored &&
            style.danglingParentheses.callSite)
            newlinesOnlyBeforeClosePolicy(close)
          else NoPolicy
        val nlIndent = if (style.activeForEdition_2020_03) indent else Num(4)
        Seq(
          noSplit,
          Split(NewlineT(acceptNoSplit = singleLineOnly), 2)
            .withIndent(nlIndent, close, Before)
            .withSingleLineOpt(if (singleLineOnly) Some(close) else None)
            .andThenPolicy(nlDanglePolicy)
        )
      case FormatToken(T.LeftParen(), T.RightParen(), _) =>
        Seq(Split(NoSplit, 0))

      // If configured to skip the trailing space after `if` and other keywords, do so.
      case FormatToken(T.KwIf() | T.KwFor() | T.KwWhile(), T.LeftParen(), _)
          if !style.spaces.afterKeywordBeforeParen =>
        Seq(Split(NoSplit, 0))

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

        val sourceIgnored = style.newlines.sourceIgnored
        val notSingleEnclosedArgument =
          sourceIgnored && !(singleArgument && isEnclosedInMatching(args(0)))
        val useConfigStyle =
          style.optIn.configStyleArguments && notSingleEnclosedArgument

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

        val nestedPenalty = nestedApplies(leftOwner) + lhsPenalty
        val excludeRanges =
          if (isBracket) insideBlockRanges[T.LeftBracket](tok, close)
          else if (style.activeForEdition_2020_03 && multipleArgs ||
            notSingleEnclosedArgument &&
            style.newlines.sourceIs(Newlines.unfold))
            Set.empty[Range]
          else if (style.newlines.sourceIs(Newlines.fold) &&
            singleArgument &&
            (!notSingleEnclosedArgument || isExcludedTree(args(0))))
            parensRange(args(0).tokens.last).toSet
          else insideBlockRanges[T.LeftBrace](tok, close)

        val indent = getApplyIndent(leftOwner)

        def insideBraces(t: FormatToken): Boolean =
          excludeRanges.exists(_.contains(t.left.start))

        def singleLine(
            newlinePenalty: Int
        )(implicit line: sourcecode.Line): Policy = {
          val baseSingleLinePolicy = if (isBracket) {
            if (singleArgument)
              penalizeAllNewlines(
                close,
                newlinePenalty,
                penalizeLambdas = false
              )
            else SingleLineBlock(close)
          } else {
            val penalty =
              if (singleArgument) newlinePenalty
              else Constants.ShouldBeNewline
            penalizeAllNewlines(
              close,
              penalty = penalty,
              ignore = insideBraces,
              penalizeLambdas = !singleArgument,
              penaliseNewlinesInsideTokens = !singleArgument
            )
          }

          baseSingleLinePolicy
        }

        val newlineMod: Modification = NoSplit.orNL(right.is[T.LeftBrace])

        val defnSite = isDefnSite(leftOwner)
        val closeFormatToken = tokens(close)
        val expirationToken: Token =
          if (defnSite && !isBracket)
            defnSiteLastToken(closeFormatToken, leftOwner)
          else
            rhsOptimalToken(closeFormatToken)

        val mustDangle = style.activeForEdition_2020_01 && (
          expirationToken.is[T.Comment]
        )
        val shouldDangle =
          if (defnSite) !shouldNotDangleAtDefnSite(leftOwner, false)
          else style.danglingParentheses.callSite
        val wouldDangle = shouldDangle || (style.activeForEdition_2020_03 && {
          val beforeClose = prev(closeFormatToken)
          beforeClose.hasBreak && beforeClose.left.is[T.Comment]
        })

        val newlinePolicy: Policy =
          if (wouldDangle || mustDangle) {
            newlinesOnlyBeforeClosePolicy(close)
          } else {
            Policy.empty(close)
          }

        val handleImplicit = style.activeForEdition_2020_03 &&
          opensImplicitParamList(formatToken, args)

        val noSplitMod =
          if (handleImplicit &&
            style.newlines.forceBeforeImplicitParamListModifier)
            null
          else getNoSplit(formatToken, !isBracket)
        val noSplitIndent = if (right.is[T.Comment]) indent else Num(0)

        val align = {
          if (defnSite) style.align.openParenDefnSite
          else style.align.openParenCallSite
        } && (!handleImplicit ||
          style.newlines.forceAfterImplicitParamListModifier)
        val alignTuple = align && isTuple(leftOwner)

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
                if (newlinePolicy.isEmpty) Policy.emptyPf
                else decideNewlinesOnlyAfterToken(breakToken)
              val noSplitCost = 1 + nestedPenalty
              val newlineCost = Constants.ExceedColumnPenalty + noSplitCost
              Seq(
                Split(Newline, newlineCost)
                  .withPolicy(newlinePolicy)
                  .withIndent(indent, close, Before),
                Split(NoSplit, noSplitCost)
                  .withSingleLine(breakToken)
                  .andThenPolicy(
                    newlinePolicy.andThen(newlineAfterAssignDecision)
                  )
              )
            }

        val noSplitPolicy =
          if (wouldDangle || mustDangle && isBracket || useConfigStyle)
            SingleLineBlock(close, exclude = excludeRanges)
          else if (splitsForAssign.isDefined)
            singleLine(3)
          else
            singleLine(10)
        val oneArgOneLine =
          newlinePolicy.andThen(OneArgOneLineSplit(formatToken))
        val (implicitPenalty, implicitPolicy) =
          if (!handleImplicit) (2, Policy.emptyPf)
          else (0, decideNewlinesOnlyAfterToken(right))
        Seq(
          Split(noSplitMod, 0, policy = noSplitPolicy)
            .onlyIf(noSplitMod != null)
            .withOptimalToken(expirationToken)
            .withIndent(noSplitIndent, close, Before),
          Split(newlineMod, (1 + nestedPenalty) * bracketCoef)
            .withPolicy(newlinePolicy.andThen(singleLine(4)))
            .onlyIf(!multipleArgs && !alignTuple && splitsForAssign.isEmpty)
            .withOptimalToken(expirationToken)
            .withIndent(indent, close, Before),
          Split(noSplitMod, (implicitPenalty + lhsPenalty) * bracketCoef)
            .withPolicy(oneArgOneLine.andThen(implicitPolicy))
            .onlyIf(noSplitMod != null)
            .onlyIf(
              (notTooManyArgs && align) || (handleImplicit &&
                style.newlines.notBeforeImplicitParamListModifier)
            )
            .withIndent(if (align) StateColumn else indent, close, Before),
          Split(Newline, (3 + nestedPenalty) * bracketCoef)
            .withPolicy(oneArgOneLine)
            .onlyIf(!singleArgument && !alignTuple)
            .withIndent(indent, close, Before)
        ) ++ splitsForAssign.getOrElse(Seq.empty)

      // Closing def site ): ReturnType
      case FormatToken(left, T.Colon(), _)
          if style.newlines.sometimesBeforeColonInMethodReturnType &&
            defDefReturnType(leftOwner).isDefined =>
        val expire = lastToken(defDefReturnType(rightOwner).get)
        val penalizeNewlines =
          penalizeAllNewlines(expire, Constants.BracketPenalty)
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
          if style.newlines.neverInResultType &&
            defDefReturnType(leftOwner).isDefined =>
        val expire = lastToken(defDefReturnType(leftOwner).get)
        Seq(
          Split(Space, 0).withPolicy(
            SingleLineBlock(expire, disallowSingleLineComments = false)
          )
        )

      case FormatToken(T.LeftParen(), T.LeftBrace(), between) =>
        Seq(
          Split(NoSplit, 0)
        )

      case FormatToken(_, T.LeftBrace(), _) if isXmlBrace(rightOwner) =>
        Seq(
          Split(NoSplit, 0)
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
          if (!style.activeForEdition_2020_03 && !style.newlines.sourceIgnored)
            Seq.empty
          else
            (rightOwner match {
              case _: Term.PartialFunction | Term.Block(
                    List(_: Term.Function | _: Term.PartialFunction)
                  ) =>
                Seq(Split(Newline, 0))
              case _ =>
                val breakAfter =
                  rhsOptimalToken(next(nextNonCommentSameLine(formatToken)))
                val multiLine =
                  newlinesOnlyBeforeClosePolicy(close)
                    .orElse(decideNewlinesOnlyAfterToken(breakAfter))
                Seq(
                  Split(Newline, 0).withSingleLine(close, killOnFail = true),
                  Split(Space, 1, policy = multiLine)
                )
            }).map(_.onlyFor(SplitTag.OneArgPerLine))
        val sourceIsKeep = style.newlines.sourceIs(Newlines.keep)
        Seq(
          Split(Space, 0).onlyIf(newlines == 0 || !sourceIsKeep),
          Split(Newline, 0)
            .onlyIf(oneArgPerLineSplits.isEmpty)
            .onlyIf(newlines != 0)
            .onlyIf(sourceIsKeep || open.pos.endLine == close.pos.startLine)
            .withSingleLine(close, killOnFail = true)
        ) ++ oneArgPerLineSplits

      case FormatToken(_, _: T.LeftBrace, _) =>
        Seq(Split(Space, 0))

      // Delim
      case FormatToken(_, T.Comma(), _) =>
        Seq(
          Split(NoSplit, 0)
        )
      // These are mostly filtered out/modified by policies.
      case tok @ FormatToken(T.Comma(), right, _) =>
        // TODO(olafur) DRY, see OneArgOneLine.
        val binPack = isBinPack(leftOwner)
        argumentStarts.get(hash(right)) match {
          case Some(nextArg) if binPack =>
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
            val singleLineComment = isSingleLineComment(right)
            val noNewline = newlines == 0 && {
              singleLineComment || style.activeForEdition_2020_01 && {
                val nextTok = nextNonComment(tok).right
                // perhaps a trailing comma
                (nextTok ne right) && nextTok.is[RightParenOrBracket]
              }
            }
            Seq(
              Split(Space, 0).notIf(newlines != 0 && singleLineComment),
              Split(Newline, 1)
                .notIf(noNewline)
                .withIndent(indent, right, ExpiresOn.Before)
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
      case FormatToken(_: T.Equals, right, _) if (leftOwner match {
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
          if (rhs.is[Term.ApplyInfix])
            beforeInfixSplit(rhs.asInstanceOf[Term.ApplyInfix], formatToken)
          else
            getSplitsValEquals(formatToken, rhs)
        }(getInfixSplitsBeforeLhs(_, formatToken, Right(rhs)))

      case FormatToken(_, _: T.Dot, _)
          if style.newlines.sourceIgnored &&
            rightOwner.is[Term.Select] && findTreeWithParent(rightOwner) {
            case _: Type.Select | _: Importer | _: Pkg => Some(true)
            case _: Term.Select | SplitCallIntoParts(_, _) => None
            case _ => Some(false)
          }.isDefined =>
        Seq(Split(NoSplit, 0))

      case t @ FormatToken(left, _: T.Dot, _)
          if !style.newlines.sourceIs(Newlines.classic) &&
            rightOwner.is[Term.Select] =>
        val (expireTree, nextSelect) = findLastApplyAndNextSelect(rightOwner)
        val prevSelect = findPrevSelect(rightOwner.asInstanceOf[Term.Select])
        val expire = lastToken(expireTree)

        val baseSplits = style.newlines.source match {
          case _ if left.is[T.Comment] =>
            Seq(Split(Space.orNL(t.noBreak), 0))

          case Newlines.keep =>
            Seq(Split(NoSplit.orNL(t.noBreak), 0))

          case Newlines.unfold =>
            if (prevSelect.isEmpty && nextSelect.isEmpty)
              Seq(Split(NoSplit, 0), Split(Newline, 1))
            else {
              val forcedBreakPolicy = nextSelect.map { tree =>
                Policy(tree.name.tokens.head) {
                  case Decision(t @ FormatToken(_, _: T.Dot, _), s)
                      if t.meta.rightOwner eq tree =>
                    s.filter(_.modification.isNewline)
                }
              }
              Seq(
                Split(NoSplit, 0).withSingleLine(expire),
                Split(NewlineT(acceptNoSplit = true), 1)
                  .withPolicyOpt(forcedBreakPolicy)
              )
            }

          case Newlines.fold =>
            val end = nextSelect.fold(expire)(x => lastToken(x.qual))
            def exclude = insideBlockRanges[LeftParenOrBrace](t, end)
            Seq(
              Split(NoSplit, 0).withSingleLine(end, exclude),
              Split(NewlineT(acceptNoSplit = true), 1)
            )
        }

        val delayedBreakPolicy = nextSelect.map { tree =>
          Policy(tree.name.tokens.head) {
            case Decision(t @ FormatToken(_, _: T.Dot, _), s)
                if t.meta.rightOwner eq tree =>
              SplitTag.SelectChainFirstNL.activateOnly(s)
          }
        }

        // trigger indent only on the first newline
        val indent = Indent(Num(2), expire, After)
        val willBreak =
          nextNonCommentSameLine(tokens(formatToken, 2)).right.is[T.Comment]
        val splits = baseSplits.map { s =>
          if (willBreak || s.modification.isNewline) s.withIndent(indent)
          else s.andThenPolicyOpt(delayedBreakPolicy)
        }

        if (prevSelect.isEmpty) splits
        else baseSplits ++ splits.map(_.onlyFor(SplitTag.SelectChainFirstNL))

      case FormatToken(T.Ident(name), _: T.Dot, _) if isSymbolicName(name) =>
        Seq(Split(NoSplit, 0))

      case FormatToken(_: T.Underscore, _: T.Dot, _) =>
        Seq(Split(NoSplit, 0))

      case tok @ FormatToken(left, dot @ T.Dot() `:chain:` chain, _) =>
        val nestedPenalty = nestedSelect(rightOwner) + nestedApplies(leftOwner)
        val optimalToken = getSelectOptimalToken(chain.last)
        val expire =
          if (chain.length == 1) lastToken(chain.last)
          else optimalToken

        val breakOnEveryDot = Policy(expire) {
          case Decision(t @ FormatToken(_, _: T.Dot, _), _)
              if chain.contains(t.meta.rightOwner) =>
            val noNL = style.optIn.breaksInsideChains && t.noBreak
            Seq(Split(NoSplit.orNL(noNL), 1))
        }
        val exclude = getExcludeIf(expire)
        // This policy will apply to both the space and newline splits, otherwise
        // the newline is too cheap even it doesn't actually prevent other newlines.
        val penalizeNewlinesInApply = penalizeAllNewlines(expire, 2)
        val noSplitPolicy =
          SingleLineBlock(expire, exclude).andThen(penalizeNewlinesInApply)
        val newlinePolicy = breakOnEveryDot.andThen(penalizeNewlinesInApply)
        val ignoreNoSplit =
          style.optIn.breakChainOnFirstMethodDot && tok.hasBreak
        val chainLengthPenalty =
          if (style.newlines.penalizeSingleSelectMultiArgList &&
            chain.length < 2) {
            // penalize by the number of arguments in the rhs open apply.
            // I know, it's a bit arbitrary, but my manual experiments seem
            // to show that it produces OK output. The key insight is that
            // many arguments on the same line can be hard to read. By not
            // putting a newline before the dot, we force the argument list
            // to break into multiple lines.
            splitCallIntoParts.lift(tokens(tok, 2).meta.rightOwner) match {
              case Some((_, Left(args))) =>
                Math.max(0, args.length - 1)
              case Some((_, Right(argss))) =>
                Math.max(0, argss.map(_.length).sum - 1)
              case _ => 0
            }
          } else 0
        val nlCost = 2 + nestedPenalty + chainLengthPenalty
        Seq(
          Split(NoSplit, 0)
            .notIf(ignoreNoSplit)
            .withPolicy(noSplitPolicy),
          Split(NewlineT(acceptNoSplit = true), nlCost)
            .withPolicy(newlinePolicy)
            .withIndent(2, optimalToken, After)
        )

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
          style.continuationIndent.extendSite
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
              templateCurly(template).getOrElse(template.tokens.last)
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
                Policy(expire) {
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
        val indent: Length =
          if (style.align.ifWhileOpenParen) StateColumn
          else style.continuationIndent.callSite
        val oldClassic = !style.activeForEdition_2020_03 &&
          style.newlines.sourceIs(Newlines.classic)
        Seq(
          Split(NoSplit, 0)
            .withIndent(indent, close, if (oldClassic) After else Before)
            .withPolicy(penalizeNewlines)
        )
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
            Policy(expire) {
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
            case _: Term.While => style.activeForEdition_2020_01
            case _ => false
          }) && !isFirstOrLastToken(close, leftOwner) =>
        val expire = leftOwner match {
          case t: Term.If => t.thenp.tokens.last
          case t: Term.For => t.body.tokens.last
          case t: Term.ForYield => t.body.tokens.last
          case t: Term.While => t.body.tokens.last
        }
        val noSpace = isSingleLineComment(right) || shouldBreak(formatToken)
        def exclude = insideBlockRanges[T.LeftBrace](formatToken, expire)
        Seq(
          Split(Space, 0)
            .notIf(noSpace)
            .withPolicy(SingleLineBlock(expire, exclude = exclude)),
          Split(Newline, 1).withIndent(2, expire, After)
        )
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
        def exclude = insideBlockRanges[T.LeftBrace](formatToken, expire)
        Seq(
          Split(Space, 0)
            .notIf(noSpace)
            .withPolicy(SingleLineBlock(expire, exclude = exclude)),
          Split(Newline, 1)
        )
      // Last else branch
      case FormatToken(_: T.KwElse, _, _) if (leftOwner match {
            case t: Term.If => !t.elsep.is[Term.If]
            case x => throw new UnexpectedTree[Term.If](x)
          }) =>
        val expire = leftOwner.asInstanceOf[Term.If].elsep.tokens.last
        val noSpace = shouldBreak(formatToken)
        Seq(
          Split(Space, 0).notIf(noSpace).withPolicy(SingleLineBlock(expire)),
          Split(Newline, 1).withIndent(2, expire, After)
        )

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
        val isConfig = opensConfigStyle(formatToken, false)
        val close = matching(open)
        val editionActive = style.activeForEdition_2020_03 ||
          !style.newlines.sourceIn(Newlines.classic)
        def spaceSplitWithoutPolicy = {
          val indent: Length = right match {
            case T.KwIf() => StateColumn
            case T.KwFor() if !style.indentYieldKeyword => StateColumn
            case _ => Num(0)
          }
          val useSpace = editionActive && style.spaces.inParentheses
          Split(Space(useSpace), 0).withIndent(indent, close, After)
        }
        def spaceSplit =
          spaceSplitWithoutPolicy.withPolicy(penalizeAllNewlines(close, 1))
        def newlineSplit(cost: Int, forceDangle: Boolean) = {
          val shouldDangle = forceDangle ||
            editionActive && style.danglingParentheses.callSite
          val policy =
            if (!shouldDangle) NoPolicy
            else newlinesOnlyBeforeClosePolicy(close)
          Split(Newline, cost)
            .withPolicy(policy)
            .withIndent(style.continuationIndent.callSite, close, Before)
        }
        if (isSingleLineComment(right) && editionActive)
          Seq(newlineSplit(0, isConfig))
        else
          style.newlines.source match {
            case Newlines.classic =>
              Seq(if (isConfig) newlineSplit(0, true) else spaceSplit)
            case Newlines.keep =>
              Seq(if (newlines != 0) newlineSplit(0, isConfig) else spaceSplit)
            case _ =>
              val singleLine = !isSuperfluousParenthesis(open, leftOwner) ||
                style.newlines.sourceIs(Newlines.unfold) &&
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
                    .andThenPolicyOpt(singleLineInfixPolicy)
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
      case tok @ FormatToken(cs @ T.KwCase(), _, _) if leftOwner.is[Case] =>
        val owner = leftOwner.asInstanceOf[Case]
        val arrow = getCaseArrow(owner).left
        // TODO(olafur) expire on token.end to avoid this bug.
        val expire = Option(owner.body)
          .filter(_.tokens.exists(!_.is[Trivia]))
          // edge case, if body is empty expire on arrow
          .fold(arrow)(t => getOptimalTokenFor(lastToken(t)))

        Seq(
          Split(Space, 0).withSingleLine(expire, killOnFail = true),
          Split(Space, 1)
            .withPolicy(
              Policy(expire) {
                case d @ Decision(t @ FormatToken(`arrow`, right, _), _)
                    // TODO(olafur) any other corner cases?
                    if !right.isInstanceOf[T.LeftBrace] &&
                      !isAttachedSingleLineComment(t) =>
                  d.onlyNewlinesWithoutFallback
              },
              ignore = style.newlines.sourceIs(Newlines.fold)
            )
            .withIndent(2, expire, After) // case body indented by 2.
            .withIndent(2, arrow, After) // cond body indented by 4.
        )

      case tok @ FormatToken(_, cond @ T.KwIf(), _) if rightOwner.is[Case] =>
        val arrow = getCaseArrow(rightOwner.asInstanceOf[Case]).left
        val exclude = insideBlockRanges[T.LeftBrace](tok, arrow)
        val singleLine = SingleLineBlock(arrow, exclude = exclude)

        Seq(
          Split(Space, 0, policy = singleLine),
          Split(Newline, 1).withPolicy(penalizeNewlineByNesting(cond, arrow))
        )

      // ForYield
      case tok @ FormatToken(_: T.LeftArrow, _, _)
          if leftOwner.is[Enumerator.Generator] =>
        asInfixApp(rightOwner, style.newlines.formatInfix).fold {
          getSplitsEnumerator(tok)
        } { app =>
          val rhs = leftOwner.asInstanceOf[Enumerator.Generator].rhs
          getInfixSplitsBeforeLhs(app, formatToken, Right(rhs))
        }
      case tok @ FormatToken(_: T.Equals, _, _)
          if leftOwner.is[Enumerator.Val] =>
        asInfixApp(rightOwner, style.newlines.formatInfix).fold {
          getSplitsEnumerator(tok)
        } { app =>
          val rhs = leftOwner.asInstanceOf[Enumerator.Val].rhs
          getInfixSplitsBeforeLhs(app, formatToken, Right(rhs))
        }

      // Inline comment
      case FormatToken(left, c: T.Comment, _) =>
        val mod =
          if (formatToken.hasBreak && blankLineBeforeDocstring(left, c))
            Newline2x
          else getMod(formatToken)
        Seq(Split(mod, 0))
      // Commented out code should stay to the left
      case FormatToken(c: T.Comment, _, _) if isSingleLineComment(c) =>
        Seq(Split(Newline, 0))
      case FormatToken(c: T.Comment, _, _) =>
        Seq(Split(getMod(formatToken), 0))

      case FormatToken(_: T.KwImplicit, _, _)
          if style.activeForEdition_2020_03 &&
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
      case tok @ FormatToken(T.Ident("|"), _, _)
          if leftOwner.is[Pat.Alternative] =>
        Seq(
          Split(Space, 0),
          Split(Newline, 1)
        )
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
              insideBlockRanges[LeftParenOrBrace](formatToken, endOfGuard)
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
            Split(Space, 0).withPolicy(SingleLineBlock(lastToken)),
            Split(Newline, 1).withIndent(2, lastToken, After)
          )
        }
      // Interpolation
      case FormatToken(_, T.Interpolation.Id(_) | T.Xml.Start(), _) =>
        Seq(
          Split(Space, 0)
        )
      case FormatToken(T.Interpolation.Id(_) | T.Xml.Start(), _, _) =>
        Seq(
          Split(NoSplit, 0)
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
          if next(formatToken).right.syntax == "*" =>
        Seq(
          Split(Space, 0)
        )
      case FormatToken(T.Underscore(), asterisk @ T.Ident("*"), _)
          if prev(formatToken).left.is[T.Colon] =>
        Seq(
          Split(NoSplit, 0)
        )
      // Xml
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
      case FormatToken(_, T.RightParen(), _) =>
        val mod =
          Space(style.spaces.inParentheses && isDefnOrCallSite(rightOwner))
        Seq(
          Split(mod, 0)
        )

      case FormatToken(left, _: T.KwCatch | _: T.KwFinally, _)
          if style.newlinesBetweenCurlyAndCatchFinally
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

  /**
    * Assigns possible splits to a FormatToken.
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
        val newlineSplits = splits.filter(_.modification.isNewline)
        if (newlineSplits.isEmpty) Seq(Split(Newline, 0))
        else newlineSplits
      case FormatToken(_, c: T.Comment, _)
          if isAttachedSingleLineComment(formatToken) =>
        splits.map(x =>
          if (x.modification.isNewline) x.copy(modification = Space)(x.line)
          else x
        )
      case _ => splits
    }
  }

  private implicit def int2num(n: Int): Num = Num(n)

  private def splitWithChain(
      isFirstWith: Boolean,
      chain: => Set[Tree],
      lastToken: => Token
  ): Seq[Split] =
    if (isFirstWith) {
      binPackParentConstructorSplits(chain, lastToken, IndentForWithChains)
    } else {
      Seq(Split(Space, 0), Split(Newline, 1))
    }

  private def getSplitsDefEquals(ft: FormatToken, body: Tree)(implicit
      style: ScalafmtConfig
  ): Seq[Split] = {
    val expire = body.tokens.last
    def excludeOld =
      getExcludeIf(
        expire,
        {
          case T.RightBrace() => true
          case close @ T.RightParen()
              if opensConfigStyle(tokens(matching(close)), true) =>
            // Example:
            // def x = foo(
            //     1
            // )
            true
          case T.RightParen() if !style.newlines.alwaysBeforeMultilineDef =>
            true
          case _ => false
        }
      )
    def exclude =
      if (style.activeForEdition_2020_03
        && style.newlines.alwaysBeforeMultilineDef)
        Set.empty[Range]
      else excludeOld
    if (ft.right.is[T.LeftBrace])
      // The block will take care of indenting by 2.
      Seq(Split(Space, 0))
    else if (isSingleLineComment(ft.right))
      Seq(Split(Newline, 0).withIndent(2, expire, After))
    else if (isJsNative(ft.right)) {
      val spacePolicy =
        if (!style.newlines.alwaysBeforeMultilineDef) Policy.NoPolicy
        else SingleLineBlock(expire, exclude = exclude)
      Seq(Split(Space, 0, policy = spacePolicy))
    } else {
      val spacePolicy = style.newlines.source match {
        case Newlines.classic =>
          if (ft.hasBreak) null
          else if (style.newlines.alwaysBeforeMultilineDef)
            SingleLineBlock(expire, exclude = exclude)
          else Policy.NoPolicy

        case Newlines.keep =>
          if (ft.hasBreak) null
          else if (style.newlines.alwaysBeforeMultilineDef)
            SingleLineBlock(expire)
          else Policy.NoPolicy

        case Newlines.unfold => SingleLineBlock(expire)

        case Newlines.fold if style.newlines.alwaysBeforeMultilineDef =>
          SingleLineBlock(expire)
        case Newlines.fold =>
          body match {
            case _: Term.Try | _: Term.TryWithHandler =>
              SingleLineBlock(expire)
            case t: Term.If =>
              if (t.elsep.tokens.isEmpty)
                SingleLineBlock(t.cond.tokens.last)
              else
                SingleLineBlock(expire)
            case _ => penalizeAllNewlines(expire, 1)
          }
      }
      Seq(
        Split(Space, 0, policy = spacePolicy).notIf(spacePolicy == null),
        Split(Newline, 1)
          .withIndent(2, expire, After)
          .withPolicy(
            penalizeAllNewlines(expire, 1),
            !style.newlines.sourceIgnored
          )
      )
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
    val optimal = tokens(expire).right match {
      case x: T.Comma => x
      case x @ RightParenOrBracket() if !wouldDangle => x
      case _ => expire
    }

    val penalty = ft.meta.leftOwner match {
      case l: Term.Assign if style.binPack.unsafeCallSite =>
        Constants.BinPackAssignmentPenalty
      case l: Term.Param if style.binPack.unsafeDefnSite =>
        Constants.BinPackAssignmentPenalty
      case _ => 0
    }

    def baseSpaceSplit =
      Split(Space, 0).notIf(isSingleLineComment(ft.right))
    def twoBranches =
      Left(
        baseSpaceSplit
          .withOptimalToken(optimal)
          .withPolicy {
            val excludeRanges =
              insideBlockRanges[T.LeftBrace](ft, expire)
            penalizeAllNewlines(
              expire,
              Constants.ShouldBeSingleLine,
              ignore = x => excludeRanges.exists(_.contains(x.left.start))
            )
          }
      )
    val okNewline = !isJsNative(ft.right)
    val spaceSplit = (style.newlines.source match {
      case Newlines.classic
          if okNewline && ft.hasBreak && ft.meta.leftOwner.is[Defn] =>
        Left(Split.ignored)
      case Newlines.classic =>
        body match {
          case _: Term.If => twoBranches
          case _: Term.ForYield => twoBranches
          case _: Term.Try | _: Term.TryWithHandler
              if style.activeForEdition_2019_11 && okNewline =>
            // we force newlines in try/catch/finally
            Left(Split.ignored)
          case _ => Right(NoPolicy)
        }

      case Newlines.keep if okNewline && ft.hasBreak => Left(Split.ignored)
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
              t.elsep.tokens.nonEmpty,
              SingleLineBlock(expire),
              baseSpaceSplit.withSingleLine(t.cond.tokens.last)
            )
          case _: Term.ForYield => twoBranches
          case _: Term.Try | _: Term.TryWithHandler =>
            Right(SingleLineBlock(expire))
          case InfixApp(ia) if style.newlines.formatInfix =>
            val end = getMidInfixToken(findLeftInfix(ia))
            val exclude = insideBlockRanges[LeftParenOrBrace](ft, end)
            Right(SingleLineBlock(end, exclude = exclude))
          case _ =>
            val policy = penalizeAllNewlines(expire, 1)
            Left(baseSpaceSplit.withOptimalToken(optimal).withPolicy(policy))
        }

      case Newlines.unfold
          if (ft.meta.rightOwner eq body) &&
            isSuperfluousParenthesis(ft.right, body) =>
        Right(NoPolicy)
      case Newlines.unfold =>
        body match {
          case _: Term.If => Right(SingleLineBlock(expire))
          case _: Term.ForYield =>
            // unfold policy on yield forces a break
            // revert it if we are attempting a single line
            val noBreakOnYield: Policy.Pf = {
              case Decision(ft, s) if s.isEmpty && ft.right.is[Token.KwYield] =>
                Seq(Split(Space, 0))
            }
            Right(SingleLineBlock(expire).andThen(noBreakOnYield))
          // we force newlines in try/catch/finally
          case _: Term.Try | _: Term.TryWithHandler => Left(Split.ignored)
          // don't tuck curried apply
          case Term.Apply(_: Term.Apply, _) => Right(SingleLineBlock(expire))
          case EndOfFirstCall(end) => Left(baseSpaceSplit.withSingleLine(end))
          case _ if okNewline && ft.meta.leftOwner.is[Defn] =>
            Right(SingleLineBlock(expire))
          case _ => Right(NoPolicy)
        }
    }).fold(
      identity,
      x => baseSpaceSplit.withPolicy(x).withOptimalToken(optimal)
    )
    Seq(
      spaceSplit,
      Split(Newline, 1 + penalty)
        .onlyIf(okNewline || spaceSplit.isIgnored)
        .withIndent(2, expire, After)
        .withPolicy(
          penalizeAllNewlines(expire, 1),
          !style.newlines.sourceIgnored
        )
    )
  }

  private def getSplitsEnumerator(
      ft: FormatToken
  )(implicit style: ScalafmtConfig): Seq[Split] = {
    val postCommentFT = nextNonCommentSameLine(ft)
    val expire = lastToken(ft.meta.leftOwner)
    if (!style.activeForEdition_2020_03 &&
      style.newlines.sourceIs(Newlines.classic))
      Seq(Split(Space, 0))
    else if (postCommentFT.right.is[T.Comment])
      Seq(Split(Space.orNL(ft.noBreak), 0).withIndent(2, expire, After))
    else if (style.newlines.sourceIs(Newlines.keep))
      Seq(
        if (ft.noBreak)
          Split(Space, 0)
            .withIndents(arrowEnumeratorGeneratorAlignIndents(expire))
        else Split(Newline, 0).withIndent(2, expire, After)
      )
    else {
      val close = getClosingIfEnclosedInMatching(postCommentFT.meta.rightOwner)
      val spaceSplit = style.newlines.source match {
        case _ if close.exists(_.is[T.RightBrace]) => Split(Space, 1)

        case Newlines.unfold =>
          if (close.isEmpty) Split.ignored else Split(Space, 1)

        case Newlines.classic if style.align.arrowEnumeratorGenerator =>
          Split(Space, 1)

        case Newlines.fold | Newlines.classic =>
          postCommentFT.meta.rightOwner match {
            case _: Term.Try | _: Term.TryWithHandler => Split.ignored
            case t: Term.If if t.elsep.tokens.nonEmpty => Split.ignored
            case t: Term.If =>
              Split(Space, 1).withSingleLine(t.cond.tokens.last)
            case _ =>
              def exclude =
                insideBlockRanges[LeftParenOrBrace](postCommentFT, expire)
              Split(Space, 0).withSingleLine(expire, exclude = exclude)
          }
      }

      Seq(
        Split(Space, 0)
          .onlyIf(spaceSplit.isIgnored || spaceSplit.cost != 0)
          .withSingleLine(expire),
        spaceSplit
          .withIndents(arrowEnumeratorGeneratorAlignIndents(expire))
          .withIndentOpt(close.map(Indent(Num(2), _, Before))),
        Split(Newline, if (spaceSplit.isIgnored) 1 else spaceSplit.cost + 1)
          .onlyIf(ft eq postCommentFT)
          .withIndent(2, expire, After)
      )
    }
  }

  private def arrowEnumeratorGeneratorAlignIndents(
      expire: Token
  )(implicit style: ScalafmtConfig) =
    if (style.align.arrowEnumeratorGenerator) {
      Seq(
        Indent(StateColumn, expire, After),
        /**
          * This gap is necessary for pretty alignment multiline expressions
          * on the right-side of enumerator.
          * Without:
          * ```
          * for {
          *    a <- new Integer {
          *          value = 1
          *        }
          *   x <- if (variable) doSomething
          *       else doAnything
          * }
          * ```
          *
          * With:
          * ```
          * for {
          *    a <- new Integer {
          *           value = 1
          *         }
          *   x <- if (variable) doSomething
          *        else doAnything
          * }
          * ```
          * */
        Indent(Num(1), expire, After)
      )
    } else {
      Seq.empty
    }

}
