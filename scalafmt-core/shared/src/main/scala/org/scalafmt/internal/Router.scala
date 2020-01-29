package org.scalafmt.internal

import org.scalafmt.Error.UnexpectedTree
import org.scalafmt.config.{ImportSelectors, NewlineCurlyLambda}
import org.scalafmt.internal.ExpiresOn.{Left, Right}
import org.scalafmt.internal.Length.{Num, StateColumn}
import org.scalafmt.internal.Policy.NoPolicy
import org.scalafmt.util._

import scala.collection.mutable
import scala.language.implicitConversions
import scala.meta.classifiers.Classifier
import scala.meta.tokens.{Token, Tokens}
import scala.meta.tokens.{Token => T}
import scala.meta.{
  Case,
  Defn,
  Enumerator,
  Import,
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

  private def getSplits(formatToken: FormatToken): Seq[Split] = {
    implicit val style = styleMap.at(formatToken)
    val leftOwner = owners(formatToken.left)
    val rightOwner = owners(formatToken.right)
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
      case FormatToken(start @ T.Interpolation.Start(), _, _) =>
        val isStripMargin = isMarginizedString(start)
        val end = matching(start)
        val policy =
          if (isTripleQuote(start)) NoPolicy
          else penalizeAllNewlines(end, BreakSingleLineInterpolatedString)
        Seq(
          // statecolumn - 1 because of margin characters |
          Split(NoSplit, 0, ignoreIf = !isStripMargin)
            .withPolicy(policy)
            .withIndent(StateColumn, end, Left)
            .withIndent(-1, end, Left),
          Split(NoSplit, 0, ignoreIf = isStripMargin).withPolicy(policy)
        )
      case FormatToken(
          T.Interpolation.Id(_) | T.Interpolation.Part(_) |
          T.Interpolation.Start() | T.Interpolation.SpliceStart(),
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
      case FormatToken(T.Dot(), open @ T.LeftBrace(), _)
          if parents(rightOwner).exists(_.is[Import]) =>
        Seq(
          Split(NoSplit, 0)
        )
      // Import left brace
      case FormatToken(open @ T.LeftBrace(), _, _)
          if parents(leftOwner).exists(_.is[Import]) =>
        val close = matching(open)
        val disallowSingleLineComments = style.importSelectors != ImportSelectors.singleLine
        val policy = SingleLineBlock(
          close,
          disallowSingleLineComments = disallowSingleLineComments
        )
        val newlineBeforeClosingCurly = newlinesOnlyBeforeClosePolicy(close)

        val newlinePolicy = style.importSelectors match {
          case ImportSelectors.noBinPack =>
            newlineBeforeClosingCurly.andThen(OneArgOneLineSplit(open))
          case ImportSelectors.binPack =>
            newlineBeforeClosingCurly
          case ImportSelectors.singleLine =>
            SingleLineBlock(close)
        }

        Seq(
          Split(if (style.spaces.inImportCurlyBraces) Space else NoSplit, 0)
            .withPolicy(policy),
          Split(
            Newline,
            1,
            ignoreIf = style.importSelectors == ImportSelectors.singleLine
          ).withPolicy(newlinePolicy)
            .withIndent(2, close, Right)
        )
      // Interpolated string left brace
      case FormatToken(open @ T.LeftBrace(), _, _)
          if leftOwner.is[SomeInterpolate] =>
        Seq(
          Split(NoSplit, 0)
        )
      case FormatToken(_, close @ T.RightBrace(), _)
          if parents(rightOwner).exists(_.is[Import]) ||
            rightOwner.is[SomeInterpolate] =>
        val isInterpolate = rightOwner.is[Term.Interpolate]
        Seq(
          Split(
            if (style.spaces.inImportCurlyBraces && !isInterpolate) Space
            else NoSplit,
            0
          )
        )
      case FormatToken(T.Dot(), underscore @ T.Underscore(), _)
          if parents(rightOwner).exists(_.is[Import]) =>
        Seq(
          Split(NoSplit, 0)
        )

      // { ... } Blocks
      case tok @ FormatToken(open @ T.LeftBrace(), right, between) =>
        val close = matching(open)
        val newlineBeforeClosingCurly = newlinesOnlyBeforeClosePolicy(close)
        val selfAnnotation: Option[Tokens] = leftOwner match {
          // Self type: trait foo { self => ... }
          case t: Template => Some(t.self.name.tokens).filter(_.nonEmpty)
          case _ => None
        }
        val isSelfAnnotation =
          style.optIn.selfAnnotationNewline &&
            newlines > 0 &&
            selfAnnotation.nonEmpty
        val nl: Modification =
          if (isSelfAnnotation) newlines2Modification(newlines, isNoIndent(tok))
          else NewlineT(shouldGet2xNewlines(tok, style, owners))

        val (lambdaExpire, lambdaArrow, lambdaIndent) =
          statementStarts
            .get(hash(right))
            .collect {
              case owner: Term.Function =>
                val arrow = lastLambda(owner).tokens.find(_.is[T.RightArrow])
                val expire = arrow.getOrElse(owner.params.last.tokens.last)
                (expire, arrow, 0)
            }
            .getOrElse {
              selfAnnotation match {
                case Some(tokens) =>
                  val arrow = leftOwner.tokens.find(_.is[T.RightArrow])
                  val expire = arrow.getOrElse(tokens.last)
                  (expire, arrow, 2)
                case _ =>
                  (null, None, 0)
              }
            }
        val lambdaPolicy =
          if (lambdaExpire == null) null
          else {
            newlineBeforeClosingCurly
              .andThen(SingleLineBlock(lambdaExpire))
          }

        def getSingleLineDecisionPre2019Nov = leftOwner.parent match {
          case Some(_: Term.If | _: Term.Try | _: Term.TryWithHandler) => null
          case _ => Policy.emptyPf
        }
        def getSingleLineDecisionFor2019Nov = {
          type Classifiers = Seq[Classifier[Token, _]]
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
            case _ if !style.activeForEdition_2020_01 => Seq.empty
            case _ => classifiersByParent
          }

          val breakSingleLineAfterClose = classifiers.nonEmpty && {
            val afterClose = tokens(close).right
            classifiers.exists(_(afterClose))
          }
          if (!breakSingleLineAfterClose) Policy.emptyPf
          else decideNewlinesOnlyAfterClose(Split(Newline, 0))(close)
        }
        def getSingleLineDecision: Policy.Pf =
          if (newlines > 0) null
          else if (style.activeForEdition_2019_11)
            getSingleLineDecisionFor2019Nov
          else
            getSingleLineDecisionPre2019Nov

        // null if skipping
        val singleLineDecision =
          if (lambdaPolicy == null) getSingleLineDecision
          else null

        val spaceMod = xmlSpace(leftOwner)

        Seq(
          Split(spaceMod, 0, ignoreIf = singleLineDecision == null)
            .withOptimalToken(close, killOnFail = true)
            .withPolicy(SingleLineBlock(close).andThen(singleLineDecision)),
          Split(
            Space,
            0,
            ignoreIf =
              style.newlines.alwaysBeforeCurlyBraceLambdaParams ||
                isSelfAnnotation || lambdaPolicy == null
          ).withOptimalToken(lambdaArrow)
            .withIndent(lambdaIndent, close, Right)
            .withPolicy(lambdaPolicy),
          Split(nl, 1)
            .withPolicy(newlineBeforeClosingCurly)
            .withIndent(2, close, Right)
        )
      case FormatToken(arrow @ T.RightArrow(), right, _)
          if statementStarts.contains(hash(right)) &&
            leftOwner.isInstanceOf[Term.Function] =>
        val endOfFunction = lastToken(
          leftOwner.asInstanceOf[Term.Function].body
        )
        val canBeSpace =
          statementStarts(hash(right)).isInstanceOf[Term.Function]
        val afterCurlyNewlines =
          style.newlines.afterCurlyLambda match {
            case NewlineCurlyLambda.never => Newline
            case NewlineCurlyLambda.always => Newline2x
            case NewlineCurlyLambda.preserve =>
              if (newlines >= 2) Newline2x else Newline
          }
        Seq(
          Split(Space, 0, ignoreIf = !canBeSpace),
          Split(afterCurlyNewlines, 1).withIndent(2, endOfFunction, Left)
        )

      case FormatToken(T.RightArrow(), right, _)
          if leftOwner.is[Term.Function] =>
        val lambda = leftOwner.asInstanceOf[Term.Function]
        val (endOfFunction, expiresOn) = functionExpire(lambda)
        val hasSingleLineComment = isSingleLineComment(right)
        val indent = // don't indent if the body is empty `{ x => }`
          if (isEmptyFunctionBody(leftOwner) && !right.is[T.Comment]) 0
          else 2
        val singleLineSplit =
          Split(Space, 0, ignoreIf = hasSingleLineComment)
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
        right match {
          case _: T.LeftBrace if caseStat.body eq rightOwner =>
            // Redundant {} block around case statements.
            Seq(
              Split(Space, 0)
                .withIndent(-2, rightOwner.tokens.last, Left)
            )
          case _ =>
            Seq(
              Split(Space, 0, ignoreIf = newlines != 0), // Gets killed by `case` policy.
              Split(
                NewlineT(isDouble = false, noIndent = rhsIsCommentedOut(tok)),
                1
              )
            )
        }
      // New statement
      case tok @ FormatToken(T.Semicolon(), right, between)
          if startsStatement(tok) && newlines == 0 =>
        val expire = statementStarts(hash(right)).tokens.last
        Seq(
          Split(Space, 0)
            .withOptimalToken(expire)
            .withPolicy(SingleLineBlock(expire)),
          // For some reason, this newline cannot cost 1.
          Split(NewlineT(shouldGet2xNewlines(tok, style, owners)), 0)
        )

      case tok @ FormatToken(left, right, between) if startsStatement(tok) =>
        val newline: Modification = NewlineT(
          shouldGet2xNewlines(tok, style, owners)
        )
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

        if ((annoRight || annoLeft) && style.optIn.annotationNewlines)
          Seq(Split(newlines2Modification(newlines), 0))
        else {
          val spaceCouldBeOk = annoLeft &&
            newlines == 0 && right.is[Keyword]
          Seq(
            Split(
              // This split needs to have an optimalAt field.
              Space,
              0,
              ignoreIf = !spaceCouldBeOk
            ).withOptimalToken(expire)
              .withPolicy(SingleLineBlock(expire)),
            // For some reason, this newline cannot cost 1.
            Split(newline, 0)
          )
        }

      case FormatToken(_, T.RightBrace(), _) =>
        Seq(
          Split(xmlSpace(rightOwner), 0),
          Split(NewlineT(isDouble = newlines > 1), 0)
        )
      case FormatToken(left @ T.KwPackage(), _, _) if leftOwner.is[Pkg] =>
        Seq(
          Split(Space, 0)
        )
      // Opening [ with no leading space.
      // Opening ( with no leading space.
      case FormatToken(
          T.KwSuper() | T.KwThis() | T.Ident(_) | T.RightBracket() |
          T.RightBrace() | T.RightParen() | T.Underscore(),
          T.LeftParen() | T.LeftBracket(),
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
              if style.spaces.afterSymbolicDefs && isSymbolicName(name.value) && name.parent
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
        val forceNewlineBeforeExtends = Policy({
          case d @ Decision(FormatToken(_, right: T.KwExtends, _), _)
              if owners(right) == leftOwner =>
            d.onlyNewlinesWithoutFallback
        }, expire.end)
        Seq(
          Split(Space, 0)
            .withOptimalToken(expire, killOnFail = true)
            .withPolicy(SingleLineBlock(expire)),
          Split(Space, 1).withPolicy(forceNewlineBeforeExtends)
        )
      // DefDef
      case tok @ FormatToken(T.KwDef(), name @ T.Ident(_), _) =>
        Seq(
          Split(Space, 0)
        )
      case tok @ FormatToken(e @ T.Equals(), right, _)
          if defBody(leftOwner).isDefined =>
        val expire = defBody(leftOwner).get.tokens.last
        val exclude = getExcludeIf(
          expire, {
            case T.RightBrace() => true
            case close @ T.RightParen()
                if opensConfigStyle(tokens(matching(close))) =>
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

        val rhsIsJsNative = isJsNative(right)
        right match {
          case T.LeftBrace() =>
            // The block will take care of indenting by 2.
            Seq(Split(Space, 0))
          case _ =>
            val rhsIsComment = isSingleLineComment(right)
            Seq(
              Split(
                Space,
                0,
                ignoreIf = rhsIsComment || newlines > 0 && !rhsIsJsNative,
                policy =
                  if (!style.newlines.alwaysBeforeMultilineDef) NoPolicy
                  else SingleLineBlock(expire, exclude = exclude)
              ),
              Split(Space, 0, ignoreIf = newlines != 0 || !rhsIsComment)
                .withIndent(2, expire, Left),
              Split(Newline, 1, ignoreIf = rhsIsJsNative)
                .withIndent(2, expire, Left)
            )
        }

      // Parameter opening for one parameter group. This format works
      // on the WHOLE defnSite (via policies)
      case ft @ FormatToken((T.LeftParen() | T.LeftBracket()), _, _)
          if (style.verticalMultiline.atDefnSite || style.verticalMultilineAtDefinitionSite) &&
            isDefnSiteWithParams(leftOwner) =>
        verticalMultiline(leftOwner, ft)(style)

      // Term.Apply and friends
      case FormatToken(T.LeftParen(), _, _)
          if style.optIn.configStyleArguments &&
            !style.newlinesBeforeSingleArgParenLambdaParams &&
            getLambdaAtSingleArgCallSite(leftOwner).isDefined => {
        val lambda = getLambdaAtSingleArgCallSite(leftOwner).get
        val lambdaLeft: Option[Token] =
          matchingOpt(functionExpire(lambda)._1).filter(_.is[T.LeftBrace])

        val arrowFt = tokens(lambda.tokens.find(_.is[T.RightArrow]).get)
        val lambdaIsABlock = lambdaLeft.exists(_ eq arrowFt.right)
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

        val newlinePenalty = 3 + nestedApplies(leftOwner)
        Seq(
          Split(NoSplit, 0, policy = SingleLineBlock(close))
            .withOptimalToken(close),
          Split(NoSplit, 0, policy = spacePolicy)
            .withOptimalToken(lambdaToken),
          Split(Newline, newlinePenalty)
            .withPolicy(newlinePolicy)
            .withIndent(style.continuationIndent.callSite, close, Right)
        )
      }

      case FormatToken(T.LeftParen() | T.LeftBracket(), right, between)
          if style.optIn.configStyleArguments &&
            (isDefnSite(leftOwner) || isCallSite(leftOwner)) &&
            (opensConfigStyle(formatToken) || {
              forceConfigStyle(leftOwner) && !styleMap.forcedBinPack(leftOwner)
            }) =>
        val open = formatToken.left
        val indent = getApplyIndent(leftOwner, isConfigStyle = true)
        val close = matching(open)
        val newlineBeforeClose: Policy.Pf = {
          case Decision(t @ FormatToken(_, `close`, _), splits) =>
            Decision(t, Seq(Split(Newline, 0)))
        }
        val extraIndent: Length =
          if (style.poorMansTrailingCommasInConfigStyle) Num(2)
          else Num(0)
        val isForcedBinPack = styleMap.forcedBinPack.contains(leftOwner)
        val policy =
          if (isForcedBinPack) Policy(newlineBeforeClose, close.end)
          else {
            val oneArgOneLine = OneArgOneLineSplit(
              open,
              noTrailingCommas = style.poorMansTrailingCommasInConfigStyle
            )
            oneArgOneLine.copy(f = oneArgOneLine.f.orElse(newlineBeforeClose))
          }
        Seq(
          Split(Newline, 0, policy = policy)
            .withIndent(indent, close, Right)
            .withIndent(extraIndent, right, Right)
        )

      case FormatToken(open @ (T.LeftParen() | T.LeftBracket()), right, between)
          if style.binPack.unsafeDefnSite && isDefnSite(leftOwner) =>
        val close = matching(open)
        val isBracket = open.is[T.LeftBracket]
        val indent = Num(style.continuationIndent.defnSite)
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
          val bracketMultiplier =
            if (isBracket) Constants.BracketPenalty else 1
          val bracketPenalty = if (isBracket) 1 else 0
          val nestingPenalty = nestedApplies(leftOwner)

          val noSplitPenalizeNewlines = penalizeBrackets(1 + bracketPenalty)
          val noSplitPolicy: Policy = argumentStarts.get(hash(right)) match {
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
            if (right.is[T.Comment]) newlines2Modification(newlines)
            else NoSplit

          Seq(
            Split(noSplitModification, 0 + (nestingPenalty * bracketMultiplier))
              .withPolicy(noSplitPolicy)
              .withIndent(indent, close, Left),
            Split(
              Newline,
              (1 + nestingPenalty * nestingPenalty) * bracketMultiplier,
              ignoreIf = right.is[T.RightParen]
            ).withPolicy(penalizeBrackets(1))
              .withIndent(indent, close, Left)
          )
        }
      case FormatToken(T.LeftParen() | T.LeftBracket(), _, _)
          if style.binPack.unsafeCallSite && isCallSite(leftOwner) =>
        val open = formatToken.left
        val close = matching(open)
        val indent = getApplyIndent(leftOwner)
        val (lhs, args) = getApplyArgs(formatToken, leftOwner)
        val optimal = leftOwner.tokens.find(_.is[T.Comma]).orElse(Some(close))
        val isBracket = open.is[T.LeftBracket]
        // TODO(olafur) DRY. Same logic as in default.
        val exclude =
          if (isBracket)
            insideBlock(formatToken, close, _.isInstanceOf[T.LeftBracket])
          else
            insideBlock(formatToken, close, x => x.isInstanceOf[T.LeftBrace])
        val excludeRanges = exclude.map(parensRange)
        val unindent =
          UnindentAtExclude(exclude, Num(-style.continuationIndent.callSite))
        val unindentPolicy =
          if (args.length == 1) Policy(unindent, close.end)
          else NoPolicy
        def ignoreBlocks(x: FormatToken): Boolean = {
          excludeRanges.exists(_.contains(x.left.end))
        }
        val noSplitPolicy =
          penalizeAllNewlines(close, 3, ignore = ignoreBlocks)
            .andThen(unindent)
        Seq(
          Split(NoSplit, 0)
            .withOptimalToken(optimal)
            .withPolicy(noSplitPolicy)
            .withIndent(indent, close, Left),
          Split(Newline, 2)
            .withPolicy(unindentPolicy)
            .withIndent(4, close, Left)
        )
      case FormatToken(T.LeftParen(), T.RightParen(), _) =>
        Seq(Split(NoSplit, 0))

      // If configured to skip the trailing space after `if` and other keywords, do so.
      case FormatToken(T.KwIf() | T.KwFor() | T.KwWhile(), T.LeftParen(), _)
          if !style.spaces.afterKeywordBeforeParen =>
        Seq(Split(NoSplit, 0))

      case tok @ FormatToken(T.LeftParen() | T.LeftBracket(), right, between)
          if !isSuperfluousParenthesis(formatToken.left, leftOwner) &&
            (!style.binPack.unsafeCallSite && isCallSite(leftOwner)) ||
            (!style.binPack.unsafeDefnSite && isDefnSite(leftOwner)) =>
        val open = tok.left
        val close = matching(open)
        val (lhs, args) = getApplyArgs(formatToken, leftOwner)
        // In long sequence of select/apply, we penalize splitting on
        // parens furthest to the right.
        val lhsPenalty = treeDepth(lhs)

        val isBracket = open.is[T.LeftBracket]
        val bracketMultiplier =
          if (isBracket) Constants.BracketPenalty
          else 1

        val nestedPenalty = nestedApplies(leftOwner)
        val exclude =
          if (isBracket) insideBlock(tok, close, _.is[T.LeftBracket])
          else
            insideBlock(tok, close, x => x.is[T.LeftBrace])
        val excludeRanges = exclude.map(parensRange)

        val indent = getApplyIndent(leftOwner)

        val singleArgument = args.length == 1

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

        val oneArgOneLine = OneArgOneLineSplit(open)

        val newlineModification: Modification =
          if (right.is[T.Comment] && newlines == 0)
            Space
          else if (right.is[T.LeftBrace]) NoSplit
          else Newline

        val defnSite = isDefnSite(leftOwner)
        val closeFormatToken = tokens(close)
        val expirationToken: Token =
          if (defnSite && !isBracket)
            defnSiteLastToken(closeFormatToken, leftOwner)
          else
            rhsOptimalToken(closeFormatToken)

        val tooManyArguments = args.length > 100

        val mustDangle = style.activeForEdition_2020_01 && (
          expirationToken.is[T.Comment]
        )
        val wouldDangle =
          if (defnSite) style.danglingParentheses.defnSite
          else style.danglingParentheses.callSite

        val newlinePolicy: Policy =
          if (wouldDangle || mustDangle) {
            newlinesOnlyBeforeClosePolicy(close)
          } else {
            Policy.empty(close)
          }

        val noSplitIndent =
          if (isSingleLineComment(right)) indent
          else Num(0)

        val isTuple = leftOwner match {
          case _: Type.Tuple => style.align.openParenDefnSite
          case _: Term.Tuple => style.align.openParenCallSite
          case _ => false
        }
        val skipOpenParenAlign = {
          !isTuple && {
            (defnSite && !style.align.openParenDefnSite) ||
            (!defnSite && !style.align.openParenCallSite)
          }
        }

        val noSplitModification: Modification =
          if (formatToken.left.is[T.LeftParen] &&
            style.spaces.inParentheses) Space
          else if (right.is[T.Comment]) newlines2Modification(newlines)
          else NoSplit

        val keepConfigStyleSplit =
          style.optIn.configStyleArguments && newlines != 0
        val splitsForAssign =
          if (defnSite || isBracket || keepConfigStyleSplit) None
          else
            getAssignAtSingleArgCallSite(leftOwner).map { assign =>
              val assignToken = assign.rhs match {
                case b: Term.Block => b.tokens.head
                case _ => assign.tokens.find(_.is[T.Equals]).get
              }
              val assignFT = tokens(assignToken)
              val hasComment = isAttachedSingleLineComment(assignFT)
              val breakToken = if (hasComment) assignFT.right else assignToken
              val newlineAfterAssignDecision =
                if (newlinePolicy.isEmpty) Policy.emptyPf
                else decideNewlinesOnlyAfterToken(breakToken)
              val noSplitCost = 1 + nestedPenalty + lhsPenalty
              val newlineCost = Constants.ExceedColumnPenalty + noSplitCost
              Seq(
                Split(Newline, newlineCost)
                  .withPolicy(newlinePolicy)
                  .withIndent(indent, close, Right),
                Split(NoSplit, noSplitCost)
                  .withOptimalToken(breakToken)
                  .withPolicy(
                    newlinePolicy
                      .andThen(newlineAfterAssignDecision)
                      .andThen(SingleLineBlock(breakToken))
                  )
              )
            }

        val noSplitPolicy =
          if (wouldDangle || mustDangle && isBracket)
            SingleLineBlock(close, exclude = excludeRanges)
          else if (splitsForAssign.isDefined)
            singleLine(3)
          else
            singleLine(10)
        Seq(
          Split(noSplitModification, 0, policy = noSplitPolicy)
            .withOptimalToken(expirationToken)
            .withIndent(noSplitIndent, close, Right),
          Split(
            newlineModification,
            (1 + nestedPenalty + lhsPenalty) * bracketMultiplier,
            policy = newlinePolicy.andThen(singleLine(4)),
            ignoreIf = args.length > 1 || isTuple || splitsForAssign.isDefined
          ).withOptimalToken(expirationToken)
            .withIndent(indent, close, Right),
          Split(
            noSplitModification,
            (2 + lhsPenalty) * bracketMultiplier,
            policy = oneArgOneLine,
            ignoreIf =
              singleArgument || tooManyArguments ||
                skipOpenParenAlign
          ).withOptimalToken(expirationToken)
            .withIndent(StateColumn, close, Right),
          Split(
            Newline,
            (3 + nestedPenalty + lhsPenalty) * bracketMultiplier,
            policy = newlinePolicy.andThen(oneArgOneLine),
            ignoreIf = singleArgument || isTuple
          ).withOptimalToken(expirationToken)
            .withIndent(indent, close, Right)
        ) ++ splitsForAssign.getOrElse(Seq.empty)

      // Closing def site ): ReturnType
      case FormatToken(left, T.Colon(), _)
          if style.newlines.sometimesBeforeColonInMethodReturnType &&
            defDefReturnType(leftOwner).isDefined =>
        val expire = lastToken(defDefReturnType(rightOwner).get)
        val penalizeNewlines =
          penalizeAllNewlines(expire, Constants.BracketPenalty)
        val sameLineSplit = if (endsWithSymbolIdent(left)) Space else NoSplit
        Seq(
          Split(sameLineSplit, 0).withPolicy(penalizeNewlines),
          // Spark style guide allows this:
          // https://github.com/databricks/scala-style-guide#indent
          Split(Newline, Constants.SparkColonNewline)
            .withIndent(style.continuationIndent.defnSite, expire, Left)
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
      case FormatToken(left, open @ T.LeftBrace(), _) =>
        val close = matching(open)
        val isComma = left.is[T.Comma]
        val bodyHasNewlines = if (isComma) {
          open.pos.endLine != close.pos.startLine
        } else {
          true
        }
        Seq(
          Split(Space, 0),
          Split(
            Newline,
            0,
            ignoreIf = !isComma || newlines == 0 || bodyHasNewlines
          ).withOptimalToken(close, killOnFail = true)
            .withPolicy(SingleLineBlock(close))
        )

      // Delim
      case FormatToken(_, T.Comma(), _) =>
        Seq(
          Split(NoSplit, 0)
        )
      // These are mostly filtered out/modified by policies.
      case tok @ FormatToken(T.Comma(), right, _) =>
        // TODO(olafur) DRY, see OneArgOneLine.
        val binPack = isBinPack(leftOwner)
        val isInfix = leftOwner.isInstanceOf[Term.ApplyInfix]
        argumentStarts.get(hash(right)) match {
          case Some(nextArg) if binPack =>
            val lastFT = tokens(nextArg.tokens.last)
            val nextComma: Option[FormatToken] = tokens(lastFT, 1) match {
              case t @ FormatToken(left @ T.Comma(), _, _)
                  if owners(left) == leftOwner =>
                Some(t)
              case _ => None
            }
            val singleLine = SingleLineBlock(lastFT.left)
            val breakOnNextComma = nextComma match {
              case Some(comma) =>
                Policy({
                  case d @ Decision(t, s) if comma == t =>
                    d.forceNewline
                }, comma.right.end)
              case _ => NoPolicy
            }
            val optToken = nextComma.map(_ =>
              OptimalToken(
                rhsOptimalToken(lastFT),
                killOnFail = true
              )
            )
            Seq(
              Split(Space, 0, optimalAt = optToken).withPolicy(singleLine),
              Split(Newline, 1, optimalAt = optToken).withPolicy(singleLine),
              // next argument doesn't fit on a single line, break on comma before
              // and comma after.
              Split(Newline, 2, optimalAt = optToken)
                .withPolicy(breakOnNextComma)
            )
          case _ if isInfix =>
            Seq(
              // Do whatever the user did if infix.
              Split(
                if (newlines > 0) Newline
                else Space,
                0
              )
            )
          case _ =>
            val indent = leftOwner match {
              case _: Defn.Val | _: Defn.Var =>
                style.continuationIndent.defnSite
              case _ =>
                0
            }
            val singleLineComment = isSingleLineComment(right)
            val noNewline = newlines == 0 && {
              singleLineComment || style.activeForEdition_2020_01 && {
                val nextTok = nextNonComment(tok).right
                // perhaps a trailing comma
                (nextTok ne right) && nextTok.is[CloseParenOrBracket]
              }
            }
            Seq(
              Split(Space, 0, ignoreIf = newlines != 0 && singleLineComment),
              Split(Newline, 1, ignoreIf = noNewline)
                .withIndent(indent, right, ExpiresOn.Right)
            )
        }
      case FormatToken(_, T.Semicolon(), _) =>
        Seq(
          Split(NoSplit, 0)
        )
      case FormatToken(T.KwReturn(), _, _) =>
        val mod = leftOwner match {
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
            val summaryTypeBoundsCount = tp.tbounds.lo.size + tp.tbounds.hi.size + tp.cbounds.size
            if (contextOption.isIfMultipleBounds && summaryTypeBoundsCount > 1 || contextOption.isAlways)
              Space
            else NoSplit

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
      case FormatToken(tok @ T.Equals(), right, between) if (leftOwner match {
            case _: Defn.Type | _: Defn.Val | _: Defn.Var | _: Term.Assign |
                _: Term.Assign | _: Term.Assign =>
              true
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

        def wouldDangle = {
          val dangleStyle = style.danglingParentheses
          (dangleStyle.defnSite && leftOwner.parent.exists(isDefnSite)) ||
          (dangleStyle.callSite && leftOwner.parent.exists(isCallSite))
        }

        val expire = rhs.tokens.last
        // rhsOptimalToken is too aggressive here
        val optimal = tokens(expire).right match {
          case x: T.Comma => x
          case x @ CloseParenOrBracket() if !wouldDangle => x
          case _ => expire
        }

        val penalty = leftOwner match {
          case l: Term.Assign if style.binPack.unsafeCallSite =>
            Constants.BinPackAssignmentPenalty
          case l: Term.Param if style.binPack.unsafeDefnSite =>
            Constants.BinPackAssignmentPenalty
          case _ => 0
        }

        val mod: Modification =
          if (isAttachedSingleLineComment(formatToken)) Space
          else Newline

        val exclude =
          insideBlock(formatToken, expire, _.isInstanceOf[T.LeftBrace])
        rhs match {
          case t: Term.ApplyInfix =>
            infixSplit(t, formatToken)
          case _ =>
            def twoBranches: Policy = {
              val excludeRanges = exclude.map(parensRange)
              penalizeAllNewlines(
                expire,
                Constants.ShouldBeSingleLine,
                ignore = x => excludeRanges.exists(_.contains(x.left.start))
              )
            }
            val jsNative = isJsNative(right)
            val noNewline = jsNative
            val spacePolicy: Policy = rhs match {
              case _: Term.If => twoBranches
              case _: Term.ForYield => twoBranches
              case _: Term.Try | _: Term.TryWithHandler
                  if style.activeForEdition_2019_11 && !noNewline =>
                null // we force newlines in try/catch/finally
              case _ => NoPolicy
            }
            val noSpace = null == spacePolicy ||
              (!jsNative && newlines > 0 && leftOwner.isInstanceOf[Defn])
            val spaceIndent = if (isSingleLineComment(right)) 2 else 0
            Seq(
              Split(Space, 0, policy = spacePolicy, ignoreIf = noSpace)
                .withOptimalToken(optimal, killOnFail = false)
                .withIndent(spaceIndent, expire, Left),
              Split(mod, 1 + penalty, ignoreIf = noNewline)
                .withIndent(2, expire, Left)
            )
        }
      case tok @ FormatToken(left, dot @ T.Dot() `:chain:` chain, _)
          if !left.is[T.Underscore] =>
        val nestedPenalty = nestedSelect(rightOwner) + nestedApplies(leftOwner)
        val optimalToken = chainOptimalToken(chain)
        val expire =
          if (chain.length == 1) lastToken(chain.last)
          else optimalToken

        val breakOnEveryDot = Policy(
          {
            case Decision(t @ FormatToken(_, dot2 @ T.Dot(), _), s)
                if chain.contains(owners(dot2)) =>
              val mod =
                if (style.optIn.breaksInsideChains && t.newlinesBetween == 0)
                  NoSplit
                else Newline
              Decision(t, Seq(Split(mod, 1)))
          },
          expire.end
        )
        val exclude = getExcludeIf(expire)
        // This policy will apply to both the space and newline splits, otherwise
        // the newline is too cheap even it doesn't actually prevent other newlines.
        val penalizeNewlinesInApply = penalizeAllNewlines(expire, 2)
        val noSplitPolicy = SingleLineBlock(expire, exclude)
          .andThen(penalizeNewlinesInApply.f)
          .copy(expire = expire.end)
        val newlinePolicy = breakOnEveryDot
          .andThen(penalizeNewlinesInApply.f)
          .copy(expire = expire.end)
        val ignoreNoSplit = style.optIn.breakChainOnFirstMethodDot && newlines > 0
        val chainLengthPenalty =
          if (style.newlines.penalizeSingleSelectMultiArgList &&
            chain.length < 2) {
            // penalize by the number of arguments in the rhs open apply.
            // I know, it's a bit arbitrary, but my manual experiments seem
            // to show that it produces OK output. The key insight is that
            // many arguments on the same line can be hard to read. By not
            // putting a newline before the dot, we force the argument list
            // to break into multiple lines.
            splitApplyIntoLhsAndArgsLifted(owners(tokens(tok, 2).right))
              .map {
                case (_, lst) => Math.max(0, lst.length - 1)
              }
              .getOrElse(0)
          } else 0
        if (TokenOps.isSymbolicIdent(left))
          Seq(Split(NoSplit, 0))
        else
          Seq(
            Split(NoSplit, 0, ignoreIf = ignoreNoSplit)
              .withPolicy(noSplitPolicy),
            Split(
              Newline.copy(acceptNoSplit = true),
              2 + nestedPenalty + chainLengthPenalty
            ).withPolicy(newlinePolicy)
              .withIndent(2, optimalToken, Left)
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
          Split(if (isSymbolicIdent(right)) Space else NoSplit, 0)
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
        rightOwner match {
          // something like new A with B with C
          case template: Template if template.parent.exists { p =>
                p.is[Term.New] || p.is[Term.NewAnonymous]
              } =>
            val isFirstWith = template.inits.headOption.exists { init =>
              // [init.tpe == leftOwner] part is about expressions like [new A with B]
              // [leftOwner.is[Init] && init == leftOwner] part is about expressions like [new A(x) with B]
              leftOwner.is[Init] && init == leftOwner || init.tpe == leftOwner
            }
            splitWithChain(
              isFirstWith,
              Set(template),
              templateCurly(template).getOrElse(template.tokens.last)
            )

          case template: Template =>
            val hasSelfAnnotation = template.self.tokens.nonEmpty
            val expire = templateCurly(rightOwner)
            val policy =
              if (hasSelfAnnotation) NoPolicy
              else {
                Policy(
                  {
                    // Force template to be multiline.
                    case d @ Decision(
                          FormatToken(open @ T.LeftBrace(), right, _),
                          splits
                        )
                        if !hasSelfAnnotation &&
                          !right.is[T.RightBrace] && // corner case, body is {}
                          childOf(template, owners(open)) =>
                      d.forceNewline
                  },
                  expire.end
                )
              }
            Seq(
              Split(Space, 0),
              Split(Newline, 1).withPolicy(policy)
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
        Seq(
          Split(NoSplit, 0)
            .withIndent(indent, close, Left)
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
            Policy({
              case d @ Decision(FormatToken(_, r: T.KwElse, _), _)
                  if elses.contains(r) =>
                d.onlyNewlinesWithFallback(Split(Newline, 0))
            }, expire.end)
        Seq(
          Split(Space, 0)
            .withOptimalToken(expire, killOnFail = true)
            .withPolicy(SingleLineBlock(expire)),
          Split(Space, 1).withPolicy(breakOnlyBeforeElse)
        )
      case FormatToken(close: T.RightParen, _, _) if (leftOwner match {
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
        // Inline comment attached to closing RightParen
        val attachedComment = isAttachedSingleLineComment(formatToken)
        val newlineModification: Modification =
          if (attachedComment)
            Space // Inline comment will force newline later.
          else Newline
        val exclude =
          insideBlock(formatToken, expire, _.is[T.LeftBrace]).map(parensRange)
        Seq(
          Split(Space, 0, ignoreIf = attachedComment || newlines > 0)
            .withPolicy(SingleLineBlock(expire, exclude = exclude)),
          Split(newlineModification, 1).withIndent(2, expire, Left)
        )
      case FormatToken(T.RightBrace(), T.KwElse(), _) =>
        val nlOnly = style.newlines.alwaysBeforeElseAfterCurlyIf ||
          !leftOwner.is[Term.Block] || !leftOwner.parent.forall(_ == rightOwner)
        Seq(
          Split(if (nlOnly) Newline else Space, 0)
        )

      case FormatToken(T.RightBrace(), T.KwYield(), _) =>
        Seq(
          Split(Space, 0)
        )
      case FormatToken(_, T.KwElse() | T.KwYield(), _) =>
        val expire = rhsOptimalToken(tokens(rightOwner.tokens.last))
        val exclude =
          insideBlock(formatToken, expire, _.is[T.LeftBrace]).map(parensRange)
        Seq(
          Split(Space, 0, ignoreIf = newlines > 0)
            .withPolicy(SingleLineBlock(expire, exclude = exclude)),
          Split(Newline, 1)
        )
      // Last else branch
      case FormatToken(_: T.KwElse, _, _) if (leftOwner match {
            case t: Term.If => !t.elsep.is[Term.If]
            case x => throw new UnexpectedTree[Term.If](x)
          }) =>
        val expire = leftOwner.asInstanceOf[Term.If].elsep.tokens.last
        Seq(
          Split(Space, 0, ignoreIf = newlines != 0)
            .withPolicy(SingleLineBlock(expire)),
          Split(Newline, 1).withIndent(2, expire, Left)
        )

      // Type variance
      case tok @ FormatToken(T.Ident(_), T.Ident(_) | T.Underscore(), _)
          if isTypeVariant(leftOwner) =>
        Seq(
          Split(NoSplit, 0)
        )

      // Var args
      case FormatToken(_, T.Ident("*"), _) if rightOwner.is[Type.Repeated] =>
        Seq(
          Split(NoSplit, 0)
        )

      case FormatToken(open @ T.LeftParen(), right, _) =>
        val owner = owners(open)
        val isConfig = opensConfigStyle(formatToken)
        val isSuperfluous = isSuperfluousParenthesis(open, owner)
        val close = matching(open)
        val breakOnClose = Policy({
          case Decision(t @ FormatToken(_, `close`, _), s) =>
            Decision(t, Seq(Split(Newline, 0)))
        }, close.end)
        val indent: Length = right match {
          case T.KwIf() => StateColumn
          case T.KwFor() if !style.indentYieldKeyword => StateColumn
          case _ => Num(0)
        }
        Seq(
          Split(Newline, 0, ignoreIf = !isConfig)
            .withPolicy(breakOnClose)
            .withIndent(style.continuationIndent.callSite, close, Right),
          Split(NoSplit, 0, ignoreIf = isConfig)
            .withIndent(indent, close, Left)
            .withPolicy(penalizeAllNewlines(close, 1))
        )
      // Infix operator.
      case tok @ FormatToken(op @ T.Ident(_), right, between)
          if isApplyInfix(op, leftOwner) =>
        // TODO(olafur) move extractor into pattern match.
        val InfixApplication(_, op, args) = leftOwner.parent.get
        infixSplit(leftOwner, op, args, formatToken)
      case FormatToken(left, op @ T.Ident(_), between)
          if isApplyInfix(op, rightOwner) =>
        val InfixApplication(_, op, args) = rightOwner.parent.get
        infixSplit(rightOwner, op, args, formatToken)
      case opt
          if style.optIn.annotationNewlines &&
            optionalNewlines(hash(opt.right)) =>
        Seq(Split(newlines2Modification(newlines), 0))
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

      // Case
      case tok @ FormatToken(cs @ T.KwCase(), _, _) if leftOwner.is[Case] =>
        val owner = leftOwner.asInstanceOf[Case]
        val arrow = getArrow(owner)
        // TODO(olafur) expire on token.end to avoid this bug.
        val expire = Option(owner.body)
          .filter(_.tokens.exists(!_.is[Trivia]))
          // edge case, if body is empty expire on arrow
          .fold(arrow)(t => getOptimalTokenFor(lastToken(t)))

        Seq(
          // Either everything fits in one line or break on =>
          Split(Space, 0)
            .withOptimalToken(expire, killOnFail = true)
            .withPolicy(SingleLineBlock(expire)),
          Split(Space, 1)
            .withPolicy(
              Policy(
                {
                  case d @ Decision(t @ FormatToken(`arrow`, right, _), _)
                      // TODO(olafur) any other corner cases?
                      if !right.isInstanceOf[T.LeftBrace] &&
                        !isAttachedSingleLineComment(t) =>
                    d.onlyNewlinesWithoutFallback
                },
                expire = expire.end
              )
            )
            .withIndent(2, expire, Left) // case body indented by 2.
            .withIndent(2, arrow, Left) // cond body indented by 4.
        )
      case tok @ FormatToken(_, cond @ T.KwIf(), _) if rightOwner.is[Case] =>
        val arrow = getArrow(rightOwner.asInstanceOf[Case])
        val exclude =
          insideBlock(tok, arrow, _.is[T.LeftBrace]).map(parensRange)
        val singleLine = SingleLineBlock(arrow, exclude = exclude)

        Seq(
          Split(Space, 0, policy = singleLine),
          Split(Newline, 1).withPolicy(penalizeNewlineByNesting(cond, arrow))
        )
      // Inline comment
      case FormatToken(_, c: T.Comment, _) =>
        Seq(Split(newlines2Modification(newlines), 0))
      // Commented out code should stay to the left
      case FormatToken(c: T.Comment, _, _) if isSingleLineComment(c) =>
        Seq(Split(Newline, 0))
      case FormatToken(c: T.Comment, _, _) =>
        Seq(Split(newlines2Modification(newlines), 0))

      // Term.ForYield
      case tok @ FormatToken(_, arrow @ T.KwIf(), _)
          if rightOwner.is[Enumerator.Guard] =>
        Seq(
          // Either everything fits in one line or break on =>
          Split(Space, 0, ignoreIf = newlines > 0),
          Split(Newline, 1)
        )
      case tok @ FormatToken(arrow @ T.LeftArrow(), _, _)
          if leftOwner.is[Enumerator.Generator] =>
        val lastToken = leftOwner.tokens.last
        val indent: Length =
          if (style.align.arrowEnumeratorGenerator) StateColumn
          else Num(0)
        Seq(
          // Either everything fits in one line or break on =>
          Split(Space, 0).withIndent(indent, lastToken, Left)
        )
      case FormatToken(T.KwYield(), _, _) if leftOwner.is[Term.ForYield] =>
        if (style.newlines.avoidAfterYield && !rightOwner.is[Term.If]) {
          Seq(Split(Space, 0))
        } else {
          val lastToken = leftOwner.asInstanceOf[Term.ForYield].body.tokens.last
          Seq(
            // Either everything fits in one line or break on =>
            Split(Space, 0).withPolicy(SingleLineBlock(lastToken)),
            Split(Newline, 1).withIndent(2, lastToken, Left)
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
          Split(if (endsWithSymbolIdent(left)) Space else NoSplit, 0)
        )
      case FormatToken(T.Hash(), ident: T.Ident, _) =>
        val mod = if (TokenOps.isSymbolicIdent(ident)) Space else NoSplit
        Seq(
          Split(mod, 0)
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
          if (style.spaces.inParentheses &&
            isDefnOrCallSite(rightOwner)) Space
          else NoSplit
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
        val mod = if (!style.spaces.inByNameTypes) NoSplit else Space
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

  // TODO(olafur) replace cache with array of seq[split]
  private val cache = mutable.Map.empty[FormatToken, Seq[Split]]

  /**
    * Assigns possible splits to a FormatToken.
    *
    * The FormatToken can be considered as a node in a graph and the
    * splits as edges. Given a format token (a node in the graph), Route
    * determines which edges lead out from the format token.
    */
  def getSplitsMemo(formatToken: FormatToken): Seq[Split] =
    cache.getOrElseUpdate(
      formatToken, {
        val splits =
          getSplits(formatToken).filter(!_.ignoreIf).map(_.adapt(formatToken))
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
              if (x.modification.isNewline) x.copy(modification = Space)
              else x
            )
          case _ => splits
        }
      }
    )

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
}
