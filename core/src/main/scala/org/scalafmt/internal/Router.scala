package org.scalafmt.internal

import scala.language.implicitConversions

import org.scalafmt.internal.ExpiresOn.Right
import org.scalafmt.internal.ExpiresOn.Left
import org.scalafmt.internal.Length.StateColumn
import org.scalafmt.internal.Length.Num
import org.scalafmt.Error.UnexpectedTree
import org.scalafmt.internal.Policy.NoPolicy
import org.scalafmt.util.LoggerOps
import org.scalafmt.util.TokenOps
import org.scalafmt.util.TreeOps
import scala.collection.mutable
import scala.meta.Tree
import scala.meta.Case
import scala.meta.Defn
import scala.meta.Enumerator
import scala.meta.Import
import scala.meta.Name
import scala.meta.Pat
import scala.meta.Pkg
import scala.meta.Template
import scala.meta.Term
import scala.meta.Type
import scala.meta.tokens.Token

// Too many to import individually.
import scala.meta.tokens.Token._

object Constants {
  val BinPackAssignmentPenalty = 10
  val SparkColonNewline = 10
  val BracketPenalty = 20
  val ExceedColumnPenalty = 1000
  // Breaking a line like s"aaaaaaa${111111 + 22222}" should be last resort.
  val BreakSingleLineInterpolatedString = 10 * ExceedColumnPenalty
}

/**
  * Assigns splits to format tokens.
  */
class Router(formatOps: FormatOps) {
  import LoggerOps._
  import TokenOps._
  import TreeOps._
  import formatOps._
  import Constants._

  private def getSplits(formatToken: FormatToken): Seq[Split] = {
    val leftOwner = owners(formatToken.left)
    val rightOwner = owners(formatToken.right)
    val newlines = newlinesBetween(formatToken.between)
    formatToken match {
      case FormatToken(_: BOF, _, _) =>
        Seq(
            Split(NoSplit, 0)
        )
      case FormatToken(_, _: EOF, _) =>
        Seq(
            Split(Newline, 0) // End files with trailing newline
        )
      case FormatToken(start: Interpolation.Start, _, _) =>
        val isStripMargin = isMarginizedString(start)
        val end = matchingParentheses(hash(start))
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
      case FormatToken(_: Interpolation.Id | _: Interpolation.Part |
                       _: Interpolation.Start | _: Interpolation.SpliceStart,
                       _,
                       _) =>
        Seq(
            Split(NoSplit, 0)
        )
      case FormatToken(_,
                       _: Interpolation.Part | _: Interpolation.End |
                       _: Interpolation.SpliceEnd,
                       _) =>
        Seq(
            Split(NoSplit, 0)
        )
      case FormatToken(_: `{`, _: `}`, _) =>
        Seq(
            Split(NoSplit, 0)
        )
      // Import
      case FormatToken(_: `.`, open: `{`, _)
          if parents(rightOwner).exists(_.isInstanceOf[Import]) =>
        Seq(
            Split(NoSplit, 0)
        )
      case FormatToken(open: `{`, _, _)
          if parents(leftOwner).exists(_.isInstanceOf[Import]) ||
          leftOwner.isInstanceOf[Term.Interpolate] =>
        val policy =
          if (leftOwner.isInstanceOf[Term.Interpolate]) NoPolicy
          else SingleLineBlock(matchingParentheses(hash(open)))
        Seq(
            Split(if (style.spacesInImportCurlyBrackets) Space else NoSplit, 0)
              .withPolicy(policy)
        )
      case FormatToken(_, close: `}`, _)
          if parents(rightOwner).exists(_.isInstanceOf[Import]) ||
          rightOwner.isInstanceOf[Term.Interpolate] =>
        Seq(
            Split(if (style.spacesInImportCurlyBrackets) Space else NoSplit, 0)
        )
      case FormatToken(_: `.`, underscore: `_ `, _)
          if parents(rightOwner).exists(_.isInstanceOf[Import]) =>
        Seq(
            Split(NoSplit, 0)
        )

      // { ... } Blocks
      case tok @ FormatToken(open: `{`, right, between) =>
        val nl = Newline(shouldGet2xNewlines(tok))
        val close = matchingParentheses(hash(open))
        val blockSize = close.start - open.end
        val ignore = blockSize > style.maxColumn || isInlineComment(right)
        val newlineBeforeClosingCurly = Policy({
          case Decision(t @ FormatToken(_, `close`, _), s) =>
            Decision(t, Seq(Split(Newline, 0)))
        }, close.end)

        val (startsLambda, lambdaPolicy, lambdaArrow, lambdaIndent) =
          statementStarts
            .get(hash(right))
            .collect {
              case owner: Term.Function =>
                val arrow = lastLambda(owner).tokens.find(_.isInstanceOf[`=>`])
                val singleLineUntilArrow =
                  newlineBeforeClosingCurly.andThen(SingleLineBlock(
                          arrow.getOrElse(owner.params.last.tokens.last)).f)
                (true, singleLineUntilArrow, arrow, 0)
            }
            .getOrElse {
              leftOwner match {
                // Self type: trait foo { self => ... }
                case t: Template
                    if !t.self.name.isInstanceOf[Name.Anonymous] =>
                  val arrow = t.tokens.find(_.isInstanceOf[`=>`])
                  val singleLineUntilArrow = newlineBeforeClosingCurly.andThen(
                      SingleLineBlock(arrow.getOrElse(t.self.tokens.last)).f)
                  (true, singleLineUntilArrow, arrow, 2)
                case _ => (false, NoPolicy, None, 0)
              }
            }

        val skipSingleLineBlock =
          ignore || startsLambda || newlinesBetween(between) > 0

        Seq(
            Split(Space, 0, ignoreIf = skipSingleLineBlock)
              .withOptimalToken(close, killOnFail = true)
              .withPolicy(SingleLineBlock(close)),
            Split(Space, 0, ignoreIf = !startsLambda)
              .withOptimalToken(lambdaArrow)
              .withIndent(lambdaIndent, close, Right)
              .withPolicy(lambdaPolicy),
            Split(nl, 1)
              .withPolicy(newlineBeforeClosingCurly)
              .withIndent(2, close, Right)
        )

      // Term.Function
      case FormatToken(open: `(`, _, _)
          // Argument list for anonymous function
          if !style.binPackParameters && (leftOwner match {
                case _: Term.Function | _: Type.Function => true
                case _ => false
              }) =>
        val close = matchingParentheses(hash(open))
        Seq(
            Split(NoSplit, 0).withIndent(StateColumn, close, Left)
        )
      case FormatToken(arrow: `=>`, right, _)
          if statementStarts.contains(hash(right)) &&
          leftOwner.isInstanceOf[Term.Function] =>
        val endOfFunction = leftOwner.tokens.last
        val canBeSpace =
          statementStarts(hash(right)).isInstanceOf[Term.Function]
        Seq(
            Split(Space, 0, ignoreIf = !canBeSpace),
            Split(Newline, 1).withIndent(2, endOfFunction, Left)
        )
      case FormatToken(arrow: `=>`, right, _)
          if leftOwner.isInstanceOf[Term.Function] =>
        val endOfFunction = functionExpire(
            leftOwner.asInstanceOf[Term.Function])
        val hasBlock = nextNonComment(formatToken).right.isInstanceOf[`{`]
        Seq(
            Split(Space, 0, ignoreIf = isInlineComment(right))
              .withPolicy(SingleLineBlock(endOfFunction)),
            Split(Space, 0, ignoreIf = !hasBlock),
            Split(Newline, 1 + nestedApplies(leftOwner), ignoreIf = hasBlock)
              .withIndent(2, endOfFunction, Right)
        )
      // Case arrow
      case tok @ FormatToken(arrow: `=>`, right, between)
          if leftOwner.isInstanceOf[Case] =>
        Seq(
            Split(Space, 0, ignoreIf = newlines != 0), // Gets killed by `case` policy.
            Split(Newline(isDouble = false, noIndent = rhsIsCommentedOut(tok)),
                  1)
        )
      // New statement
      case tok @ FormatToken(_: `;`, right, between)
          if startsStatement(tok) && newlines == 0 =>
        val expire = statementStarts(hash(right)).tokens.last
        Seq(
            Split(Space, 0)
              .withOptimalToken(expire)
              .withPolicy(SingleLineBlock(expire)),
            // For some reason, this newline cannot cost 1.
            Split(Newline(shouldGet2xNewlines(tok)), 0)
        )

      case tok @ FormatToken(left, right, between) if startsStatement(tok) =>
        val oldNewlines = newlinesBetween(between)
        val newline: Modification = Newline(shouldGet2xNewlines(tok))
        val expire = rightOwner.tokens
          .find(_.isInstanceOf[`=`])
          .map { equalsToken =>
            val equalsFormatToken = leftTok2tok(equalsToken)
            if (equalsFormatToken.right.isInstanceOf[`{`]) {
              equalsFormatToken.right
            } else {
              equalsToken
            }
          }
          .getOrElse(rightOwner.tokens.last)

        val spaceCouldBeOk =
          oldNewlines == 0 && !left.isInstanceOf[Comment] &&
          right.isInstanceOf[Keyword] &&
          isSingleIdentifierAnnotation(prev(tok))
        Seq(
            Split(
                  // This split needs to have an optimalAt field.
                  Space,
                  0,
                  ignoreIf = !spaceCouldBeOk)
              .withOptimalToken(expire)
              .withPolicy(SingleLineBlock(expire)),
            // For some reason, this newline cannot cost 1.
            Split(newline, 0)
        )
      case FormatToken(_: `(`, _: `{`, between) =>
        Seq(
            Split(NoSplit, 0)
        )

      // non-statement starting curly brace
      case FormatToken(_, _: `{`, between) =>
        Seq(
            Split(Space, 0)
        )

      case FormatToken(_, _: `}`, _) =>
        Seq(
            Split(Space, 0),
            Split(Newline, 0)
        )
      case FormatToken(left: `package `, _, _)
          if leftOwner.isInstanceOf[Pkg] =>
        Seq(
            Split(Space, 0)
        )
      // Opening [ with no leading space.
      // Opening ( with no leading space.
      case FormatToken(
          _: `this` | _: Ident | _: `]` | _: `}` | _: `)`, _: `(` | _: `[`, _)
          if noSpaceBeforeOpeningParen(rightOwner) && {
            leftOwner.parent.forall {
              // infix applications have no space.
              case _: Type.ApplyInfix | _: Term.ApplyInfix => false
              case parent => true
            }
          } =>
        Seq(
            Split(NoSplit, 0)
        )
      // Defn.{Object, Class, Trait}
      case tok @ FormatToken(_: `object` | _: `class ` | _: `trait`, _, _) =>
        val expire = defnTemplate(leftOwner)
          .flatMap(templateCurly)
          .getOrElse(leftOwner.tokens.last)
        val forceNewlineBeforeExtends = Policy({
          case Decision(t @ FormatToken(_, right: `extends`, _), s)
              if owners(right) == leftOwner =>
            Decision(t, s.filter(_.modification.isNewline))
        }, expire.end)
        Seq(
            Split(Space, 0)
              .withOptimalToken(expire, killOnFail = true)
              .withPolicy(SingleLineBlock(expire)),
            Split(Space, 1).withPolicy(forceNewlineBeforeExtends)
        )
      // DefDef
      case tok @ FormatToken(_: `def`, name: Ident, _) =>
        Seq(
            Split(Space, 0)
        )
      case tok @ FormatToken(e: `=`, right, _)
          if defBody(leftOwner).isDefined =>
        val expire = defBody(leftOwner).get.tokens.last
        val exclude = getExcludeIf(expire, {
          case _: `}` => true
          case close: `)`
              if opensConfigStyle(
                  leftTok2tok(matchingParentheses(hash(close)))) =>
            // Example:
            // def x = foo(
            //     1
            // )
            true
          case _ => false
        })

        val rhsIsJsNative = isJsNative(right)
        Seq(
            Split(Space,
                  0,
                  ignoreIf = newlines > 0 && !rhsIsJsNative,
                  policy = SingleLineBlock(expire, exclude = exclude)),
            Split(Newline, 1, ignoreIf = rhsIsJsNative)
              .withIndent(2, expire, Left)
        )
      // Term.Apply and friends
      case FormatToken(_: `(` | _: `[`, _, between)
          if style.configStyleArguments &&
          (isDefnSite(leftOwner) || isCallSite(leftOwner)) &&
          opensConfigStyle(formatToken) =>
        val open = formatToken.left.asInstanceOf[Delim]
        val indent = getApplyIndent(leftOwner, isConfigStyle = true)
        val close = matchingParentheses(hash(open))
        val oneArgOneLine = OneArgOneLineSplit(open)
        val configStyle = oneArgOneLine.copy(f = oneArgOneLine.f.orElse {
          case Decision(t @ FormatToken(_, `close`, _), splits) =>
            Decision(t, Seq(Split(Newline, 0)))
        })
        Seq(
            Split(Newline, 0, policy = configStyle)
              .withIndent(indent, close, Right)
        )
      case FormatToken(open: `(`, right, _)
        if style.binPackParameters && isDefnSite(leftOwner) =>
        val close = matchingParentheses(hash(open))
        val indent = Num(style.continuationIndentDefnSite)
        val nextArg: Policy = argumentStarts.get(hash(right)) match {
          case Some(arg) => penalizeAllNewlines(arg.tokens.last, 3)
          case _ => NoPolicy
        }

        Seq(
          Split(NoSplit, 0).withIndent(indent, close, Left),
          Split(Newline, 1, ignoreIf = right.isInstanceOf[`)`])
              .withIndent(indent, close, Left)
        )
      case FormatToken(_: `(` | _: `[`, _, _)
          if style.binPackArguments && isCallSite(leftOwner) =>
        val open = formatToken.left
        val close = matchingParentheses(hash(open))
        val optimal =
          leftOwner.tokens.find(_.isInstanceOf[`,`]).orElse(Some(close))
        Seq(
            Split(NoSplit, 0)
              .withOptimalToken(optimal)
              .withPolicy(penalizeAllNewlines(close, 3))
              .withIndent(4, close, Left),
            Split(Newline, 2).withIndent(4, close, Left)
        )
      case tok @ FormatToken(_: `(` | _: `[`, right, between)
          if !isSuperfluousParenthesis(formatToken.left, leftOwner) &&
          (!style.binPackArguments && isCallSite(leftOwner)) ||
          (!style.binPackParameters && isDefnSite(leftOwner)) =>
        val open = tok.left.asInstanceOf[Delim]
        val close = matchingParentheses(hash(open))
        val (lhs, args) = getApplyArgs(formatToken, leftOwner)
        // In long sequence of select/apply, we penalize splitting on
        // parens furthest to the right.
        val lhsPenalty = treeDepth(lhs)

        val isBracket = open.isInstanceOf[`[`]
        val bracketMultiplier =
          if (isBracket) Constants.BracketPenalty
          else 1

        val nestedPenalty = nestedApplies(leftOwner)
        val exclude =
          if (isBracket) insideBlock(tok, close, _.isInstanceOf[`[`])
          else insideBlock(tok, close, x => x.isInstanceOf[`{`])
        val excludeRanges = exclude.map(parensRange)

        val indent = getApplyIndent(leftOwner)

        // It seems acceptable to unindent by the continuation indent inside
        // curly brace wrapped blocks.
        val unindent = UnindentAtExclude(exclude, Num(-4))
        val singleArgument = args.length == 1

        def singleLine(newlinePenalty: Int): Policy = {
          val baseSingleLinePolicy =
            if (isBracket) {
              if (singleArgument)
                penalizeAllNewlines(
                    close, newlinePenalty, penalizeLambdas = false)
              else SingleLineBlock(close)
            } else {
              if (singleArgument) {
                penalizeAllNewlines(
                    close, newlinePenalty, penalizeLambdas = false)
              } else SingleLineBlock(close, excludeRanges)
            }

          if (exclude.isEmpty || isBracket) baseSingleLinePolicy
          else baseSingleLinePolicy.andThen(unindent)
        }

        val oneArgOneLine = OneArgOneLineSplit(open)

        val modification =
          if (right.isInstanceOf[Comment]) newlines2Modification(between)
          else NoSplit

        val newlineModification: Modification =
          if (right.isInstanceOf[Comment] && newlinesBetween(between) == 0)
            Space
          else Newline

        val charactersInside = (close.start - open.end) - 2

        val fitsOnOneLine =
          singleArgument || excludeRanges.nonEmpty ||
          charactersInside <= style.maxColumn

        val expirationToken: Token =
          if (isDefnSite(leftOwner) && !isBracket) defnSiteLastToken(leftOwner)
          else rhsOptimalToken(leftTok2tok(close))

        val tooManyArguments = args.length > 100

        val isTuple = leftOwner match {
          case _: Type.Tuple | _: Term.Tuple => true
          case _ => false
        }

        Seq(
            Split(modification,
                  0,
                  policy = singleLine(6),
                  ignoreIf = !fitsOnOneLine)
              .withOptimalToken(expirationToken)
              .withIndent(indent, close, Left),
            Split(newlineModification,
                  (1 + nestedPenalty + lhsPenalty) * bracketMultiplier,
                  policy = singleLine(5),
                  ignoreIf = !fitsOnOneLine || isTuple)
              .withOptimalToken(expirationToken)
              .withIndent(indent, close, Left),
            // TODO(olafur) singleline per argument!
            Split(modification,
                  (2 + lhsPenalty) * bracketMultiplier,
                  policy = oneArgOneLine,
                  ignoreIf = singleArgument || tooManyArguments)
              .withOptimalToken(expirationToken)
              .withIndent(StateColumn, close, Right),
            Split(Newline,
                  (3 + nestedPenalty + lhsPenalty) * bracketMultiplier,
                  policy = oneArgOneLine,
                  ignoreIf = singleArgument || isTuple)
              .withOptimalToken(expirationToken)
              .withIndent(indent, close, Left)
        )

      // Closing def site ): ReturnType
      case FormatToken(_, colon: `:`, _)
          if style.allowNewlineBeforeColonInMassiveReturnTypes &&
          defDefReturnType(leftOwner).isDefined =>
        val expire = lastToken(defDefReturnType(rightOwner).get)
        val penalizeNewlines = penalizeAllNewlines(
            expire, Constants.BracketPenalty)
        Seq(
            Split(NoSplit, 0).withPolicy(penalizeNewlines),
            // Spark style guide allows this:
            // https://github.com/databricks/scala-style-guide#indent
            Split(Newline, Constants.SparkColonNewline)
              .withIndent(2, expire, Left)
              .withPolicy(penalizeNewlines)
        )

      // Delim
      case FormatToken(_, _: `,`, _) =>
        Seq(
            Split(NoSplit, 0)
        )
      // These are mostly filtered out/modified by policies.
      case tok @ FormatToken(_: `,`, right, _) =>
        // TODO(olafur) DRY, see OneArgOneLine.
        val rhsIsAttachedComment =
          tok.right.isInstanceOf[Comment] && newlinesBetween(tok.between) == 0
        val binPack = isBinPack(leftOwner)
        val isInfix = leftOwner.isInstanceOf[Term.ApplyInfix]
        argumentStarts.get(hash(right)) match {
          case Some(nextArg) if binPack =>
            val nextComma: Option[FormatToken] = next(
                leftTok2tok(nextArg.tokens.last)) match {
              case t @ FormatToken(left: `,`, _, _)
                  if owners(left) == leftOwner =>
                Some(t)
              case _ => None
            }
            penalizeAllNewlines(nextArg.tokens.last, 3)
            val singleLine = SingleLineBlock(nextArg.tokens.last)
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
                      rhsOptimalToken(leftTok2tok(nextArg.tokens.last)),
                      killOnFail = true))
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
                Split(if (newlines > 0) Newline
                      else Space,
                      0)
            )
          case _ =>
            Seq(
                Split(Space, 0),
                Split(Newline, 1, ignoreIf = rhsIsAttachedComment)
            )
        }
      case FormatToken(_, _: `;`, _) =>
        Seq(
            Split(NoSplit, 0)
        )
      // Return always gets space
      case FormatToken(_: `return`, _, _) =>
        Seq(
            Split(Space, 0)
        )
      case FormatToken(left: Ident, _: `:`, _)
          if rightOwner.isInstanceOf[Type.Param] =>
        Seq(
            Split(NoSplit, 0)
        )
      case FormatToken(left: Ident, _: `:`, _) =>
        Seq(
            Split(identModification(left), 0)
        )
      case FormatToken(_, _: `:`, _) =>
        Seq(
            Split(NoSplit, 0)
        )
      // Only allow space after = in val if rhs is a single line or not
      // an infix application or an if. For example, this is allowed:
      // val x = function(a,
      //                  b)
      case FormatToken(tok: `=`, right, between) if (leftOwner match {
            case _: Defn.Type | _: Defn.Val | _: Defn.Var |
                _: Term.Update | _: Term.Assign | _: Term.Arg.Named =>
              true
            case t: Term.Param => t.default.isDefined
            case _ => false
          }) =>
        val rhs: Tree = leftOwner match {
          case l: Term.Assign => l.rhs
          case l: Term.Update => l.rhs
          case l: Term.Arg.Named => l.rhs
          case l: Term.Param => l.default.get
          case l: Defn.Type => l.body
          case l: Defn.Val => l.rhs
          case r: Defn.Var =>
            r.rhs match {
              case Some(x) => x
              case None => r // var x: Int = _, no policy
            }
        }

        val expire = rhs.tokens.last
        val spacePolicy: Policy = rhs match {
          case _: Term.ApplyInfix | _: Term.If => SingleLineBlock(expire)
          case _ => NoPolicy
        }

        val penalty = leftOwner match {
          case l: Term.Arg.Named if style.binPackArguments =>
            Constants.BinPackAssignmentPenalty
          case l: Term.Param if style.binPackParameters =>
            Constants.BinPackAssignmentPenalty
          case _ => 0
        }

        val mod: Modification =
          if (isAttachedComment(right, between)) Space
          else Newline

        Seq(
            Split(Space, 0, policy = spacePolicy),
            Split(mod, 1 + penalty, ignoreIf = isJsNative(right))
              .withIndent(2, expire, Left)
        )
      case tok @ FormatToken(left, dot: `.`, _)
          if rightOwner.isInstanceOf[Term.Select] &&
          isOpenApply(next(next(tok)).right) && !left.isInstanceOf[`_ `] &&
          !parents(rightOwner).exists(_.isInstanceOf[Import]) =>
        val owner = rightOwner.asInstanceOf[Term.Select]
        val nestedPenalty = nestedSelect(rightOwner) + nestedApplies(leftOwner)
        val chain = getSelectChain(owner)
        val lastToken = lastTokenInChain(chain)
        val optimalToken = chainOptimalToken(chain)
        val breakOnEveryDot = Policy({
          case Decision(t @ FormatToken(left, dot2: `.`, _), s)
              if chain.contains(owners(dot2)) =>
            Decision(t, Seq(Split(Newline, 1)))
        }, lastToken.end)
        val exclude = getExcludeIf(lastToken)
        // This policy will apply to both the space and newline splits, otherwise
        // the newline is too cheap even it doesn't actually prevent other newlines.
        val penalizeNewlinesInApply = penalizeAllNewlines(lastToken, 2)
        val noSplitPolicy = SingleLineBlock(lastToken, exclude)
          .andThen(penalizeNewlinesInApply.f)
          .copy(expire = lastToken.end)
        val newlinePolicy = breakOnEveryDot
          .andThen(penalizeNewlinesInApply.f)
          .copy(expire = lastToken.end)
        Seq(
            Split(NoSplit, 0)
              .withOptimalToken(optimalToken, killOnFail = false)
              .withPolicy(noSplitPolicy),
            Split(Newline.copy(acceptNoSplit = true), 2 + nestedPenalty)
              .withPolicy(newlinePolicy)
              .withIndent(2, lastToken, Left)
        )
      // ApplyUnary
      case tok @ FormatToken(_: Ident, _: Literal, _)
          if leftOwner == rightOwner =>
        Seq(
            Split(NoSplit, 0)
        )
      case tok @ FormatToken(op: Ident, _, _) if leftOwner.parent.exists {
            case unary: Term.ApplyUnary => unary.op.tokens.head == op
            case _ => false
          } =>
        Seq(
            Split(NoSplit, 0)
        )
      // Annotations, see #183 for discussion on this.
      case FormatToken(_, bind: `@`, _) if rightOwner.isInstanceOf[Pat.Bind] =>
        Seq(
            Split(Space, 0)
        )
      case FormatToken(bind: `@`, _, _) if leftOwner.isInstanceOf[Pat.Bind] =>
        Seq(
            Split(Space, 0)
        )
      case FormatToken(_: `@`, right: Delim, _) =>
        Seq(Split(NoSplit, 0))
      case FormatToken(_: `@`, right: Ident, _) =>
        // Add space if right starts with a symbol
        Seq(Split(identModification(right), 0))

      // Template
      case FormatToken(_, right: `extends`, _) =>
        val template = defnTemplate(rightOwner)
        val lastToken = template
          .flatMap(templateCurly)
          .orElse(template.map(_.tokens.last))
          .getOrElse(rightOwner.tokens.last)
        val breakOnEveryWith =
          if (style.binPackParentConstructors) NoPolicy
          else {
            Policy({
              case Decision(t @ FormatToken(_, right: `with`, _), splits)
                  if template == ownersMap.get(hash(right)) =>
                Decision(t, splits.filter(_.modification.isNewline))
            }, lastToken.end)
          }
        Seq(
            Split(Space, 0)
              .withPolicy(SingleLineBlock(lastToken))
              .withIndent(Num(4), lastToken, Left),
            Split(Newline, 1)
              .withPolicy(breakOnEveryWith)
              .withIndent(Num(4), lastToken, Left)
        )
      case tok @ FormatToken(_, right: `with`, _) if (rightOwner match {
            case _: Template => true
            case _ => false
          }) =>
        val template = rightOwner
        val expire = templateCurly(rightOwner)
        Seq(
            Split(Space, 0),
            Split(Newline, 1).withPolicy(Policy({
              // Force template to be multiline.
              case d @ Decision(FormatToken(open: `{`, right, _), splits)
                  if !right.isInstanceOf[`}`] && // corner case, body is {}
                  childOf(template, owners(open)) =>
                d.copy(splits = splits.filter(_.modification.isNewline))
            }, expire.end))
        )
      // If/For/While/For with (
      case FormatToken(open: `(`, _, _) if (leftOwner match {
            case _: Term.If | _: Term.While => true
            case _: Term.For | _: Term.ForYield
                if !isSuperfluousParenthesis(open, leftOwner) =>
              true
            case _ => false
          }) =>
        val close = matchingParentheses(hash(open))
        val penalizeNewlines = penalizeNewlineByNesting(open, close)
        val indent: Length =
          if (style.alignByIfWhileOpenParen) StateColumn
          else style.continuationIndentCallSite
        Seq(
            Split(NoSplit, 0)
              .withIndent(indent, close, Left)
              .withPolicy(penalizeNewlines)
        )
      case FormatToken(_: `if`, _, _) if leftOwner.isInstanceOf[Term.If] =>
        val owner = leftOwner.asInstanceOf[Term.If]
        val expire = owner.elsep.tokens.lastOption.getOrElse(owner.tokens.last)
        val elses = getElseChain(owner)
        val breakOnlyBeforeElse = Policy({
          case d @ Decision(t, s)
              if elses.contains(t.right) && !t.left.isInstanceOf[`}`] =>
            d.safeNoNewlines
        }, expire.end)
        Seq(
            Split(Space, 0)
              .withOptimalToken(expire, killOnFail = true)
              .withPolicy(SingleLineBlock(expire)),
            Split(Space, 1).withPolicy(breakOnlyBeforeElse)
        )
      case FormatToken(close: `)`, right, between)
          if leftOwner.isInstanceOf[Term.If] &&
          !isFirstOrLastToken(close, leftOwner) =>
        val owner = leftOwner.asInstanceOf[Term.If]
        val expire = owner.thenp.tokens.last
        val rightIsOnNewLine = newlines > 0
        // Inline comment attached to closing `)`
        val attachedComment = !rightIsOnNewLine && isInlineComment(right)
        val newlineModification: Modification =
          if (attachedComment)
            Space // Inline comment will force newline later.
          else Newline
        Seq(
            Split(Space, 0, ignoreIf = attachedComment || newlines > 0)
              .withIndent(2, expire, Left)
              .withPolicy(SingleLineBlock(expire)),
            Split(newlineModification, 1).withIndent(2, expire, Left)
        )
      case tok @ FormatToken(_: `}`, els: `else`, _) =>
        Seq(
            Split(Space, 0)
        )
      case tok @ FormatToken(_, els: `else`, _) =>
        val expire = rhsOptimalToken(leftTok2tok(rightOwner.tokens.last))
        Seq(
            Split(Space, 0, ignoreIf = newlines > 0)
              .withOptimalToken(expire)
              .withPolicy(SingleLineBlock(expire)),
            Split(Newline, 1)
        )
      // Last else branch
      case tok @ FormatToken(els: `else`, _, _)
          if !nextNonComment(tok).right.isInstanceOf[`if`] =>
        val expire = leftOwner match {
          case t: Term.If => t.elsep.tokens.last
          case x => throw new UnexpectedTree[Term.If](x)
        }
        Seq(
            Split(Space,
                  0,
                  policy = SingleLineBlock(expire),
                  ignoreIf = newlines > 0),
            Split(Newline, 1).withIndent(2, expire, Left)
        )

      // Type variance
      case tok @ FormatToken(_: Ident, _: Ident, _)
          if isTypeVariant(leftOwner) =>
        Seq(
            Split(NoSplit, 0)
        )

      // Var args
      case FormatToken(_, asterisk: Ident, _)
          if asterisk.code == "*" &&
          rightOwner.isInstanceOf[Type.Arg.Repeated] =>
        Seq(
            Split(NoSplit, 0)
        )

      // ApplyInfix.
      case FormatToken(open: `(`, right, _)
          if leftOwner.isInstanceOf[Term.ApplyInfix] =>
        val isConfig = opensConfigStyle(formatToken)
        val close = matchingParentheses(hash(open))
        val breakOnClose = Policy({
          case Decision(t @ FormatToken(_, `close`, _), s) =>
            Decision(t, Seq(Split(Newline, 0)))
        }, close.end)
        val indent: Length = right match {
          case _: `if` => StateColumn
          case _ => Num(4)
        }
        Seq(
            Split(Newline, 0, ignoreIf = !isConfig)
              .withPolicy(breakOnClose)
              .withIndent(style.continuationIndentCallSite, close, Right),
            Split(NoSplit, 0, ignoreIf = isConfig)
              .withIndent(indent, close, Left)
              .withPolicy(penalizeAllNewlines(close, 1))
        )
      // Infix operator.
      case tok @ FormatToken(op: Ident, right, between)
          if leftOwner.parent.exists {
            case infix: Term.ApplyInfix => infix.op == owners(op)
            case _ => false
          } =>
        val owner = leftOwner.parent.get.asInstanceOf[Term.ApplyInfix]
        val isAssignment = isAssignmentOperator(op)
        val isBool = isBoolOperator(op)
        // TODO(olafur) Document that we only allow newlines for this subset
        // of infix operators. To force a newline for other operators it's
        // possible to wrap arguments in parentheses.
        val weControlSplit =
          isAssignment || isBool || newlineOkOperators.contains(op.code)
        val openParenPenalty = if (right.isInstanceOf[`(`]) 0 else 1
        val newlineCost =
          if (isAssignment || isBool) 1
          else if (weControlSplit) 3
          else 0 // Ignored
        val indent =
          if (isAssignment) 2
          else 0
        // Optimization, assignment operators make the state space explode in
        // sbt build files because of := operators everywhere.
        val optimalToken =
          if (isAssignment)
            for {
              lastArg <- owner.args.lastOption
              lastToken <- lastArg.tokens.lastOption
            } yield OptimalToken(lastToken)
          else None
        val modification = newlines2Modification(between)
        Seq(
            Split(modification, 0, ignoreIf = weControlSplit),
            Split(Space,
                  0,
                  optimalAt = optimalToken,
                  ignoreIf = !weControlSplit),
            Split(Newline,
                  newlineCost,
                  ignoreIf = !weControlSplit)
              .withIndent(indent, formatToken.right, Left)
        )

      // Pat
      case tok @ FormatToken(or: Ident, _, _)
          if or.code == "|" && leftOwner.isInstanceOf[Pat.Alternative] =>
        Seq(
            Split(Space, 0),
            Split(Newline, 1)
        )
      case tok @ FormatToken(_: Ident | _: Literal | _: Interpolation.End |
                             _: Xml.End,
                             _: Ident | _: Literal | _: Xml.Start,
                             _) =>
        Seq(
            Split(Space, 0)
        )

      // Case
      case tok @ FormatToken(_, _: `match`, _) =>
        Seq(
            Split(Space, 0)
        )

      // Protected []
      case tok @ FormatToken(_, _: `[`, _)
          if isModPrivateProtected(leftOwner) =>
        Seq(
            Split(NoSplit, 0)
        )
      case tok @ FormatToken(_: `[`, _, _)
          if isModPrivateProtected(leftOwner) =>
        Seq(
            Split(NoSplit, 0)
        )

      // Case
      case tok @ FormatToken(cs: `case`, _, _)
          if leftOwner.isInstanceOf[Case] =>
        val owner = leftOwner.asInstanceOf[Case]
        val arrow = getArrow(owner)
        // TODO(olafur) expire on token.end to avoid this bug.
        val expire = Option(owner.body)
          .filter(_.tokens.exists(!_.isInstanceOf[Trivia]))
          .map(lastToken)
          .map(getRightAttachedComment)
          .getOrElse(arrow) // edge case, if body is empty expire on arrow.

        Seq(
            // Either everything fits in one line or break on =>
            Split(Space, 0)
              .withOptimalToken(expire, killOnFail = true)
              .withPolicy(SingleLineBlock(expire)),
            Split(Space, 1)
              .withPolicy(Policy({
                case Decision(t @ FormatToken(`arrow`, right, between), s)
                    // TODO(olafur) any other corner cases?
                    if !right.isInstanceOf[`{`] &&
                    !isAttachedComment(right, between) =>
                  Decision(t, s.filter(_.modification.isNewline))
              }, expire = expire.end))
              .withIndent(2, expire, Left) // case body indented by 2.
              .withIndent(2, arrow, Left) // cond body indented by 4.
        )
      case tok @ FormatToken(_, cond: `if`, _)
          if rightOwner.isInstanceOf[Case] =>
        val arrow = getArrow(rightOwner.asInstanceOf[Case])
        val exclude =
          insideBlock(tok, arrow, _.isInstanceOf[`{`]).map(parensRange)
        val singleLine = SingleLineBlock(arrow, exclude = exclude)

        Seq(
            Split(Space, 0, policy = singleLine),
            Split(Newline, 1).withPolicy(penalizeNewlineByNesting(cond, arrow))
        )
      // Inline comment
      case FormatToken(_, c: Comment, between) =>
        Seq(Split(newlines2Modification(between), 0))
      // Commented out code should stay to the left
      case FormatToken(c: Comment, _, between) if c.code.startsWith("//") =>
        Seq(Split(Newline, 0))
      case FormatToken(c: Comment, _, between) =>
        Seq(Split(newlines2Modification(between), 0))

      // Term.ForYield
      case tok @ FormatToken(_, arrow: `if`, _)
          if rightOwner.isInstanceOf[Enumerator.Guard] =>
        Seq(
            // Either everything fits in one line or break on =>
            Split(Space, 0, ignoreIf = newlines > 0),
            Split(Newline, 1)
        )
      case tok @ FormatToken(arrow: `<-`, _, _)
          if leftOwner.isInstanceOf[Enumerator.Generator] =>
        val lastToken = leftOwner.tokens.last
        val indent: Length =
          if (style.alignByArrowEnumeratorGenerator) StateColumn
          else Num(0)
        Seq(
            // Either everything fits in one line or break on =>
            Split(Space, 0).withIndent(indent, lastToken, Left)
        )
      case tok @ FormatToken(_: `yield`, right, _)
          if leftOwner.isInstanceOf[Term.ForYield] &&
          !right.isInstanceOf[`{`] =>
        val lastToken = leftOwner.asInstanceOf[Term.ForYield].body.tokens.last
        Seq(
            // Either everything fits in one line or break on =>
            Split(Space, 0).withPolicy(SingleLineBlock(lastToken)),
            Split(Newline, 1).withIndent(2, lastToken, Left)
        )
      // Interpolation
      case FormatToken(_, _: Interpolation.Id | _: Xml.Start, _) =>
        Seq(
            Split(Space, 0)
        )
      case FormatToken(_: Interpolation.Id | _: Xml.Start, _, _) =>
        Seq(
            Split(NoSplit, 0)
        )
      // Throw exception
      case FormatToken(_: `throw`, _, _) =>
        Seq(
            Split(Space, 0)
        )
      // Open paren generally gets no space.
      case FormatToken(open: `(`, _, _) =>
        Seq(
            Split(NoSplit, 0)
        )

      // Singleton types
      case FormatToken(_, _: `type`, _)
          if rightOwner.isInstanceOf[Type.Singleton] =>
        Seq(
            Split(NoSplit, 0)
        )
      // seq to var args foo(seq:_*)
      case FormatToken(_: `:`, _: `_ `, _)
          if next(formatToken).right.code == "*" =>
        Seq(
            Split(Space, 0)
        )
      case FormatToken(_: `_ `, asterisk: Ident, _)
          if asterisk.code == "*" &&
          prev(formatToken).left.isInstanceOf[`:`] =>
        Seq(
            Split(NoSplit, 0)
        )
      // Xml
      case FormatToken(_: Xml.Part, _, _) =>
        Seq(
            Split(NoSplit, 0)
        )
      case FormatToken(_, _: Xml.Part, _) =>
        Seq(
            Split(NoSplit, 0)
        )

      // Fallback
      case FormatToken(_, _: `.` | _: `#`, _) =>
        Seq(
            Split(NoSplit, 0)
        )
      case FormatToken(_: `.` | _: `#`, _: Ident | _: `this`, _) =>
        Seq(
            Split(NoSplit, 0)
        )
      case FormatToken(_, _: `]` | _: `)`, _) =>
        Seq(
            Split(NoSplit, 0)
        )
      case FormatToken(_, _: Keyword, _) =>
        Seq(
            Split(Space, 0)
        )
      case FormatToken(_: Keyword | _: Modifier, _, _) =>
        Seq(
            Split(Space, 0)
        )
      case FormatToken(_: `[`, _, _) =>
        Seq(
            Split(NoSplit, 0)
        )
      case FormatToken(_, _: Delim, _) =>
        Seq(
            Split(Space, 0)
        )
      case FormatToken(_: Delim, _, _) =>
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
    cache.getOrElseUpdate(formatToken, {
      val splits = getSplits(formatToken).map(_.adapt(formatToken))
      formatToken match {
        // TODO(olafur) refactor into "global policy"
        // Only newlines after inline comments.
        case FormatToken(c: Comment, _, _) if c.code.startsWith("//") =>
          val newlineSplits = splits.filter(_.modification.isNewline)
          if (newlineSplits.isEmpty) Seq(Split(Newline, 0))
          else newlineSplits
        case _ => splits
      }
    })

  private implicit def int2num(n: Int): Num = Num(n)
}
