package org.scalafmt.internal

import scala.language.implicitConversions

import org.scalafmt.Error.UnexpectedTree
import org.scalafmt.internal.Policy.NoPolicy
import org.scalafmt.util.LoggerOps
import org.scalafmt.util.TokenOps
import org.scalafmt.util.TreeOps
import scala.collection.mutable
import scala.meta.Tree
import scala.meta.internal.ast.Case
import scala.meta.internal.ast.Defn
import scala.meta.internal.ast.Enumerator
import scala.meta.internal.ast.Import
import scala.meta.internal.ast.Name
import scala.meta.internal.ast.Pat
import scala.meta.internal.ast.Pkg
import scala.meta.internal.ast.Template
import scala.meta.internal.ast.Term
import scala.meta.internal.ast.Type
import scala.meta.tokens.Token

// Too many to import individually.
import scala.meta.tokens.Token._

object Constants {
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

  def getSplits(formatToken: FormatToken): Seq[Split] = {
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
            Split(NoSplit, 0).withPolicy(policy)
        )
      case FormatToken(_, close: `}`, _)
          if parents(rightOwner).exists(_.isInstanceOf[Import]) ||
          rightOwner.isInstanceOf[Term.Interpolate] =>
        Seq(
            Split(NoSplit, 0)
        )
      case FormatToken(_: `.`, underscore: `_ `, _)
          if parents(rightOwner).exists(_.isInstanceOf[Import]) =>
        Seq(
            Split(NoSplit, 0)
        )

      // Top level defns
      case tok@FormatToken(_, right, _)
          if !right.isInstanceOf[`package `] && !isDocstring(tok.left) &&
          gets2x(tok) =>
        Seq(
            Split(Newline2x, 0)
        )

      // { ... } Blocks
      case tok@FormatToken(open: `{`, right, between) =>
        val nl = Newline(shouldGet2xNewlines(tok))
        val close = matchingParentheses(hash(open))
        val blockSize = close.start - open.end
        val ignore = blockSize > style.maxColumn || isInlineComment(right)
        val newlineBeforeClosingCurly = Policy({
          case Decision(t@FormatToken(_, `close`, _), s) =>
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
      // For loop with (
      case tok@FormatToken(_: `(`, _, _)
          if leftOwner.isInstanceOf[Term.For] ||
          leftOwner.isInstanceOf[Term.ForYield] =>
        // TODO(olafur) allow newlines?
        Seq(
            Split(NoSplit, 0)
        )

      // Term.Function
      case FormatToken(arrow: `=>`, right, _)
          if statementStarts.contains(hash(right)) &&
          leftOwner.isInstanceOf[Term.Function] =>
        val endOfFunction = leftOwner.tokens.last
        val canBeSpace = statementStarts(hash(right))
          .isInstanceOf[Term.Function]
        Seq(
            Split(Space, 0, ignoreIf = !canBeSpace),
            Split(Newline, 1).withIndent(2, endOfFunction, Left)
        )
      case FormatToken(arrow: `=>`, right, _)
          if leftOwner.isInstanceOf[Term.Function] =>
        val endOfFunction = functionExpire(
            leftOwner.asInstanceOf[Term.Function])
        Seq(
            Split(Space, 0, ignoreIf = isInlineComment(right))
              .withPolicy(SingleLineBlock(endOfFunction)),
            Split(Newline, 1 + nestedApplies(leftOwner))
              .withIndent(2, endOfFunction, Right)
        )
      // Case arrow
      case tok@FormatToken(arrow: `=>`, right, between)
          if leftOwner.isInstanceOf[Case] =>
        Seq(
            Split(Space, 0, ignoreIf = newlines != 0), // Gets killed by `case` policy.
            Split(
                Newline(gets2x = false, hasIndent = rhsIsCommentedOut(tok)), 1)
        )
      // New statement
      case tok@FormatToken(_: `;`, right, between)
          if startsStatement(tok) && newlines == 0 =>
        val expire = statementStarts(hash(right)).tokens.last
        Seq(
            Split(Space, 0)
              .withOptimalToken(expire)
              .withPolicy(SingleLineBlock(expire)),
            // For some reason, this newline cannot cost 1.
            Split(Newline(shouldGet2xNewlines(tok)), 0)
        )

      case tok@FormatToken(left, right, between) if startsStatement(tok) =>
        val oldNewlines = newlinesBetween(between)
        val newline: Modification = Newline(shouldGet2xNewlines(tok))
        val expire = rightOwner.tokens
          .find(_.isInstanceOf[`=`])
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
          if noSpaceBeforeOpeningParen(rightOwner) =>
        Seq(
            Split(NoSplit, 0)
        )
      // Defn.{Object, Class, Trait}
      case tok@FormatToken(_: `object` | _: `class ` | _: `trait`, _, _) =>
        val expire = defnTemplate(leftOwner)
          .flatMap(templateCurly)
          .getOrElse(leftOwner.tokens.last)
        Seq(
            Split(Space, 0).withIndent(4, expire, Left)
        )
      case FormatToken(_: `(`, _, _)
          if style.binPackParameters && isDefnSite(leftOwner) =>
        Seq(
            Split(NoSplit, 0),
            Split(Newline, 1) // indent handled by name of def/class.
        )
      // DefDef
      case tok@FormatToken(_: `def`, name: Ident, _) =>
        Seq(
            Split(Space, 0).withIndent(4, defnSiteLastToken(leftOwner), Left)
        )
      case tok@FormatToken(e: `=`, right, _)
          if leftOwner.isInstanceOf[Defn.Def] =>
        val expire = leftOwner.asInstanceOf[Defn.Def].body.tokens.last
        val exclude = getExcludeIfEndingWithBlock(expire)
        val rhsIsJsNative = isJsNative(right)
        Seq(
            Split(
                Space, 0, policy = SingleLineBlock(expire, exclude = exclude)),
            Split(Newline, 0, ignoreIf = rhsIsJsNative)
              .withIndent(2, expire, Left)
        )
      // Named argument foo(arg = 1)
      case tok@FormatToken(e: `=`, right, _) if (leftOwner match {
            case t: Term.Arg.Named => true
            case t: Term.Param if t.default.isDefined => true
            case _ => false
          }) =>
        val expire = leftOwner match {
          case t: Term.Arg.Named => t.rhs.tokens.last
          case t: Term.Param => t.default.get.tokens.last
          case t => throw UnexpectedTree[Term.Param](t)
        }
        Seq(
            Split(Space, 0).withIndent(2, expire, Left)
        )
      case tok@FormatToken(open: `(`, _, _)
          if style.binPackParameters && isDefnSite(leftOwner) =>
        Seq(
            Split(NoSplit, 0)
        )
      // Term.Apply and friends
      case FormatToken(_: `(` | _: `[`, _, _)
          if style.binPackArguments && isCallSite(leftOwner) =>
        val open = formatToken.left
        val close = matchingParentheses(hash(open))
        val optimal =
          leftOwner.tokens.find(_.isInstanceOf[`,`]).orElse(Some(close))
        Seq(
            Split(NoSplit, 0)
              .withOptimalToken(optimal)
              .withIndent(4, close, Left),
            Split(Newline, 1).withIndent(4, close, Left)
        )
      case tok@FormatToken(_: `(` | _: `[`, right, between)
          if !isSuperfluousParenthesis(formatToken.left, leftOwner) &&
          (!style.binPackArguments && isCallSite(leftOwner)) ||
          (!style.binPackParameters && isDefnSite(leftOwner)) =>
        val open = tok.left.asInstanceOf[Delim]
        val close = matchingParentheses(hash(open))
        val (lhs, args) = splitApplyIntoLhsAndArgsLifted(leftOwner).getOrElse {
          logger.error(s"""Unknown tree
                          |${log(leftOwner.parent.get)}
                          |${isDefnSite(leftOwner)}""".stripMargin)
          throw UnexpectedTree[Term.Apply](leftOwner)
        }
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

        //          insideBlock(tok, close, _.isInstanceOf[`{`])
        val indent = leftOwner match {
          case _: Pat => Num(0) // Indentation already provided by case.
          case x if isDefnSite(x) && !x.isInstanceOf[Type.Apply] => Num(0)
          case _ => Num(4)
        }

        // It seems acceptable to unindent by the continuation indent inside
        // curly brace wrapped blocks.
        val unindentAtExclude: PartialFunction[Decision, Decision] = {
          case Decision(t, s) if exclude.contains(t.left) =>
            val close = matchingParentheses(hash(t.left))
            Decision(t, s.map(_.withIndent(-4, close, Left)))
        }
        val singleArgument = args.length == 1

        val baseSingleLinePolicy =
          if (isBracket) {
            if (singleArgument)
              SingleLineBlock(
                  close, excludeRanges, disallowInlineComments = false)
            else SingleLineBlock(close)
          } else {
            if (singleArgument) {
              penalizeAllNewlines(close, 5) // TODO(olafur) magic number
            } else SingleLineBlock(close, excludeRanges)
          }
        val singleLine =
          if (exclude.isEmpty || isBracket) baseSingleLinePolicy
          else baseSingleLinePolicy.andThen(unindentAtExclude)

        val oneArgOneLine = OneArgOneLineSplit(open)

        // TODO(olafur) document how "config style" works.
        val configStyle = oneArgOneLine.copy(f = oneArgOneLine.f.orElse {
            case Decision(t@FormatToken(_, `close`, _), splits) =>
              Decision(t, Seq(Split(Newline, 0)))
          })
        val closeFormatToken = prev(leftTok2tok(close))

        val isConfigStyle =
          style.configStyleArguments && newlinesBetween(between) > 0 &&
          newlinesBetween(closeFormatToken.between) > 0

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
          if (isDefnSite(leftOwner)) defnSiteLastToken(leftOwner)
          else rhsOptimalToken(leftTok2tok(close))

        val tooManyArguments = args.length > 100

        Seq(
            Split(modification,
                  0,
                  policy = singleLine,
                  ignoreIf = !fitsOnOneLine || isConfigStyle)
              .withOptimalToken(expirationToken)
              .withIndent(indent, close, Left),
            Split(newlineModification,
                  (1 + nestedPenalty + lhsPenalty) * bracketMultiplier,
                  policy = singleLine,
                  ignoreIf = !fitsOnOneLine || isConfigStyle)
              .withOptimalToken(expirationToken)
              .withIndent(indent, close, Left),
            // TODO(olafur) singleline per argument!
            Split(modification,
                  (2 + lhsPenalty) * bracketMultiplier,
                  policy = oneArgOneLine,
                  ignoreIf = singleArgument || isConfigStyle ||
                    tooManyArguments)
              .withOptimalToken(expirationToken)
              .withIndent(StateColumn, close, Right),
            Split(Newline,
                  (3 + nestedPenalty + lhsPenalty) * bracketMultiplier,
                  policy = oneArgOneLine,
                  ignoreIf = singleArgument || isConfigStyle)
              .withOptimalToken(expirationToken)
              .withIndent(indent, close, Left),
            Split(Newline,
                  0,
                  policy = configStyle,
                  ignoreIf = !isConfigStyle).withIndent(indent, close, Right)
        )

      // Closing def site ): ReturnType
      case FormatToken(_, close: `)`, _)
          if next(formatToken).right.isInstanceOf[`:`] &&
          !style.binPackParameters && defDefReturnType(rightOwner).isDefined =>
        val expire = lastToken(defDefReturnType(rightOwner).get)
        val penalizeNewlines = penalizeAllNewlines(
            expire, Constants.BracketPenalty)
        Seq(
            Split(NoSplit, 0).withPolicy(penalizeNewlines),
            // In case the return type is super long, we may need to break
            // before the closing ).
            Split(Newline, 3)
        )

      // Delim
      case FormatToken(_, _: `,`, _) =>
        Seq(
            Split(NoSplit, 0)
        )
      // These are mostly filtered out/modified by policies.
      case tok@FormatToken(_: `,`, _, _) =>
        // TODO(olafur) DRY, see OneArgOneLine.
        val rhsIsAttachedComment =
          tok.right.isInstanceOf[Comment] && newlinesBetween(tok.between) == 0
        val endOfArgument = findFirst(next(tok), leftOwner.tokens.last) {
          case FormatToken(expire: `,`, _, _) if owners(expire) == leftOwner =>
            true
          case FormatToken(expire: `)`, _, _) if owners(expire) == leftOwner =>
            true
          case _ => false
        }
        val singleLineToEndOfArg: Policy = endOfArgument
          .map(expire => SingleLineBlock(expire.left))
          .getOrElse(NoPolicy)
        val optimalToken = endOfArgument.map(_.left)
        Seq(
            Split(Space, 0)
              .withOptimalToken(optimalToken)
              .withPolicy(singleLineToEndOfArg),
            Split(Newline, 1, ignoreIf = rhsIsAttachedComment)
        )
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
            Split(Space, 0)
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
      case FormatToken(tok: `=`, right, between) // TODO(olafur) scala.meta should have uniform api for these two
          if leftOwner.isInstanceOf[Defn.Val] ||
          leftOwner.isInstanceOf[Defn.Var] =>
        val rhs: Tree = leftOwner match {
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

        val mod: Modification =
          if (isAttachedComment(right, between)) Space
          else Newline

        Seq(
            Split(Space, 0, policy = spacePolicy),
            Split(mod, 1, ignoreIf = isJsNative(right))
              .withIndent(2, expire, Left)
        )
      case tok@FormatToken(left, dot: `.`, _)
          if rightOwner.isInstanceOf[Term.Select] &&
          isOpenApply(next(next(tok)).right) && !left.isInstanceOf[`_ `] &&
          !parents(rightOwner).exists(_.isInstanceOf[Import]) =>
        val owner = rightOwner.asInstanceOf[Term.Select]
        val nestedPenalty = nestedSelect(rightOwner) + nestedApplies(leftOwner)
        val chain = getSelectChain(owner)
        val lastToken = lastTokenInChain(chain)
        val optimalToken = chainOptimalToken(chain)
        val breakOnEveryDot = Policy({
          case Decision(t@FormatToken(left, dot2: `.`, _), s)
              if chain.contains(owners(dot2)) =>
            Decision(t, Seq(Split(Newline, 1)))
        }, lastToken.end)
        val exclude = getExcludeIfEndingWithBlock(lastToken)
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
            Split(Newline, 1 + nestedPenalty)
              .withPolicy(newlinePolicy)
              .withIndent(2, lastToken, Left)
        )
      // ApplyUnary
      case tok@FormatToken(_: Ident, _: Literal, _)
          if leftOwner == rightOwner =>
        Seq(
            Split(NoSplit, 0)
        )
      case tok@FormatToken(op: Ident, _, _) if leftOwner.parent.exists {
            case unary: Term.ApplyUnary => unary.op.tokens.head == op
            case _ => false
          } =>
        Seq(
            Split(NoSplit, 0)
        )
      // Annotations
      case FormatToken(left: Ident, bind: `@`, _)
          if rightOwner.isInstanceOf[Pat.Bind] =>
        Seq(
            Split(identModification(left), 0)
        )
      case FormatToken(_, bind: `@`, _) if rightOwner.isInstanceOf[Pat.Bind] =>
        Seq(
            Split(NoSplit, 0)
        )
      case FormatToken(_: `@`, right: Delim, _) => Seq(Split(NoSplit, 0))
      case FormatToken(_: `@`, right: Ident, _) =>
        // Add space if right starts with a symbol
        Seq(Split(identModification(right), 0))

      // Template
      case FormatToken(_, right: `extends`, _) =>
        val lastToken = defnTemplate(rightOwner)
          .flatMap(templateCurly)
          .getOrElse(rightOwner.tokens.last)
        Seq(
            Split(Space, 0).withPolicy(SingleLineBlock(lastToken)),
            Split(Newline, 1)
        )
      case tok@FormatToken(_, right: `with`, _)
          if rightOwner.isInstanceOf[Template] =>
        val template = rightOwner
        val expire = templateCurly(rightOwner)
        Seq(
            Split(Space, 0),
            Split(Newline, 1).withPolicy(Policy({
              // Force template to be multiline.
              case d@Decision(FormatToken(open: `{`, right, _), splits)
                  if !right.isInstanceOf[`}`] &&  // corner case, body is {}
                  childOf(template, owners(open)) =>
                d.copy(splits = splits.filter(_.modification.isNewline))
            }, expire.end))
        )
      // If
      case FormatToken(open: `(`, _, _) if leftOwner.isInstanceOf[Term.If] =>
        val close = matchingParentheses(hash(open))
        val penalizeNewlines = penalizeNewlineByNesting(open, close)
        Seq(
            Split(NoSplit, 0)
              .withIndent(StateColumn, close, Left)
              .withPolicy(penalizeNewlines)
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
            Split(Space, 0, ignoreIf = attachedComment)
              .withIndent(2, expire, Left)
              .withPolicy(SingleLineBlock(expire)),
            Split(newlineModification, 1).withIndent(2, expire, Left)
        )
      case tok@FormatToken(_: `}`, els: `else`, _) =>
        Seq(
            Split(Space, 0)
        )
      case tok@FormatToken(_, els: `else`, _) =>
        val expire = rhsOptimalToken(leftTok2tok(rightOwner.tokens.last))
        Seq(
            Split(Space, 0, ignoreIf = newlines > 0)
              .withOptimalToken(expire)
              .withPolicy(SingleLineBlock(expire)),
            Split(Newline, 1)
        )
      // Last else branch
      case tok@FormatToken(els: `else`, _, _)
          if !nextNonComment(tok).right.isInstanceOf[`if`] =>
        val expire = leftOwner match {
          case t: Term.If => t.elsep.tokens.last
          case x => throw new UnexpectedTree[Term.If](x)
        }
        Seq(
            Split(Space, 0, policy = SingleLineBlock(expire)),
            Split(Newline, 1).withIndent(2, expire, Left)
        )

      // Type variance
      case tok@FormatToken(_: Ident, _: Ident, _)
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
        val close = matchingParentheses(hash(open))
        val indent: Length = right match {
          case _: `if` => StateColumn
          case _ => Num(4)
        }
        Seq(
            Split(NoSplit, 0).withIndent(indent, close, Left)
        )
      case FormatToken(_, open: `(`, _)
          if rightOwner.isInstanceOf[Term.ApplyInfix] =>
        val close = matchingParentheses(hash(open))
        val optimalToken = Some(OptimalToken(close))
        Seq(
            Split(Space, 0, optimalAt = optimalToken)
              .withPolicy(SingleLineBlock(close)),
            Split(Newline, 1, optimalAt = optimalToken)
        )
      // Infix operator.
      case tok@FormatToken(op: Ident, _, _) if leftOwner.parent.exists {
            case infix: Term.ApplyInfix => infix.op == owners(op)
            case _ => false
          } =>
        val owner = leftOwner.parent.get.asInstanceOf[Term.ApplyInfix]
        val isAssignment = isAssignmentOperator(op)
        val isBool = isBoolOperator(op)
        // TODO(olafur) Document that we only allow newlines for this subset
        // of infix operators. To force a newline for other operators it's
        // possible to wrap arguments in parentheses.
        val newlineOk =
          isAssignment || isBool || newlineOkOperators.contains(op.code)
        val newlineCost =
          if (isAssignment || isBool) 1
          else if (newlineOk) 3
          else 0 // Ignored
        val indent =
          if (isAssignment) 2
          else 0
        // Optimization, assignment operators make the state space explode in
        // sbt build files because of := operators everywhere.
        val optimalToken =
          if (isAssignment) Some(OptimalToken(owner.args.last.tokens.last))
          else None
        Seq(
            Split(Space, 0, optimalAt = optimalToken),
            Split(Newline, newlineCost, ignoreIf = !newlineOk)
              .withIndent(indent, formatToken.right, Left)
        )

      case tok@FormatToken(_: Ident | _: Literal | _: Interpolation.End |
                           _: Xml.End,
                           _: Ident | _: Literal | _: Xml.Start,
                           _) =>
        Seq(
            Split(Space, 0)
        )

      // Pat
      case tok@FormatToken(or: Ident, _, _)
          if or.code == "|" && leftOwner.isInstanceOf[Pat.Alternative] =>
        Seq(
            Split(Space, 0),
            Split(Newline, 1)
        )
      // Case
      case tok@FormatToken(_, _: `match`, _) =>
        Seq(
            Split(Space, 0)
        )

      // Protected []
      case tok@FormatToken(_, _: `[`, _) if isModPrivateProtected(leftOwner) =>
        Seq(
            Split(NoSplit, 0)
        )
      case tok@FormatToken(_: `[`, _, _) if isModPrivateProtected(leftOwner) =>
        Seq(
            Split(NoSplit, 0)
        )

      // Case
      case tok@FormatToken(cs: `case`, _, _) if leftOwner.isInstanceOf[Case] =>
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
                case Decision(t@FormatToken(`arrow`, right, between), s)
                    // TODO(olafur) any other corner cases?
                    if !right.isInstanceOf[`{`] &&
                    !isAttachedComment(right, between) =>
                  Decision(t, s.filter(_.modification.isNewline))
              }, expire = expire.end))
              .withIndent(2, expire, Left) // case body indented by 2.
              .withIndent(2, arrow, Left) // cond body indented by 4.
        )
      case tok@FormatToken(_, cond: `if`, _)
          if rightOwner.isInstanceOf[Case] =>
        val arrow = getArrow(rightOwner.asInstanceOf[Case])
        val exclude =
          insideBlock(tok, arrow, _.isInstanceOf[`{`]).map(parensRange)
        val singleLine = SingleLineBlock(arrow, exclude = exclude)
        Seq(
            Split(Space, 0, policy = singleLine),
            Split(Newline, 1)
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
      case tok@FormatToken(_, arrow: `if`, _)
          if rightOwner.isInstanceOf[Enumerator.Guard] =>
        Seq(
            // Either everything fits in one line or break on =>
            Split(Space, 0),
            Split(Newline, 1).withIndent(4, leftOwner.tokens.last, Left)
        )
      case tok@FormatToken(arrow: `<-`, _, _)
          if leftOwner.isInstanceOf[Enumerator.Generator] =>
        val lastToken =
          findSiblingGuard(leftOwner.asInstanceOf[Enumerator.Generator])
            .map(_.tokens.last)
            .getOrElse(rightOwner.tokens.last)
        Seq(
            // Either everything fits in one line or break on =>
            Split(Space, 0).withIndent(StateColumn, lastToken, Left)
        )
      case tok@FormatToken(_: `yield`, right, _)
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
      case FormatToken(_: `(`, _, _) =>
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
            Split(NoSplit, 0)
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
      case FormatToken(_: `.` | _: `#`, _: Ident, _) =>
        Seq(
            Split(NoSplit, 0)
        )
      case FormatToken(_, _: `)` | _: `]`, _) =>
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
