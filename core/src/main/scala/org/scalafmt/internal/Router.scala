package org.scalafmt.internal

import scala.language.implicitConversions

import org.scalafmt.Error.UnexpectedTree
import org.scalafmt.ScalaStyle

import scala.collection.mutable
import scala.meta.Tree
import scala.meta.internal.ast.Case
import scala.meta.internal.ast.Ctor
import scala.meta.internal.ast.Decl
import scala.meta.internal.ast.Defn
import scala.meta.internal.ast.Enumerator
import scala.meta.internal.ast.Import
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
}

/**
  * Assigns splits to format tokens.
  */
class Router(val style: ScalaStyle,
             val tree: Tree,
             val tokens: Array[FormatToken],
             val matchingParentheses: Map[TokenHash, Token],
             val statementStarts: Map[TokenHash, Tree],
             val ownersMap: Map[TokenHash, Tree])
    extends ScalaFmtLogger with FormatOps {

  def getSplits(formatToken: FormatToken): Seq[Split] = {
    val leftOwner = owners(formatToken.left)
    val rightOwner = owners(formatToken.right)
    formatToken match {
      case FormatToken(_: BOF, _, _) =>
        Seq(
            Split(NoSplit, 0)
        )
      case FormatToken(_, _: EOF, _) =>
        Seq(
            Split(Newline, 0) // End files with trailing newline
        )
      case tok if tok.left.name.startsWith("xml") &&
          tok.right.name.startsWith("xml") =>
        Seq(
            Split(NoSplit, 0)
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
        Seq(
            Split(NoSplit, 0).withPolicy(SingleLineBlock(
                matchingParentheses(hash(open))))
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
      case tok: FormatToken if !isDocstring(tok.left) && gets2x(tok) =>
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

        val (startsLambda, lambdaPolicy, lambdaArrow) =
          statementStarts.get(hash(right)).collect {
            case owner: Term.Function =>
              val arrow = owner.tokens.find(_.isInstanceOf[`=>`])
              logger.trace(s"$tok  ${leftTok2tok(arrow.get)}")
              val singleLineUntilArrow =
                newlineBeforeClosingCurly.orElse(SingleLineBlock(
                    arrow.getOrElse(owner.params.last.tokens.last)).f)
              logger.trace(s"${leftTok2tok(close)} ${leftTok2tok(arrow.get)}")
              (true, singleLineUntilArrow, arrow)
          }.getOrElse((false, NoPolicy, None))

        val skipSingleLineBlock =
          ignore || startsLambda || newlinesBetween(between) > 0

        Seq(
            Split(Space, 0, ignoreIf = skipSingleLineBlock)
              .withPolicy(SingleLineBlock(close)),
            Split(Space, 0, ignoreIf = !startsLambda, optimalAt = lambdaArrow)
              .withPolicy(lambdaPolicy),
            Split(nl, 1).withPolicy(newlineBeforeClosingCurly)
              .withIndent(2, close, Right)
        )
      // For loop with (
      case tok@FormatToken(_: `(`, _, _) if leftOwner.isInstanceOf[Term.For] ||
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
        Seq(
            Split(Newline, 0).withIndent(2, endOfFunction, Left)
        )
      case FormatToken(arrow: `=>`, right, _)
          if leftOwner.isInstanceOf[Term.Function] =>
        val endOfFunction = leftOwner.tokens.last
        Seq(
            Split(Space, 0).withPolicy(SingleLineBlock(endOfFunction)),
            Split(Newline, 1 + nestedApplies(leftOwner))
              .withIndent(2, endOfFunction, Left)
        )
      // New statement
      case tok@FormatToken(left, right, between) if startsStatement(tok) =>
        val oldNewlines = newlinesBetween(between)
        val newline: Modification = Newline(shouldGet2xNewlines(tok))
        //          if () Newline2x
        //          else Newline
        val expire = rightOwner.tokens.find(_.isInstanceOf[`=`])
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
                  ignoreIf = !spaceCouldBeOk,
                  optimalAt = Some(expire)).withPolicy(SingleLineBlock(
                expire)),
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
        val owner = leftOwner
        val expire = defnTemplate(owner).flatMap { templ =>
          templ.tokens.find(_.isInstanceOf[`{`])
        }.getOrElse(owner.tokens.last)
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
        //          if style.binPackParameters =>
        Seq(
            Split(Space, 0).withIndent(4, defnSiteLastToken(leftOwner), Left)
        )
      case tok@FormatToken(e: `=`, right, _)
          if leftOwner.isInstanceOf[Defn.Def] =>
        val expire = leftOwner.asInstanceOf[Defn.Def].body.tokens.last
        val rhsIsJsNative = isJsNative(right)
        Seq(
            Split(Space, 0, policy = SingleLineBlock(expire)),
            Split(Newline, 0, ignoreIf = rhsIsJsNative)
              .withIndent(2, expire, Left)
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
            Split(NoSplit, 0, optimalAt = optimal).withIndent(4, close, Left),
            Split(Newline, 1).withIndent(4, close, Left)
        )
      case tok@FormatToken(_: `(` | _: `[`, right, between)
          if (!style.binPackArguments && isCallSite(leftOwner)) ||
          (!style.binPackParameters && isDefnSite(leftOwner)) =>
        val open = tok.left.asInstanceOf[Delim]
        val (lhs, args): (Tree, Seq[Tree]) = leftOwner match {
          case t: Term.Apply => t.fun -> t.args
          case t: Pat.Extract => t.ref -> t.args
          case t: Pat.Tuple => t -> t.elements
          case t: Term.ApplyType => t -> t.targs
          case t: Term.Update => t.fun -> t.argss.flatten
          case t: Term.Tuple => t -> t.elements
          case t: Type.Apply => t.tpe -> t.args
          case t: Type.Param => t.name -> t.tparams
          // TODO(olafur) flatten correct? Filter by this () section?
          case t: Defn.Def => t.name -> t.paramss.flatten
          case t: Decl.Def => t.name -> t.paramss.flatten
          case t: Defn.Class => t.name -> t.ctor.paramss.flatten
          case t: Defn.Trait => t.name -> t.ctor.paramss.flatten
          case t: Ctor.Primary => t.name -> t.paramss.flatten
          case t: Ctor.Secondary => t.name -> t.paramss.flatten
          case x =>
            logger.error(s"""Unknown tree
                 |${log(x.parent.get)}
                 |${isDefnSite(leftOwner)}""".stripMargin)
            ???
        }
        val close = matchingParentheses(hash(open))
        // In long sequence of select/apply, we penalize splitting on
        // parens furthest to the right.
        val lhsPenalty = treeDepth(lhs)

        val isBracket = open.isInstanceOf[`[`]
        val bracketMultiplier =
          if (isBracket)
            Constants.BracketPenalty
          else 1

        val nestedPenalty = nestedApplies(leftOwner)
        val exclude =
          if (isBracket) insideBlock(tok, close, _.isInstanceOf[`[`])
          else insideBlock(tok, close, _.isInstanceOf[`{`])
        //          insideBlock(tok, close, _.isInstanceOf[`{`])
        val indent = leftOwner match {
          case _: Pat => Num(0) // Indentation already provided by case.
          case x if isDefnSite(x) && !x.isInstanceOf[Type.Apply] => Num(0)
          case _ => Num(4)
        }

        val singleArgument = args.length == 1

        // TODO(olafur) overfitting unit tests?
        val singleLine =
          if (isBracket) {
            if (singleArgument)
              SingleLineBlock(close, exclude, killInlineComments = false)
            else SingleLineBlock(close)
          } else {
            if (singleArgument) NoPolicy
            else SingleLineBlock(close, exclude)
          }
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
          singleArgument || exclude.nonEmpty ||
          charactersInside <= style.maxColumn

        // TODO(olafur) ignoreIf: State => Boolean?
        val expirationToken: Token =
          if (isDefnSite(leftOwner)) defnSiteLastToken(leftOwner)
          else rhsOptimalToken(leftTok2tok(close))

        val optimalToken = Some(expirationToken)

        val singleLineExpiration: Token =
          if (isBracket) expirationToken
          else nextNonComment(tok).right
        Seq(
            Split(modification,
                  0,
                  policy = singleLine,
                  ignoreIf = !fitsOnOneLine || isConfigStyle,
                  optimalAt = optimalToken)
            // TODO(olafur) allow style to specify indent here?
              .withIndent(indent, singleLineExpiration, Left),
            Split(newlineModification,
                  (1 + nestedPenalty + lhsPenalty) * bracketMultiplier,
                  policy = singleLine,
                  ignoreIf = !fitsOnOneLine || isConfigStyle,
                  optimalAt = optimalToken)
              .withIndent(indent, singleLineExpiration, Left),
            // TODO(olafur) singleline per argument!
            Split(modification,
                  (2 + lhsPenalty) * bracketMultiplier,
                  policy = oneArgOneLine,
                  ignoreIf = singleArgument || isConfigStyle,
                  optimalAt = optimalToken)
              .withIndent(StateColumn, close, Right),
            Split(Newline,
                  (3 + nestedPenalty + lhsPenalty) * bracketMultiplier,
                  policy = oneArgOneLine,
                  ignoreIf = singleArgument || isConfigStyle,
                  optimalAt = optimalToken).withIndent(indent, close, Left),
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
        val singleLineToEndOfArg: Policy =
          endOfArgument.map(expire => SingleLineBlock(expire.left))
            .getOrElse(NoPolicy)
        val optimalToken = endOfArgument.map(_.left)
        Seq(
            Split(Space, 0, optimalAt = optimalToken)
              .withPolicy(singleLineToEndOfArg),
            Split(Newline, 1, ignoreIf = rhsIsAttachedComment)
        )
      case FormatToken(_, _: `;`, _) =>
        Seq(
            Split(NoSplit, 0)
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
          // Only split if rhs is an application
          // TODO(olafur) counterexample? For example a.b[C]
          isOpenApply(next(next(tok)).right) && !left.isInstanceOf[`_ `] &&
          // TODO(olafur) optimize
          !parents(rightOwner).exists(_.isInstanceOf[Import]) =>
        val nestedPenalty = nestedSelect(rightOwner) + nestedApplies(leftOwner)
        val isLhsOfApply = rightOwner.parent.exists {
          case apply: Term.Apply => apply.fun.tokens.contains(left)
          case _ => false
        }
        val noApplyPenalty =
          if (!isLhsOfApply) 1
          else 0
        Seq(
            Split(NoSplit, 0),
            Split(Newline, 1 + nestedPenalty + noApplyPenalty)
              .withIndent(2, dot, Left)
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
        val lastToken = defnTemplate(rightOwner).map(templateCurly)
          .getOrElse(rightOwner.tokens.last)
        Seq(
            Split(Space, 0).withPolicy(SingleLineBlock(lastToken)),
            Split(Newline, 1)
        )
      case tok@FormatToken(_, right: `with`, _)
          if rightOwner.isInstanceOf[Template] =>
        val template = rightOwner.asInstanceOf[Template]
        // TODO(olafur) is this correct?
        val expire = templateCurly(template)
        Seq(
            Split(Space, 0),
            Split(Newline, 1).withPolicy(Policy({
              // Force template to be multiline.
              case d@Decision(FormatToken(open: `{`, _, _), splits)
                  if childOf(template, owners(open)) =>
                d.copy(splits = splits.filter(_.modification.isNewline))
            }, expire.end))
        )
      // If
      case FormatToken(open: `(`, _, _) if leftOwner.isInstanceOf[Term.If] =>
        val close = matchingParentheses(hash(open))
        val penalizeNewlines = penalizeNewlineByNesting(open, close)
        Seq(
            Split(NoSplit, 0).withIndent(StateColumn, close, Left)
              .withPolicy(penalizeNewlines)
        )
      case FormatToken(close: `)`, right, between)
          if leftOwner.isInstanceOf[Term.If] =>
        val owner = leftOwner.asInstanceOf[Term.If]
        val expire = owner.thenp.tokens.last
        val rightIsOnNewLine = newlinesBetween(between) > 0
        // Inline comment attached to closing `)`
        val attachedComment = !rightIsOnNewLine && isInlineComment(right)
        val newlineModification: Modification =
          if (attachedComment)
            Space // Inline comment will force newline later.
          else Newline
        Seq(
            Split(Space, 0, ignoreIf = rightIsOnNewLine || attachedComment)
              .withIndent(2, expire, Left).withPolicy(SingleLineBlock(expire)),
            Split(newlineModification, 1).withIndent(2, expire, Left)
        )
      case tok@FormatToken(_: `}`, els: `else`, _) =>
        Seq(
            Split(Space, 0)
        )
      case tok@FormatToken(_, els: `else`, _) =>
        // According to scala-js style guide, no single-line if else.
        Seq(
            Split(Newline, 0)
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
      case FormatToken(_, asterisk: Ident, _) if asterisk.code == "*" &&
          rightOwner.isInstanceOf[Type.Arg.Repeated] =>
        Seq(
            Split(NoSplit, 0)
        )

      // ApplyInfix.
      case tok@FormatToken(left: Ident, _, _) if isBoolOperator(left) =>
        Seq(
            Split(Space, 0),
            Split(Newline, 1)
        )
      case tok@FormatToken(_: Ident | _: Literal | _: Interpolation.End,
                           _: Ident | _: Literal | _: Xml.End,
                           _) =>
        Seq(
            Split(Space, 0)
        )
      case FormatToken(open: `(`, right, _)
          if leftOwner.isInstanceOf[Term.ApplyInfix] =>
        val close = matchingParentheses(hash(open))
        Seq(
            Split(NoSplit, 0).withIndent(4, close, Left)
        )
      case FormatToken(_, open: `(`, _)
          if rightOwner.isInstanceOf[Term.ApplyInfix] =>
        val close = matchingParentheses(hash(open))
        Seq(
            Split(Space, 0, optimalAt = Some(close))
              .withPolicy(SingleLineBlock(close)),
            Split(Newline, 1, optimalAt = Some(close))
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
        val lastToken = owner.body.tokens.filter {
          case _: Whitespace | _: Comment => false
          case _ => true
        }.lastOption
          .getOrElse(arrow) // edge case, if body is empty expire on arrow.

        Seq(
            // Either everything fits in one line or break on =>
            Split(Space, 0).withPolicy(SingleLineBlock(lastToken)),
            Split(Space, 1).withPolicy(Policy({
              case Decision(t@FormatToken(`arrow`, right, between), s)
//                   TODO(olafur) any other corner cases?
                  if !right.isInstanceOf[`{`] &&
                  !isAttachedComment(right, between) =>
                Decision(t, s.filter(_.modification.isNewline))
            }, expire = lastToken.end))
              .withIndent(2, lastToken, Left) // case body indented by 2.
              .withIndent(2, arrow, Left) // cond body indented by 4.
        )
      case tok@FormatToken(_, cond: `if`, _)
          if rightOwner.isInstanceOf[Case] =>
        val owner = rightOwner.asInstanceOf[Case]
        val lastToken = getArrow(owner)
        val penalizeNewlines = penalizeNewlineByNesting(cond, lastToken)
        Seq(
            Split(Space, 0, policy = penalizeNewlines),
            Split(Newline, 1, policy = penalizeNewlines)
        )
      case tok@FormatToken(arrow: `=>`, right, between)
          if leftOwner.isInstanceOf[Case] =>
        Seq(
            Split(Space, 0),
            Split(
                Newline(gets2x = false, hasIndent = rhsIsCommentedOut(tok)), 1)
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
        val lastToken = findSiblingGuard(
            leftOwner.asInstanceOf[Enumerator.Generator]).map(_.tokens.last)
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
      case FormatToken(_: `_ `, asterisk: Ident, _) if asterisk.code == "*" &&
          prev(formatToken).left.isInstanceOf[`:`] =>
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
          splits.filter(_.modification.isNewline)
        case _ => splits
      }
    })

  private implicit def int2num(n: Int): Num = Num(n)
}
