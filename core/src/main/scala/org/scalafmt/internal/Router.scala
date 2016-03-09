package org.scalafmt.internal

import org.scalafmt.Error.CaseMissingArrow
import org.scalafmt.Error.UnexpectedTree
import org.scalafmt.ScalaStyle

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.implicitConversions
import scala.meta.Tree
import scala.meta.internal.ast.Case
import scala.meta.internal.ast.Ctor
import scala.meta.internal.ast.Decl
import scala.meta.internal.ast.Defn
import scala.meta.internal.ast.Enumerator
import scala.meta.internal.ast.Import
import scala.meta.internal.ast.Mod
import scala.meta.internal.ast.Pat
import scala.meta.internal.ast.Pkg
import scala.meta.internal.ast.Template
import scala.meta.internal.ast.Term
import scala.meta.internal.ast.Type
import scala.meta.prettyprinters.Structure
import scala.meta.tokens.Token

// Too many to import individually.
import scala.meta.tokens.Token._

/**
  * Assigns splits to format tokens.
  */
class Router(style: ScalaStyle,
             tree: Tree,
             tokens: Array[FormatToken],
             matchingParentheses: Map[TokenHash, Token],
             statementStarts: Map[TokenHash, Tree],
             ownersMap: Map[TokenHash, Tree]) extends ScalaFmtLogger {
  @inline
  def owners(token: Token): Tree = ownersMap(hash(token))

  /*
   * The tokens on the left hand side of Pkg
   *
   * For example Set(org, ., scalafmt) in:
   *
   * package org.scalafmt
   *
   * import foo.bar
   * ...
   *
   */
  val packageTokens: Set[Token] = {
    val result = new mutable.SetBuilder[Token, Set[Token]](Set.empty[Token])
    tree.collect {
      case p: Pkg => result ++= p.ref.tokens
    }
    result.result()
  }

  private val leftTok2tok: Map[Token, FormatToken] =
    tokens.map(t => t.left -> t).toMap
  private val tok2idx: Map[FormatToken, Int] = tokens.zipWithIndex.toMap
  // TODO(olafur) replace cache with array of list[split]
  private val cache = mutable.Map.empty[FormatToken, Seq[Split]]

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
//        // Argument block
//      case FormatToken(_: `,`, open: `{`, _)
//        if leftOwner.isInstanceOf[Term.Apply] => Seq(
//        Split(Space, 0)
//      )
      case FormatToken(open: `{`, _, _)
          if parents(leftOwner).exists(_.isInstanceOf[Import]) ||
          leftOwner.isInstanceOf[Term.Interpolate] =>
        Seq(
            Split(NoSplit, 0)
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
      // { ... } Blocks
      case tok@FormatToken(open: `{`, right, between) =>
        val nl = Newline(
            newlinesBetween(between) > 1 || gets2x(nextNonComment(tok)),
            rhsIsCommentedOut(tok))
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
      case tok: FormatToken if !isDocstring(tok.left) && gets2x(tok) =>
        Seq(
            Split(Newline2x, 0)
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
        val newline: Modification =
          if ((gets2x(nextNonComment(tok)) || oldNewlines > 1) &&
              !isDocstring(left)) Newline2x
          else Newline
        Seq(
            Split(newline, 0)
        )

      // TODO(olafur) more general?
      case FormatToken(_: `]`, right: `(`, _) =>
        Seq(
            Split(NoSplit, 0)
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
      case tok@FormatToken(left, open: `[`, _)
          if rightOwner.isInstanceOf[Term.ApplyType] ||
          rightOwner.isInstanceOf[Defn.Def] ||
          rightOwner.isInstanceOf[Decl.Def] ||
          leftOwner.isInstanceOf[Type.Name] =>
        Seq(
            Split(NoSplit, 0)
        )
      // Opening ( with no leading space.
      case FormatToken(left, open: `(`, _) if (rightOwner match {
            case _: Term.Apply | _: Decl.Def | _: Defn.Def => true
            case _ if rightOwner.parent.exists(_.isInstanceOf[Defn.Class]) =>
              true
            case _ => false
          }) =>
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
      //     TODO(olafur) Naive match, can be 1) comment between 2) abstract decl.
      case tok@FormatToken(d: `def`, name: Ident, _) =>
        Seq(
            Split(Space, 0).withIndent(4, defnSiteLastToken(leftOwner), Left)
        )
      case tok@FormatToken(e: `=`, _, _) if leftOwner.isInstanceOf[Defn.Def] =>
        val expire = leftOwner.asInstanceOf[Defn.Def].body.tokens.last
        Seq(
            Split(Space, 0, policy = SingleLineBlock(expire)),
            Split(Newline, 0).withIndent(2, expire, Left)
        )
      case tok@FormatToken(_, open: `[`, _)
          if rightOwner.isInstanceOf[Defn.Def] =>
        Seq(
            Split(NoSplit, 0)
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
          case t: Term.Tuple => t -> t.elements
          case t: Type.Apply => t.tpe -> t.args
          // TODO(olafur) flatten correct? Filter by this () section?
          case t: Defn.Def => t.name -> t.paramss.flatten
          case t: Decl.Def => t.name -> t.paramss.flatten
          case t: Defn.Class => t.name -> t.ctor.paramss.flatten
          case t: Ctor.Primary => t.name -> t.paramss.flatten
          case x =>
            logger.error(s"""Unknown tree
                 |${log((x.parent.get))}
                 |${isDefnSite(leftOwner)}""".stripMargin)
            ???
        }
        val close = matchingParentheses(hash(open))
        // In long sequence of select/apply, we penalize splitting on
        // parens furthest to the right.
        val lhsPenalty = treeDepth(lhs)

        val bracketPenalty = open match {
          case _: `[` => 1
          case _ => 0
        }
        val nestedPenalty = nestedApplies(leftOwner)

        val exclude = insideBlock(tok, close)
        val indent = leftOwner match {
          case _: Pat => Num(0) // Indentation already provided by case.
          // TODO(olafur) This is an odd rule, when is it wrong?
          case x if isDefnSite(x) => Num(0)
          case _ => Num(4)
        }
        val singleArgument = args.length == 1

        val singleLine = // Don't force single line policy if only one argument.
          if (singleArgument) NoPolicy
          else SingleLineBlock(close, exclude)
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

        val charactersInside = (close.start - open.end) - 2
        val fitsOnOneLine =
          singleArgument || exclude.nonEmpty ||
          charactersInside <= style.maxColumn

        // TODO(olafur) ignoreIf: State => Boolean?
        val optimalToken =
          if (isDefnSite(leftOwner)) Some(defnSiteLastToken(leftOwner))
          else Some(rhsOptimalToken(leftTok2tok(close)))

        Seq(
            Split(modification,
                  0,
                  policy = singleLine,
                  ignoreIf = !fitsOnOneLine || isConfigStyle,
                  optimalAt = optimalToken)
            // TODO(olafur) allow style to specify indent here?
              .withIndent(indent, nextNonComment(tok).right, Left),
            Split(Newline,
                  1 + nestedPenalty + lhsPenalty + bracketPenalty,
                  policy = singleLine,
                  ignoreIf = !fitsOnOneLine || isConfigStyle,
                  optimalAt = optimalToken).withIndent(indent, right, Left),
            // TODO(olafur) singleline per argument!
            Split(modification,
                  2 + lhsPenalty + bracketPenalty,
                  policy = oneArgOneLine,
                  ignoreIf = singleArgument || isConfigStyle,
                  optimalAt = optimalToken)
              .withIndent(StateColumn, close, Right),
            Split(Newline,
                  3 + nestedPenalty + lhsPenalty + bracketPenalty,
                  policy = oneArgOneLine,
                  ignoreIf = singleArgument || isConfigStyle,
                  optimalAt = optimalToken).withIndent(indent, close, Left),
            Split(Newline,
                  0,
                  policy = configStyle,
                  ignoreIf = !isConfigStyle).withIndent(indent, close, Right)
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
            Split(mod, 1).withIndent(2, expire, Left)
        )
      case tok@FormatToken(left, dot: `.`, _)
          if rightOwner.isInstanceOf[Term.Select] &&
          // Only split if rhs is an application
          // TODO(olafur) counterexample? For example a.b[C]
          next(next(tok)).right.isInstanceOf[`(`] &&
          !left.isInstanceOf[`_ `] && // TODO(olafur) optimize
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
      case FormatToken(_, right: `extends`, _)
          if rightOwner.isInstanceOf[Defn.Class] =>
        val owner = rightOwner.asInstanceOf[Defn.Class]
        val lastToken = templateCurly(owner.templ)
        // TODO(olafur) use more precise policy?
        val penalizeNewlines = penalizeNewlineByNesting(right, lastToken)
        Seq(
            Split(Space, 0).withPolicy(penalizeNewlines),
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
      case FormatToken(_: `(`, _: `(` | _: `{`, _) =>
        Seq(
            Split(NoSplit, 0)
        )

      // Type variance
      case tok@FormatToken(_: Ident, _: Ident, _)
          if isTypeVariant(leftOwner) =>
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

      // Pattern matching
      case tok@FormatToken(_, open: `(`, _)
          if rightOwner.isInstanceOf[Pat.Extract] =>
        Seq(
            Split(NoSplit, 0)
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
      case tok@FormatToken(arrow: `=>`, _, _)
          if leftOwner.isInstanceOf[Case] =>
        Seq(
            Split(Space, 0),
            Split(Newline, 1)
        )
      // Inline comment
      case FormatToken(_, c: Comment, between) if c.code.startsWith("//") =>
        Seq(Split(newlines2Modification(between), 0))
      // Commented out code should stay to the left
      case FormatToken(c: Comment, _, between) if c.code.startsWith("//") =>
        Seq(Split(Newline, 0))
      case tok@FormatToken(_, c: Comment, _) =>
        val newline: Modification =
          if (isDocstring(c) || gets2x(next(tok)) ||
              newlinesBetween(tok.between) > 1)
            Newline2x
          else Newline
        Seq(
            Split(newline, 0)
        )
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
      // Curried functions
      case FormatToken(_: `)`, _: `(`, _) =>
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

  /**
   * Assigns possible splits to a FormatToken.
   *
   * The FormatToken can be considered as a node in a graph and the
   * splits as edges. Given a format token (a node in the graph), Route
   * determines which edges lead out from the format token.
   */
  def getSplitsMemo(formatToken: FormatToken): Seq[Split] =
    cache.getOrElseUpdate(formatToken, {
      val splits = getSplits(formatToken)
      formatToken match {
        // TODO(olafur) refactor into "global policy"
        // Only newlines after inline comments.
        case FormatToken(c: Comment, _, _) if c.code.startsWith("//") =>
          splits.filter(_.modification.isNewline)
        case _ => splits
      }
    })

  def isDocstring(token: Token): Boolean = {
    token.isInstanceOf[Comment] && token.code.startsWith("/**")
  }

  @tailrec
  final def findFirst(start: FormatToken, end: Token)(
      f: FormatToken => Boolean): Option[FormatToken] = {
    if (start.left.start < end.start) None
    else if (f(start)) Some(start)
    else {
      val next_ = next(start)
      if (next_ == start) None
      else findFirst(next_, end)(f)
    }
  }

  @tailrec
  final def nextNonComment(curr: FormatToken): FormatToken = {
    if (!curr.right.isInstanceOf[Comment]) curr
    else {
      val tok = next(curr)
      if (tok == curr) curr
      else nextNonComment(tok)
    }
  }

  def gets2x(tok: FormatToken): Boolean = {
    if (!statementStarts.contains(hash(tok.right))) false
    else if (packageTokens.contains(tok.left) &&
             !packageTokens.contains(tok.right)) true
    else {
      val rightOwner = statementStarts(hash(tok.right))
      if (!rightOwner.tokens.headOption.contains(tok.right)) false
      else
        rightOwner match {
          case _: Defn.Def | _: Pkg.Object | _: Defn.Class | _: Defn.Object |
              _: Defn.Trait =>
            true
          case _ => false
        }
    }
  }

  def OneArgOneLineSplit(open: Delim)(implicit line: sourcecode.Line): Policy = {
    val expire = matchingParentheses(hash(open))
    Policy({
      // Newline on every comma.
      case Decision(t@FormatToken(comma: `,`, right, between), splits)
          if owners(open) == owners(comma) &&
          // TODO(olafur) what the right { decides to be single line?
          !right.isInstanceOf[`{`] &&
          // If comment is bound to comma, see unit/Comment.
          (!right.isInstanceOf[Comment] ||
              between.exists(_.isInstanceOf[`\n`])) =>
        Decision(t, splits.filter(_.modification.isNewline))
    }, expire.end)
  }

  def penalizeNewlineByNesting(from: Token, to: Token)(
      implicit line: sourcecode.Line): Policy = {
    val range = Range(from.start, to.end).inclusive
    Policy({
      case Decision(t, s) if range.contains(t.right.start) =>
        // TODO(olafur) hack, overfitting unit test. Use spans, see
        // http://journal.stuffwithstuff.com/2015/09/08/the-hardest-program-ive-ever-written/#12
        val nonBoolPenalty =
          if (isBoolOperator(t.left)) 0
          else 1
        val penalty =
          nestedSelect(owners(t.left)) + nestedApplies(owners(t.right)) + nonBoolPenalty
        Decision(t, s.map {
          case split if split.modification.isNewline =>
            split.withPenalty(penalty)
          case x => x
        })
    }, to.end)
  }

  /**
   * How many parents of tree are Term.Apply?
   */
  def nestedApplies(tree: Tree): Int = {
    // TODO(olafur) optimize?
    tree.parent.fold(0) {
      case parent@(_: Term.Apply | _: Term.ApplyInfix) =>
        1 + nestedApplies(parent)
      case parent => nestedApplies(parent)
    }
  }

  // TODO(olafur) abstract with [[NestedApplies]]

  def nestedSelect(tree: Tree): Int = {
    tree.parent.fold(0) {
      case parent: Term.Select => 1 + nestedSelect(parent)
      case parent => nestedSelect(parent)
    }
  }

  def SingleLineBlock(expire: Token, exclude: Set[Range] = Set.empty)(
      implicit line: sourcecode.Line): Policy = {
    Policy({
      case Decision(tok, splits)
          if exclude.forall(!_.contains(tok.left.start)) &&
          !tok.right.isInstanceOf[EOF] && tok.right.end <= expire.end =>
        Decision(tok, splits.filterNot(_.modification.isNewline))
    }, expire.end, noDequeue = exclude.isEmpty)
  }

  def insideBlock(start: FormatToken, end: Token): Set[Range] = {
    var inside = false
    val result = new mutable.SetBuilder[Range, Set[Range]](Set.empty[Range])
    var curr = start
    while (curr.left != end) {
      if (curr.left.isInstanceOf[`{`]) {
        inside = true
        result += Range(
            curr.left.start, matchingParentheses(hash(curr.left)).end)
        curr = leftTok2tok(matchingParentheses(hash(curr.left)))
      } else {
        curr = next(curr)
      }
    }
    result.result()
  }

  def prev(tok: FormatToken): FormatToken = {
    val i = tok2idx(tok)
    if (i == 0) tok
    else tokens(i - 1)
  }

  def next(tok: FormatToken): FormatToken = {
    val i = tok2idx(tok)
    if (i == tokens.length - 1) tok
    else tokens(i + 1)
  }

  def isInlineComment(token: Token): Boolean =
    token match {
      case c: Comment => c.code.startsWith("//")
      case _ => false
    }

  def newlines2Modification(between: Vector[Whitespace]): Modification =
    newlinesBetween(between) match {
      case 0 => Space
      case x => Newline(x == 2, endsWithNoIndent(between))
    }

  // TODO(olafur) calculate this once inside getSplits.

  def newlinesBetween(between: Vector[Whitespace]): Int =
    between.count(_.isInstanceOf[`\n`])

  def defnTemplate(tree: Tree): Option[Template] =
    tree match {
      case t: Defn.Object => Some(t.templ)
      case t: Defn.Class => Some(t.templ)
      case t: Defn.Trait => Some(t.templ)
      case t: Pkg.Object => Some(t.templ)
      case _ => None
    }

  @tailrec
  final def rhsOptimalToken(start: FormatToken): Token =
    start.right match {
      case _: `,` | _: `(` | _: `)` | _: `]` | _: `;` | _: `=>`
          if next(start) != start &&
          !owners(start.right).tokens.headOption.contains(start.right) =>
        rhsOptimalToken(next(start))
      case _ => start.left
    }

  def rhsIsCommentedOut(formatToken: FormatToken): Boolean =
    formatToken.right.isInstanceOf[Comment] &&
    endsWithNoIndent(formatToken.between)

  def endsWithNoIndent(between: Vector[Whitespace]): Boolean =
    between.lastOption.exists(_.isInstanceOf[`\n`])

  def isBoolOperator(token: Token): Boolean =
    token.code match {
      case "||" | "&&" => true
      case _ => false
    }

  def identModification(ident: Ident): Modification = {
    val lastCharacter = ident.code.last
    if (Character.isLetterOrDigit(lastCharacter) || lastCharacter == '`')
      NoSplit
    else Space
  }

  def getArrow(caseStat: Case): Token =
    caseStat.tokens.find(t => t.isInstanceOf[`=>`] && owners(t) == caseStat)
      .getOrElse(throw CaseMissingArrow(caseStat))

  // TODO(olafur) scala.meta should make this easier.

  def findSiblingGuard(
      generator: Enumerator.Generator): Option[Enumerator.Guard] = {
    for {
      parent <- generator.parent if parent.isInstanceOf[Term.For] ||
               parent.isInstanceOf[Term.ForYield]
      sibling <- {
        val enums = parent match {
          case p: Term.For => p.enums
          case p: Term.ForYield => p.enums
        }
        enums.zip(enums.tail).collectFirst {
          case (`generator`, guard: Enumerator.Guard) => guard
        }
      }
    } yield sibling
  }

  /**
    * Calculates depth to deepest child in tree.
    */
  // TODO(olafur) inefficient, precalculate?

  def treeDepth(tree: Tree): Int =
    if (tree.children.isEmpty) 0
    else 1 + tree.children.map(treeDepth).max

  def templateCurly(template: Template): Token = {
    template.tokens.find(_.isInstanceOf[`{`]).getOrElse(template.tokens.last)
  }

  def isAttachedComment(token: Token, between: Vector[Whitespace]) =
    isInlineComment(token) && newlinesBetween(between) == 0

  def isDefnSite(tree: Tree): Boolean =
    tree match {
      case _: Decl.Def | _: Defn.Def | _: Defn.Class => true
      case x: Ctor.Primary if x.parent.exists(_.isInstanceOf[Defn.Class]) =>
        true
      case _ => false
    }

  def isCallSite(tree: Tree): Boolean =
    tree match {
      case _: Term.Apply | _: Pat.Extract | _: Pat.Tuple | _: Term.Tuple |
          _: Term.ApplyType =>
        true
      case _ => false
    }

  def defnSiteLastToken(tree: Tree): Token = {
    tree.tokens.find(t => t.isInstanceOf[`=`] && owners(t) == tree)
      .getOrElse(tree.tokens.last)
  }

  def isModPrivateProtected(tree: Tree): Boolean =
    tree match {
      case _: Mod.Private | _: Mod.Protected => true
      case _ => false
    }

  def isTypeVariant(tree: Tree): Boolean =
    tree match {
      case _: Mod.Contravariant | _: Mod.Covariant => true
      case _ => false
    }

  @tailrec
  final def startsStatement(tok: FormatToken): Boolean = {
    statementStarts.contains(hash(tok.right)) ||
    (tok.right.isInstanceOf[Comment] &&
        tok.between.exists(_.isInstanceOf[`\n`]) && startsStatement(next(tok)))
  }
  // Used for convenience when calling withIndent.

  private implicit def int2num(n: Int): Num = Num(n)
}
