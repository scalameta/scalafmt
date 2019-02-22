package org.scalafmt.internal

import scala.annotation.tailrec
import scala.collection.mutable
import scala.meta.Case
import scala.meta.Ctor
import scala.meta.Decl
import scala.meta.Defn
import scala.meta.Import
import scala.meta.Name
import scala.meta.Pat
import scala.meta.Pkg
import scala.meta.Template
import scala.meta.Term
import scala.meta.Tree
import scala.meta.Type
import scala.meta.prettyprinters.Structure
import scala.meta.tokens.Token
import scala.meta.tokens.Token._
import org.scalafmt.Error.CaseMissingArrow
import org.scalafmt.config.{DanglingExclude, ScalafmtConfig, VerticalMultiline}
import org.scalafmt.internal.ExpiresOn.Left
import org.scalafmt.internal.ExpiresOn.Right
import org.scalafmt.internal.Length.Num
import org.scalafmt.internal.Policy.NoPolicy
import org.scalafmt.util.CtorModifier
import org.scalafmt.util.StyleMap
import org.scalafmt.util.TokenOps
import org.scalafmt.util.TreeOps
import org.scalafmt.util.Whitespace
import org.scalafmt.util.Modifier
import org.scalafmt.util.RightParenOrBracket
import org.scalameta.logger
import scala.meta.Init

/**
  * Helper functions for generating splits/policies for a given tree.
  */
class FormatOps(val tree: Tree, val initStyle: ScalafmtConfig) {
  val runner = initStyle.runner
  import TokenOps._
  import TreeOps._
  implicit val dialect = initStyle.runner.dialect
  val tokens: Array[FormatToken] = FormatToken.formatTokens(tree.tokens)
  val ownersMap = getOwners(tree)
  val statementStarts = getStatementStarts(tree)
  val dequeueSpots = getDequeueSpots(tree) ++ statementStarts.keys
  val matchingParentheses: Map[TokenHash, Token] = getMatchingParentheses(
    tree.tokens)
  val styleMap =
    new StyleMap(tokens, initStyle, ownersMap, matchingParentheses)
  private val vAlignDepthCache = mutable.Map.empty[Tree, Int]
  // Maps token to number of non-whitespace bytes before the token's position.
  private final val nonWhitespaceOffset: Map[Token, Int] = {
    val resultB = Map.newBuilder[Token, Int]
    var curr = 0
    tree.tokens.foreach {
      case t =>
        resultB += (t -> curr)
        if (!t.is[Whitespace]) {
          curr += (t.end - t.start)
        }

    }
    resultB.result()
  }

  val (forceConfigStyle, emptyQueueSpots) = getForceConfigStyle

  @inline def matching(token: Token): Token = matchingParentheses(hash(token))

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
  val (packageTokens, importTokens, argumentStarts, optionalNewlines) = {
    val packages = Set.newBuilder[Token]
    val imports = Set.newBuilder[Token]
    val arguments = mutable.Map.empty[TokenHash, Tree]
    val optional = mutable.Set.empty[TokenHash]
    def add(tree: Tree): Unit = {
      if (tree.tokens.nonEmpty && !arguments.contains(hash(tree.tokens.head))) {
        arguments += hash(tree.tokens.head) -> tree
      }
    }
    def addOptional(tree: Tree): Unit =
      tree.tokens.headOption.foreach(x => optional += hash(x))
    def iter(tree: Tree): Unit = {
      tree match {
        case p: Pkg => packages ++= p.ref.tokens
        case i: Import => imports ++= i.tokens
        case t: Term => add(t)
        case t: Term.Param =>
          add(t)
          t.mods.foreach(addOptional)
          addOptional(t.name)
        case _ =>
      }
      tree.children.foreach(iter)
    }
    iter(tree)
    (packages.result(), imports.result(), arguments.toMap, optional)
  }

  object `:owner:` {
    def unapply(tok: Token): Option[(Token, Tree)] =
      ownersMap.get(hash(tok)).map(tree => tok -> tree)
  }

  object `:chain:` {
    def unapply(tok: Token): Option[(Token, Vector[Term.Select])] = {
      val rightOwner = owners(tok)
      val openApply = next(leftTok2tok(tok)).right
      def startsOpenApply =
        isOpenApply(
          openApply,
          includeCurly = initStyle.includeCurlyBraceInSelectChains,
          includeNoParens = initStyle.includeNoParensInSelectChains)
      def isChildOfImport =
        parents(rightOwner).exists(_.is[Import])
      def isShortCurlyChain(chain: Vector[Term.Select]): Boolean =
        chain.length == 2 && {
          !(for {
            child <- chain.lastOption
            parent <- child.parent
          } yield isChainApplyParent(parent, child)).getOrElse(false)
        }

      rightOwner match {
        case t: Term.Select if startsOpenApply && !isChildOfImport =>
          val chain = getSelectChain(t)
          if (openApply.is[LeftBrace] && isShortCurlyChain(chain)) None
          else Some(tok -> chain)
        case _ => None
      }
    }
  }

  lazy val leftTok2tok: Map[Token, FormatToken] = {
    val result = Map.newBuilder[Token, FormatToken]
    result.sizeHint(tokens.length)
    tokens.foreach(t => result += t.left -> t)
    result += (tokens.last.right -> tokens.last)
    result.result()
  }

  lazy val tok2idx: Map[FormatToken, Int] = tokens.zipWithIndex.toMap

  def prev(tok: Token): Token = prev(leftTok2tok(tok)).right
  def prev(tok: FormatToken): FormatToken = {
    val i = tok2idx(tok)
    if (i == 0) tok
    else tokens(i - 1)
  }

  def next(tok: Token): Token = leftTok2tok(tok).right
  def next(tok: FormatToken): FormatToken = {
    val i = tok2idx(tok)
    if (i == tokens.length - 1) tok
    else tokens(i + 1)
  }

  @tailrec
  final def findFirst(start: FormatToken, end: Token)(
      f: FormatToken => Boolean): Option[FormatToken] = {
    if (start.left.start > end.start) None
    else if (f(start)) Some(start)
    else {
      val next_ = next(start)
      if (next_ == start) None
      else findFirst(next_, end)(f)
    }
  }

  @tailrec
  final def prevNonCommentWithCount(
      curr: FormatToken,
      accum: Int = 0): (Int, FormatToken) = {
    if (!curr.left.is[Comment]) accum -> curr
    else {
      val tok = prev(curr)
      if (tok == curr) accum -> curr
      else prevNonCommentWithCount(tok, accum + 1)
    }
  }
  def prevNonComment(curr: FormatToken): FormatToken =
    prevNonCommentWithCount(curr)._2

  @tailrec
  final def nextNonCommentWithCount(
      curr: FormatToken,
      accum: Int = 0): (Int, FormatToken) = {
    if (!curr.right.is[Comment]) accum -> curr
    else {
      val tok = next(curr)
      if (tok == curr) accum -> curr
      else nextNonCommentWithCount(tok, accum + 1)
    }
  }
  def nextNonComment(curr: FormatToken): FormatToken =
    nextNonCommentWithCount(curr)._2

  @tailrec
  final def rhsOptimalToken(start: FormatToken): Token = start.right match {
    case Comma() | LeftParen() | RightParen() | RightBracket() | Semicolon() |
        RightArrow() | Equals()
        if next(start) != start &&
          !startsNewBlock(start.right) &&
          newlinesBetween(start.between) == 0 =>
      rhsOptimalToken(next(start))
    case _ => start.left
  }

  final def startsNewBlock(t: Token): Boolean =
    owners(t).tokens.headOption.contains(t)

  /**
    * js.native is very special in Scala.js.
    *
    * Context: https://github.com/scalameta/scalafmt/issues/108
    */
  def isJsNative(jsToken: Token): Boolean = {
    initStyle.newlines.neverBeforeJsNative && jsToken.syntax == "js" &&
    owners(jsToken).parent.exists(
      _.show[Structure].trim == """Term.Select(Term.Name("js"), Term.Name("native"))""")
  }

  def isTripleQuote(token: Token): Boolean = token.syntax.startsWith("\"\"\"")

  def isMarginizedString(token: Token): Boolean = token match {
    case start @ Interpolation.Start() =>
      val end = matchingParentheses(hash(start))
      val afterEnd = next(leftTok2tok(end))
      afterEnd.left.syntax == "." && afterEnd.right.syntax == "stripMargin"
    case string: Constant.String =>
      string.syntax.startsWith("\"\"\"") && {
        val afterString = next(leftTok2tok(string))
        afterString.left.syntax == "." &&
        afterString.right.syntax == "stripMargin"
      }
    case _ => false
  }

  @tailrec
  final def startsStatement(tok: FormatToken): Boolean = {
    statementStarts.contains(hash(tok.right)) ||
    (tok.right.is[Comment] && tok.between.exists(_.is[LF]) &&
    startsStatement(next(tok)))
  }

  def parensRange(open: Token): Range =
    Range(open.start, matchingParentheses(hash(open)).end)

  def getExcludeIf(
      end: Token,
      cond: Token => Boolean = _.is[RightBrace]): Set[Range] = {
    if (cond(end)) // allow newlines in final {} block
      Set(Range(matchingParentheses(hash(end)).start, end.end))
    else Set.empty[Range]
  }

  def skipUnindent(token: Token): Boolean = {
    token.is[LeftParen] && {
      val owner = owners(token)
      val isSuperfluous = isSuperfluousParenthesis(token, owner)
      isSuperfluous && (owner match {
        case _: Term.ApplyUnary | _: Term.Block => false
        case _ => true
      })

    }
  }

  def insideBlock(
      start: FormatToken,
      end: Token,
      matches: Token => Boolean): Set[Token] = {
    val result = Set.newBuilder[Token]
    var prev = start
    var curr = next(start)

    def goToMatching(): Unit = {
      val close = matchingParentheses(hash(curr.left))
      curr = leftTok2tok(close)
    }

    while (curr.left.start < end.start && curr != prev) {
      if (matches(curr.left)) {
        result += curr.left
        goToMatching()
      } else {
        prev = curr
        curr = next(curr)
      }
    }
    result.result()
  }

  def defnSiteLastToken(open: Token, tree: Tree): Token = {
    tree match {
      // TODO(olafur) scala.meta should make this easier.
      case procedure: Defn.Def
          if procedure.decltpe.isDefined &&
            procedure.decltpe.get.tokens.isEmpty =>
        procedure.body.tokens.find(_.is[LeftBrace])
      case Defn.Def(_, _, _, _, _, b @ Term.Block(_)) =>
        b.tokens.headOption
      case _: Ctor.Primary =>
        leftTok2tok(matchingParentheses(hash(open))) match {
          // This is a terrible terrible hack. Please consider removing this.
          // The RightParen() LeftBrace() pair is presumably a ") {" combination
          // at a class definition
          case FormatToken(RightParen(), b @ LeftBrace(), _) => Some(b)
          case _ => Some(matchingParentheses(hash(open)))
        }
      case _ =>
        tree.tokens.find(t => t.is[Equals] && owners(t) == tree)
    }
  }.getOrElse(tree.tokens.last)

  def OneArgOneLineSplit(open: Token, noTrailingCommas: Boolean = false)(
      implicit line: sourcecode.Line): Policy = {
    // TODO(olafur) clear queue between arguments, they are independent.
    val expire = matchingParentheses(hash(open))
    Policy(
      {
        case d @ Decision(t @ FormatToken(left, comma @ Comma(), _), splits)
            if noTrailingCommas &&
              !next(t).right.is[Comment] &&
              owners(open) == owners(comma) =>
          Decision(t, splits.map {
            case x if x.modification == NoSplit =>
              x.copy(modification = Newline)
            case x => x
          })

        // Newline on every comma.
        case d @ Decision(
              t @ FormatToken(comma @ Comma(), right, between),
              splits)
            if owners(open) == owners(comma) &&
              // TODO(olafur) what the right { decides to be single line?
              !right.is[LeftBrace] &&
              // If comment is bound to comma, see unit/Comment.
              (!right.is[Comment] ||
                between.exists(_.is[LF])) =>
          Decision(t, splits.filter { x =>
            val isNewline = x.modification.isNewline
            if (noTrailingCommas && !right.is[Comment]) !isNewline
            else isNewline
          })
      },
      expire.end
    )
  }

  def UnindentAtExclude(
      exclude: Set[Token],
      indent: Length): PartialFunction[Decision, Decision] = {
    case Decision(t, s) if exclude.contains(t.left) =>
      val close = matchingParentheses(hash(t.left))
      Decision(t, s.map(_.withIndent(indent, close, ExpiresOn.Left)))
  }

  def penalizeAllNewlines(
      expire: Token,
      penalty: Int,
      penalizeLambdas: Boolean = true,
      ignore: FormatToken => Boolean = _ => false,
      penaliseNewlinesInsideTokens: Boolean = false)(
      implicit line: sourcecode.Line): Policy = {
    Policy(
      {
        case Decision(tok, s)
            if tok.right.end < expire.end &&
              (penalizeLambdas || !tok.left.is[RightArrow]) && !ignore(tok) =>
          Decision(tok, s.map {
            case split
                if split.modification.isNewline ||
                  (penaliseNewlinesInsideTokens && tok.leftHasNewline) =>
              split.withPenalty(penalty)
            case x => x
          })
      },
      expire.end
    )
  }

  def penalizeNewlineByNesting(from: Token, to: Token)(
      implicit line: sourcecode.Line): Policy = {
    val range = Range(from.start, to.end).inclusive
    Policy(
      {
        case Decision(t, s) if range.contains(t.right.start) =>
          val nonBoolPenalty =
            if (isBoolOperator(t.left)) 0
            else 5

          val penalty =
            nestedSelect(owners(t.left)) + nestedApplies(owners(t.right)) +
              nonBoolPenalty
          Decision(t, s.map {
            case split if split.modification.isNewline =>
              split.withPenalty(penalty)
            case x => x
          })
      },
      to.end
    )
  }

  def getArrow(caseStat: Case): Token =
    caseStat.tokens
      .find(t => t.is[RightArrow] && owners(t) == caseStat)
      .getOrElse(throw CaseMissingArrow(caseStat))

  def templateCurly(owner: Tree): Token = {
    defnTemplate(owner).flatMap(templateCurly).getOrElse(owner.tokens.last)
  }

  def templateCurly(template: Template): Option[Token] = {
    template.tokens.find(x => x.is[LeftBrace] && owners(x) == template)
  }

  def safeFilterNewlines(splits: Seq[Split])(
      implicit line: sourcecode.Line): Seq[Split] = {
    val filtered = splits.filter(_.modification.isNewline)
    if (filtered.nonEmpty) filtered
    else Seq(Split(Newline, 0))
  }

  final def getElseChain(term: Term.If): Vector[KwElse] = {
    term.tokens.find(x => x.is[KwElse] && owners(x) == term) match {
      case Some(els @ KwElse()) =>
        val rest = term.elsep match {
          case t: Term.If => getElseChain(t)
          case _ => Vector.empty[KwElse]
        }
        els +: rest
      case _ => Vector.empty[KwElse]
    }
  }

  def lastTokenInChain(chain: Vector[Term.Select]): Token = {
    if (chain.length == 1) lastToken(chain.last)
    else chainOptimalToken(chain)
  }

  /**
    * Returns last token of select, handles case when select's parent is apply.
    *
    * For example, in:
    * foo.bar[T](1, 2)
    * the last token is the final )
    *
    * @param dot the dot owned by the select.
    */
  def getSelectsLastToken(dot: Dot): Token = {
    var curr = next(leftTok2tok(dot))
    while (isOpenApply(curr.right, includeCurly = true, includeNoParens = true) &&
      !statementStarts.contains(hash(curr.right))) {
      curr = leftTok2tok(matchingParentheses(hash(curr.right)))
    }
    curr.left
  }

  def getRightAttachedComment(token: Token): Token = {
    val formatToken = leftTok2tok(token)
    if (isAttachedSingleLineComment(formatToken.right, formatToken.between))
      formatToken.right
    else token
  }

  def chainOptimalToken(chain: Vector[Term.Select]): Token = {
    val lastDotIndex = chain.last.tokens.lastIndexWhere(_.is[Dot])
    val lastDot =
      if (lastDotIndex != -1)
        chain.last.tokens(dialect)(lastDotIndex).asInstanceOf[Dot]
      else
        throw new IllegalStateException(s"Missing . in select ${chain.last}")
    lastToken(owners(getSelectsLastToken(lastDot)))
  }

  def infixSplit(owner: Term.ApplyInfix, formatToken: FormatToken)(
      implicit line: sourcecode.Line): Split =
    infixSplit(owner, owner.op, owner.args, formatToken)

  def infixSplit(
      owner: Tree,
      op: Name,
      rhsArgs: Seq[Tree],
      formatToken: FormatToken)(implicit line: sourcecode.Line): Split = {
    val style = styleMap.at(formatToken)
    val modification = newlines2Modification(
      formatToken.between,
      rightIsComment = formatToken.right.isInstanceOf[Comment])
    val indent = {
      if (style.unindentTopLevelOperators &&
        !isTopLevelInfixApplication(owner) &&
        style.indentOperator.includeRegexp
          .findFirstIn(formatToken.left.syntax)
          .isDefined &&
        style.indentOperator.excludeRegexp
          .findFirstIn(formatToken.left.syntax)
          .isEmpty) 2
      else if ((style.unindentTopLevelOperators ||
        isTopLevelInfixApplication(owner)) &&
        (style.indentOperator.includeRegexp
          .findFirstIn(op.tokens.head.syntax)
          .isEmpty ||
        style.indentOperator.excludeRegexp
          .findFirstIn(op.tokens.head.syntax)
          .isDefined)) 0
      else if (!modification.isNewline &&
        !isAttachedSingleLineComment(formatToken.right, formatToken.between)) 0
      else 2
    }
    val isRightAssociative =
      // NOTE. Silly workaround because we call infixSplit from assignment =, see #798
      formatToken.left.syntax != "=" &&
        isRightAssociativeOperator(op.value)
    val expire = (for {
      arg <- {
        if (isRightAssociative) rhsArgs.headOption
        else rhsArgs.lastOption
      }
      token <- {
        if (isRightAssociative) {
          arg match {
            case Term.ApplyInfix(lhs, _, _, _) =>
              lhs.tokens.lastOption
            case Pat.ExtractInfix(lhs, _, _) =>
              lhs.tokens.lastOption
            case _ =>
              arg.tokens.lastOption
          }
        } else {
          arg.tokens.lastOption
        }
      }
    } yield token).getOrElse(owner.tokens.last)

    owner.parent match {
      case Some(_: Type.ApplyInfix)
          if style.spaces.neverAroundInfixTypes.contains((op.value)) =>
        Split(NoSplit, 0)
      case _ =>
        Split(modification, 0).withIndent(Num(indent), expire, ExpiresOn.Left)
    }

  }

  /**
    * Returns the expire token for the owner of dot.
    *
    * If the select is part of an apply like
    *
    * foo.bar { ... }
    *
    * the expire token is the closing }, otherwise it's bar.
    */
  def selectExpire(dot: Dot): Token = {
    val owner = ownersMap(hash(dot))
    (for {
      parent <- owner.parent
      (_, args) <- splitApplyIntoLhsAndArgsLifted(parent) if args.nonEmpty
    } yield {
      args.last.tokens.last
    }).getOrElse(owner.tokens.last)
  }

  def isEmptyFunctionBody(tree: Tree): Boolean = tree match {
    case function: Term.Function =>
      function.body match {
        case b: Term.Block => b.stats.isEmpty
        case _ => false
      }
    case _ => false
  }

  def functionExpire(function: Term.Function): (Token, ExpiresOn) = {
    (for {
      parent <- function.parent
      blockEnd <- parent match {
        case b: Term.Block if b.stats.length == 1 =>
          Some(b.tokens.last -> Right)
        case _ => None
      }
    } yield blockEnd).getOrElse {
      function.tokens.last match {
        case tok @ Whitespace() => tok -> Right
        case tok => tok -> Left
      }
    }
  }

  def noOptimizationZones(tree: Tree): Set[Token] = {
    val result = Set.newBuilder[Token]
    var inside = false
    var expire = tree.tokens.head
    tree.tokens.foreach {
      case t if !inside && ((t, ownersMap(hash(t))) match {
            case (LeftParen(), _: Term.Apply | _: Init) =>
              // TODO(olafur) https://github.com/scalameta/scalameta/issues/345
              val x = true
              x
            // Type compounds can be inside defn.defs
            case (LeftBrace(), Type.Refine(_, _)) => true
            case _ => false
          }) =>
        inside = true
        expire = matchingParentheses(hash(t))
      case x if x == expire => inside = false
      case x if inside => result += x
      case _ =>
    }
    result.result()
  }

  def opensConfigStyle(formatToken: FormatToken): Boolean = {
    newlinesBetween(formatToken.between) > 0 && {
      val close = matchingParentheses(hash(formatToken.left))
      newlinesBetween(prev(leftTok2tok(close)).between) > 0
    }
  }

  def styleAt(tree: Tree): ScalafmtConfig = {
    val style =
      styleMap.at(leftTok2tok.getOrElse(tree.tokens.head, tokens.head))
    if (styleMap.forcedBinPack(tree)) // off-by-one
      styleMap.setBinPack(style, callSite = true)
    else style
  }

  def getApplyIndent(leftOwner: Tree, isConfigStyle: Boolean = false): Num = {
    val style = styleAt(leftOwner)
    leftOwner match {
      case _: Pat if leftOwner.parent.exists(_.is[Case]) =>
        // The first layer of indentation is provided by the case ensure
        // orpan comments and the case cond is indented correctly.
        Num(0)
      case x if isDefnSite(x) && !x.isInstanceOf[Type.Apply] =>
        if (style.binPack.unsafeDefnSite && !isConfigStyle) Num(0)
        else Num(style.continuationIndent.defnSite)
      case _ => Num(style.continuationIndent.callSite)
    }
  }

  def isBinPack(owner: Tree): Boolean = {
    val style = styleAt(owner)
    (style.binPack.unsafeCallSite && isCallSite(owner)) ||
    (style.binPack.unsafeDefnSite && isDefnSite(owner))
  }

  def isSingleIdentifierAnnotation(tok: FormatToken): Boolean = {
    val toMatch = if (tok.right.is[RightParen]) {
      // Hack to allow any annotations with arguments like @foo(1)
      prev(prev(leftTok2tok(matchingParentheses(hash(tok.right)))))
    } else {
      tok
    }
    toMatch match {
      case FormatToken(At(), _: Ident, _) => true
      case _ => false
    }
  }

  def distance(left: Token, right: Token): Int = {
    nonWhitespaceOffset(right) - nonWhitespaceOffset(left)
  }

  def ctorWithChain(owners: Set[Tree], lastToken: Token): Policy = {
    if (styleMap.at(leftTok2tok(lastToken)).binPack.parentConstructors)
      NoPolicy
    else {
      Policy(
        {
          case Decision(t @ FormatToken(_, right @ KwWith(), _), splits)
              if owners.contains(ownersMap(hash(right))) =>
            Decision(t, splits.filter(_.modification.isNewline))
        },
        lastToken.end
      )
    }
  }

  def binPackParentConstructorSplits(
      owners: Set[Tree],
      lastToken: Token,
      indent: Int)(implicit line: sourcecode.Line): Seq[Split] = {
    Seq(
      Split(Space, 0)
        .withPolicy(SingleLineBlock(lastToken))
        .withIndent(Num(indent), lastToken, Left),
      Split(NewlineT(acceptSpace = true), 1)
        .withPolicy(ctorWithChain(owners, lastToken))
        .withIndent(Num(indent), lastToken, Left)
    )
  }

  def newlineBeforeClosingCurlyPolicy(close: Token) =
    Policy({
      case d @ Decision(t @ FormatToken(_, `close`, _), s) =>
        d.onlyNewlines
    }, close.end)

  // Returns the depth of this node in the AST, used to prevent false positives.
  final def vAlignDepth(tree: Tree): Int = {
    vAlignDepthCache.getOrElseUpdate(tree, vAlignDepthUnCached(tree))
  }

  private final def vAlignDepthUnCached(tree: Tree): Int = {
    val count: Int = tree match {
      // infix applications don't count towards the length, context #531
      case _: Term.ApplyInfix => 0
      case _ => 1
    }
    tree.parent match {
      case Some(parent) => count + vAlignDepth(parent)
      case None => count
    }
  }

  def getForceConfigStyle: (Set[Tree], Set[TokenHash]) = {
    val maxDistance = runner.optimizer.forceConfigStyleOnOffset
    if (maxDistance < 0)
      (Set.empty, Set.empty)
    else {
      val clearQueues = Set.newBuilder[TokenHash]
      val forces = Set.newBuilder[Tree]
      tree.tokens.foreach {
        case left @ LeftParen() `:owner:` (app: Term.Apply)
            if app.args.length >= runner.optimizer.forceConfigStyleMinArgCount &&
              distance(left, matching(left)) > maxDistance =>
          forces += app
          app.args.foreach { arg =>
            clearQueues += hash(arg.tokens.head)
          }
        case _ =>
      }
      (forces.result(), clearQueues.result())
    }
  }

  /**
    * Implementation for `verticalMultiline`
    */
  def verticalMultiline(owner: Tree, ft: FormatToken)(
      implicit style: ScalafmtConfig): Seq[Split] = {

    val FormatToken(open, r, _) = ft
    val close = matching(open)
    val indentParam = Num(style.continuationIndent.defnSite)
    val indentSep = Num((indentParam.n - 2).max(0))
    val isBracket = open.is[LeftBracket]

    @tailrec
    def loop(token: Token): Option[Token] = {
      leftTok2tok(matching(token)) match {
        case FormatToken(RightParenOrBracket(), l @ LeftParen(), _) =>
          loop(l)
        case f @ FormatToken(left @ RightParenOrBracket(), right, _) =>
          right match {
            case Modifier() =>
              // This case only applies to classes
              next(f).right match {
                case x @ (_: Token.LeftParen | _: Token.LeftBracket) =>
                  loop(x)
                case _ =>
                  Some(left)
              }
            case _ =>
              Some(left)
          }
        case _ => None
      }
    }

    // find the last param on the defn so that we can apply our `policy`
    // till the end.
    val lastParen = loop(open).get

    val mixedParams = {
      owner match {
        case cls: meta.Defn.Class =>
          cls.tparams.nonEmpty && cls.ctor.paramss.nonEmpty
        case _ => false
      }
    }

    val isClassLike = owner.is[meta.Ctor.Primary] || owner.is[meta.Defn.Class]
    val isTrait = owner.is[meta.Defn.Trait]
    val excludeClass = style.verticalMultiline.excludeDanglingParens
      .contains(DanglingExclude.`class`)
    val excludeTrait = style.verticalMultiline.excludeDanglingParens
      .contains(DanglingExclude.`trait`)

    val shouldNotDangle =
      (isClassLike && excludeClass) ||
        (isTrait && excludeTrait)

    // Since classes and defs aren't the same (see below), we need to
    // create two (2) OneArgOneLineSplit when dealing with classes. One
    // deals with the type params and the other with the value params.
    val oneLinePerArg = {
      val base = OneArgOneLineSplit(open)
      if (mixedParams) {
        val afterTypes = leftTok2tok(matchingParentheses(hash(open)))
        // Try to find the first paren. If found, then we are dealing with
        // a class with type AND value params. Otherwise it is a class with
        // just type params.
        findFirst(afterTypes, lastParen)(t => t.left.is[LeftParen])
          .fold(base)(t => base.merge(OneArgOneLineSplit(t.left)))
      } else base
    }

    // DESNOTE(2017-03-28, pjrt) Classes and defs aren't the same.
    // For defs, type params and value param have the same `owners`. However
    // this is not the case for classes. Type params have the class itself
    // as the owner, but value params have the Ctor as the owner, so a
    // simple check isn't enough. Instead we check against the owner of the
    // `lastParen` as well, which will be the same as the value param's
    // owner.
    val valueParamsOwner = owners(lastParen)
    @inline def ownerCheck(rp: Token): Boolean = {
      val rpOwner = owners(rp)
      rpOwner == owner || rpOwner == valueParamsOwner
    }

    val paramGroupSplitter: PartialFunction[Decision, Decision] = {
      // If this is a class, then don't dangle the last paren unless the line ends with a comment
      case Decision(t @ FormatToken(previous, rp @ RightParenOrBracket(), _), _)
          if shouldNotDangle && rp == lastParen && !isSingleLineComment(
            previous) =>
        val split = Split(NoSplit, 0)
        Decision(t, Seq(split))
      // Indent seperators `)(` and `](` by `indentSep`
      case Decision(t @ FormatToken(_, rp @ RightParenOrBracket(), _), _)
          if ownerCheck(rp) =>
        val split = Split(Newline, 0).withIndent(indentSep, rp, Left)
        Decision(t, Seq(split))
      // Add a newline after left paren if:
      // - There's an implicit keyword and newlineBeforeImplicitKW is enabled
      // - newlineAfterOpenParen is enabled
      // - Mixed-params case with constructor modifier `] private (`
      case Decision(t @ FormatToken(open2 @ LeftParen(), right, _), _) =>
        val close2 = matchingParentheses(hash(open2))
        val prevT = prev(t).left

        val isImplicitArgList = right.is[KwImplicit]

        val newlineBeforeImplicitEnabled =
          style.verticalMultiline.newlineBeforeImplicitKW ||
            style.newlines.beforeImplicitKWInVerticalMultiline

        val mixedParamsWithCtorModifier =
          mixedParams && owners(prevT).is[CtorModifier]

        // We don't want to create newlines for default values.
        val isDefinition = ownerCheck(close2)

        val shouldAddNewline =
          (isImplicitArgList && newlineBeforeImplicitEnabled) ||
            (style.verticalMultiline.newlineAfterOpenParen && !isImplicitArgList && isDefinition) ||
            mixedParamsWithCtorModifier

        val mod =
          if (shouldAddNewline) Newline
          else NoSplit

        Decision(
          t,
          Seq(
            Split(mod, 0)
              .withIndent(indentParam, close2, Right)
          ))
      case Decision(t @ FormatToken(KwImplicit(), _, _), _)
          if (style.verticalMultiline.newlineAfterImplicitKW || style.newlines.afterImplicitKWInVerticalMultiline) =>
        val split = Split(Newline, 0)
        Decision(t, Seq(split))
    }

    // Our policy is a combination of OneArgLineSplit and a custom splitter
    // for parameter groups.
    val policy = oneLinePerArg.merge(paramGroupSplitter, lastParen.end)

    val firstIndent =
      if (r.is[RightParen]) indentSep // An empty param group
      else indentParam

    val singleLineExpire =
      if (isBracket) close // If we can fit the type params, make it so
      else lastParen // If we can fit all in one block, make it so

    val maxArity = valueParamsOwner match {
      case d: Decl.Def if d.paramss.nonEmpty => d.paramss.map(_.size).max
      case d: Defn.Def if d.paramss.nonEmpty => d.paramss.map(_.size).max
      case m: Defn.Macro if m.paramss.nonEmpty => m.paramss.map(_.size).max
      case c: Ctor.Primary if c.paramss.nonEmpty => c.paramss.map(_.size).max
      case c: Ctor.Secondary if c.paramss.nonEmpty => c.paramss.map(_.size).max
      case _ => 0
    }

    val aboveArityThreshold = (maxArity >= style.verticalMultiline.arityThreshold) || (maxArity >= style.verticalMultilineAtDefinitionSiteArityThreshold)

    Seq(
      Split(NoSplit, 0, ignoreIf = !isBracket && aboveArityThreshold)
        .withPolicy(SingleLineBlock(singleLineExpire)),
      Split(Newline, 1) // Otherwise split vertically
        .withIndent(firstIndent, close, Right)
        .withPolicy(policy)
    )

  }

  // Returns leading comment, if there exists one, otherwise formatToken.right
  @tailrec
  final def leadingComment(formatToken: FormatToken): Token = {
    if (formatToken.newlinesBetween <= 1 && formatToken.left.is[Comment])
      leadingComment(prev(formatToken))
    else formatToken.right
  }

  def xmlSpace(owner: Tree): Modification = owner match {
    case _: Term.Xml | _: Pat.Xml => NoSplit
    case _ => Space
  }
}
