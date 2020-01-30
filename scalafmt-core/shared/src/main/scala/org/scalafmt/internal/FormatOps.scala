package org.scalafmt.internal

import java.{util => ju}
import scala.collection.JavaConverters._
import org.scalafmt.Error.CaseMissingArrow
import org.scalafmt.config.{DanglingExclude, NewlineCurlyLambda, ScalafmtConfig}
import org.scalafmt.internal.ExpiresOn.{Left, Right}
import org.scalafmt.internal.Length.Num
import org.scalafmt.internal.Policy.NoPolicy
import org.scalafmt.util._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.meta.{
  Case,
  Ctor,
  Decl,
  Defn,
  Import,
  Init,
  Name,
  Pat,
  Pkg,
  Template,
  Term,
  Tree,
  Type
}
import scala.meta.prettyprinters.Structure
import scala.meta.tokens.Token
import scala.meta.tokens.{Token => T}

/**
  * Helper functions for generating splits/policies for a given tree.
  */
class FormatOps(val tree: Tree, val initStyle: ScalafmtConfig) {
  val runner = initStyle.runner
  import TokenOps._
  import TreeOps._
  implicit val dialect = initStyle.runner.dialect
  val tokens: FormatTokens = FormatTokens(tree.tokens)
  private val ownersMap = getOwners(tree)
  val statementStarts = getStatementStarts(tree)
  val dequeueSpots = getDequeueSpots(tree) ++ statementStarts.keys
  private val matchingParentheses: Map[TokenHash, Token] =
    getMatchingParentheses(tree.tokens)
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
  @inline def matchingOpt(token: Token): Option[Token] =
    matchingParentheses.get(hash(token))

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

    import scala.collection.mutable
    val workList = new ju.LinkedList[Tree]()
    workList.add(tree)
    while (!workList.isEmpty) {
      val tree = workList.poll()
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
      workList.addAll(tree.children.asJava)
    }
    (packages.result(), imports.result(), arguments.toMap, optional)
  }

  object `:owner:` {
    def unapply(tok: Token): Option[(Token, Tree)] =
      ownersMap.get(hash(tok)).map(tree => tok -> tree)
  }

  object `:chain:` {
    def unapply(tok: Token): Option[(Token, Vector[Term.Select])] = {
      val rightOwner = owners(tok)
      val openApply = tokens(tok, 1).right
      def startsOpenApply =
        isOpenApply(
          openApply,
          includeCurly = initStyle.includeCurlyBraceInSelectChains,
          includeNoParens = initStyle.includeNoParensInSelectChains
        )
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
          val chain = getSelectChain(t, Vector(t))
          if (openApply.is[T.LeftBrace] && isShortCurlyChain(chain)) None
          else Some(tok -> chain)
        case _ => None
      }
    }
  }

  @inline def prev(tok: FormatToken): FormatToken = tokens(tok, -1)
  @inline def next(tok: FormatToken): FormatToken = tokens(tok, 1)

  @tailrec
  final def findFirst(start: FormatToken, end: Token)(
      f: FormatToken => Boolean
  ): Option[FormatToken] = {
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
      accum: Int = 0
  ): (Int, FormatToken) = {
    if (!curr.left.is[T.Comment]) accum -> curr
    else {
      val tok = prev(curr)
      if (tok == curr) accum -> curr
      else prevNonCommentWithCount(tok, accum + 1)
    }
  }
  def prevNonComment(curr: FormatToken): FormatToken =
    prevNonCommentWithCount(curr)._2

  @tailrec
  final def nextNonCommentSameLine(curr: FormatToken): FormatToken =
    if (curr.newlinesBetween != 0 || !curr.right.is[T.Comment]) curr
    else {
      val tok = next(curr)
      if (tok == curr) curr
      else nextNonCommentSameLine(tok)
    }

  @tailrec
  final def nextNonCommentWithCount(
      curr: FormatToken,
      accum: Int = 0
  ): (Int, FormatToken) = {
    if (!curr.right.is[T.Comment]) accum -> curr
    else {
      val tok = next(curr)
      if (tok == curr) accum -> curr
      else nextNonCommentWithCount(tok, accum + 1)
    }
  }
  def nextNonComment(curr: FormatToken): FormatToken =
    nextNonCommentWithCount(curr)._2

  @tailrec
  final def rhsOptimalToken(
      start: FormatToken
  )(implicit style: ScalafmtConfig): Token = {
    start.right match {
      case T.Comma() | T.LeftParen() | T.RightParen() | T.RightBracket() |
          T.Semicolon() | T.RightArrow() | T.Equals()
          if (1 + start.meta.idx) != tokens.length &&
            !startsNewBlock(start.right) &&
            start.newlinesBetween == 0 =>
        rhsOptimalToken(next(start))
      case c: T.Comment
          if style.activeForEdition_2020_01 && start.newlinesBetween == 0 =>
        c
      case _ => start.left
    }
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
      _.show[Structure].trim == """Term.Select(Term.Name("js"), Term.Name("native"))"""
    )
  }

  def isTripleQuote(token: Token): Boolean = token.syntax.startsWith("\"\"\"")

  def isMarginizedString(token: Token): Boolean = token match {
    case start @ T.Interpolation.Start() =>
      val end = matching(start)
      val afterEnd = tokens(end, 1)
      afterEnd.left.syntax == "." && afterEnd.right.syntax == "stripMargin"
    case string: T.Constant.String =>
      string.syntax.startsWith("\"\"\"") && {
        val afterString = tokens(string, 1)
        afterString.left.syntax == "." &&
        afterString.right.syntax == "stripMargin"
      }
    case _ => false
  }

  @tailrec
  final def startsStatement(tok: FormatToken): Boolean = {
    statementStarts.contains(hash(tok.right)) ||
    (tok.right.is[T.Comment] && tok.newlinesBetween != 0 &&
    startsStatement(next(tok)))
  }

  def parensRange(open: Token): Range =
    Range(open.start, matching(open).end)

  def getExcludeIf(
      end: Token,
      cond: Token => Boolean = _.is[T.RightBrace]
  ): Set[Range] = {
    if (cond(end)) // allow newlines in final {} block
      Set(Range(matching(end).start, end.end))
    else Set.empty[Range]
  }

  def insideBlock(
      start: FormatToken,
      end: Token,
      matches: Token => Boolean
  ): Set[Token] = {
    val result = Set.newBuilder[Token]
    var prev = start
    var curr = next(start)

    def goToMatching(): Unit = {
      val close = matching(curr.left)
      curr = tokens(close)
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

  def defnSiteLastToken(close: FormatToken, tree: Tree): Token = {
    tree match {
      // TODO(olafur) scala.meta should make this easier.
      case procedure: Defn.Def
          if procedure.decltpe.isDefined &&
            procedure.decltpe.get.tokens.isEmpty =>
        procedure.body.tokens.find(_.is[T.LeftBrace])
      case t: Defn.Def if t.body.is[Term.Block] =>
        t.body.tokens.headOption
      case _: Ctor.Primary =>
        close match {
          // This is a terrible terrible hack. Please consider removing this.
          // The RightParen() LeftBrace() pair is presumably a ") {" combination
          // at a class definition
          case FormatToken(_: T.RightParen, b: T.LeftBrace, _) => Some(b)
          case _ => Some(close.left)
        }
      case _ =>
        tree.tokens.find(t => t.is[T.Equals] && owners(t) == tree)
    }
  }.getOrElse(tree.tokens.last)

  def OneArgOneLineSplit(open: Token, noTrailingCommas: Boolean = false)(
      implicit line: sourcecode.Line
  ): Policy = {
    // TODO(olafur) clear queue between arguments, they are independent.
    val expire = matching(open)
    Policy(
      {
        case d @ Decision(t @ FormatToken(left, comma @ T.Comma(), _), splits)
            if noTrailingCommas &&
              !next(t).right.is[T.Comment] &&
              owners(open) == owners(comma) =>
          Decision(t, splits.map {
            case x if x.modification == NoSplit =>
              x.copy(modification = Newline)
            case x => x
          })

        // Newline on every comma.
        case d @ Decision(
              t @ FormatToken(comma @ T.Comma(), right, between),
              splits
            )
            if owners(open) == owners(comma) &&
              // TODO(olafur) what the right { decides to be single line?
              !right.is[T.LeftBrace] &&
              // If comment is bound to comma, see unit/Comment.
              (!right.is[T.Comment] || t.newlinesBetween != 0) =>
          Decision(t, splits.filter { x =>
            val isNewline = x.modification.isNewline
            if (noTrailingCommas && !right.is[T.Comment]) !isNewline
            else isNewline
          })
      },
      expire.end
    )
  }

  def UnindentAtExclude(
      exclude: Set[Token],
      indent: Length
  ): Policy.Pf = {
    case Decision(t, s) if exclude.contains(t.left) =>
      val close = matching(t.left)
      Decision(t, s.map(_.withIndent(indent, close, ExpiresOn.Left)))
  }

  def penalizeAllNewlines(
      expire: Token,
      penalty: Int,
      penalizeLambdas: Boolean = true,
      ignore: FormatToken => Boolean = _ => false,
      penaliseNewlinesInsideTokens: Boolean = false
  )(implicit line: sourcecode.Line): Policy = {
    Policy(
      {
        case Decision(tok, s)
            if tok.right.end < expire.end &&
              (penalizeLambdas || !tok.left.is[T.RightArrow]) && !ignore(tok) =>
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
      implicit line: sourcecode.Line
  ): Policy = {
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
      .find(t => t.is[T.RightArrow] && owners(t) == caseStat)
      .getOrElse(throw CaseMissingArrow(caseStat))

  def templateCurly(owner: Tree): Token = {
    defnTemplate(owner).flatMap(templateCurly).getOrElse(owner.tokens.last)
  }

  def templateCurly(template: Template): Option[Token] = {
    template.tokens.find(x => x.is[T.LeftBrace] && owners(x) == template)
  }

  @inline
  def getElseChain(term: Term.If): Seq[T] = getElseChain(term, Seq.empty)

  @tailrec
  private final def getElseChain(term: Term.If, res: Seq[T]): Seq[T] = {
    term.tokens.find(x => x.is[T.KwElse] && owners(x) == term) match {
      case Some(els @ T.KwElse()) =>
        val tuck = !initStyle.newlines.alwaysBeforeElseAfterCurlyIf && {
          val prev = tokens(els, -1).left
          prev.is[T.RightBrace] && owners(prev) != term
        }
        val newRes = if (tuck) res else res :+ els
        term.elsep match {
          case t: Term.If => getElseChain(t, newRes)
          case _ => newRes
        }
      case _ => res
    }
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
  def getSelectsLastToken(dot: T.Dot): Token = {
    var curr = tokens(dot, 1)
    while (isOpenApply(curr.right, includeCurly = true, includeNoParens = true) &&
      !statementStarts.contains(hash(curr.right))) {
      if (curr.right.is[T.Dot]) {
        curr = tokens(curr, 2)
      } else {
        curr = tokens(matching(curr.right))
      }
    }
    curr.left
  }

  def getOptimalTokenFor(token: Token): Token =
    getOptimalTokenFor(tokens(token))

  def getOptimalTokenFor(ft: FormatToken): Token =
    if (isAttachedSingleLineComment(ft)) ft.right else ft.left

  def chainOptimalToken(chain: Vector[Term.Select]): Token = {
    val lastDotIndex = chain.last.tokens.lastIndexWhere(_.is[T.Dot])
    val lastDot =
      if (lastDotIndex != -1)
        chain.last.tokens(dialect)(lastDotIndex).asInstanceOf[T.Dot]
      else
        throw new IllegalStateException(s"Missing . in select ${chain.last}")
    lastToken(owners(getSelectsLastToken(lastDot)))
  }

  def infixIndent(
      owner: Term.ApplyInfix,
      formatToken: FormatToken,
      isNewline: Boolean
  )(implicit style: ScalafmtConfig): Int =
    infixIndent(owner, owner.op, owner.args, formatToken, isNewline)

  def infixIndent(
      owner: Tree,
      op: Name,
      rhsArgs: Seq[Tree],
      formatToken: FormatToken,
      isNewline: Boolean
  )(implicit style: ScalafmtConfig): Int = {
    if (style.verticalAlignMultilineOperators) {
      if (formatToken.left.text == "=") 2 else 0
    } else if (style.unindentTopLevelOperators &&
      rhsArgs.headOption
        .forall(x => x.isNot[Term.Block] && x.isNot[Term.NewAnonymous]) &&
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
    else if (!isNewline &&
      !isSingleLineComment(formatToken.right)) 0
    else 2
  }

  def infixSplit(
      owner: Term.ApplyInfix,
      formatToken: FormatToken
  )(implicit line: sourcecode.Line, style: ScalafmtConfig): Seq[Split] =
    infixSplit(owner, owner.op, owner.args, formatToken)

  private def infixSplitImpl(
      owner: Tree,
      op: Name,
      rhsArgs: Seq[Tree],
      formatToken: FormatToken
  )(implicit line: sourcecode.Line, style: ScalafmtConfig): Seq[Split] = {
    val isNotEquals = !formatToken.left.is[Token.Equals]
    val isRightAssociative =
      // NOTE. Silly workaround because we call infixSplit from assignment =, see #798
      isNotEquals &&
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

    // we don't modify line breaks generally around infix expressions
    // TODO: if that ever changes, modify how rewrite rules handle infix
    val modification = newlines2Modification(
      formatToken.newlinesBetween,
      isNoIndent(formatToken)
    )
    val isNewline = modification.isNewline
    val indent = infixIndent(owner, op, rhsArgs, formatToken, isNewline)
    val split =
      Split(modification, 0).withIndent(Num(indent), expire, ExpiresOn.Left)

    if (!style.activeForEdition_2020_01 || isNotEquals ||
      isNewline || formatToken.right.is[T.Comment])
      Seq(split)
    else {
      val altIndent = infixIndent(owner, op, rhsArgs, formatToken, true)
      Seq(
        split,
        Split(Newline, 1).withIndent(Num(altIndent), expire, ExpiresOn.Left)
      )
    }
  }

  def infixSplit(
      owner: Tree,
      op: Name,
      rhsArgs: Seq[Tree],
      formatToken: FormatToken
  )(implicit line: sourcecode.Line, style: ScalafmtConfig): Seq[Split] =
    owner.parent match {
      case Some(_: Type.ApplyInfix)
          if style.spaces.neverAroundInfixTypes.contains((op.value)) =>
        Seq(Split(NoSplit, 0))
      case _ =>
        infixSplitImpl(owner, op, rhsArgs, formatToken)
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
    def dropWS(rtoks: Seq[Token]): Seq[Token] =
      rtoks.dropWhile(_.is[Whitespace])
    def orElse(rtoks: Seq[Token]) = {
      val last = rtoks.head
      if (last.is[T.RightParen] && (matching(last) eq rtoks.last))
        rtoks.tail.find(!_.is[Whitespace]).get -> Left
      else
        last -> Left
    }
    def dropComment(rtoks: Seq[Token]) =
      if (rtoks.head.is[T.Comment]) dropWS(rtoks.tail) else rtoks

    def getRToks = dropWS(function.tokens.reverse)
    function.parent match {
      case Some(b: Term.Block) if b.stats.length == 1 =>
        b.tokens.last -> Right
      case Some(Case(_, _, `function`)) =>
        orElse(dropComment(getRToks))
      case _ =>
        orElse(getRToks)
    }
  }

  def noOptimizationZones(tree: Tree): Set[Token] = {
    val result = Set.newBuilder[Token]
    var inside = false
    var expire = tree.tokens.head
    tree.tokens.foreach {
      case t if !inside && ((t, ownersMap(hash(t))) match {
            case (T.LeftParen(), _: Term.Apply | _: Init) =>
              // TODO(olafur) https://github.com/scalameta/scalameta/issues/345
              val x = true
              x
            // Type compounds can be inside defn.defs
            case (T.LeftBrace(), Type.Refine(_, _)) => true
            case _ => false
          }) =>
        inside = true
        expire = matching(t)
      case x if x == expire => inside = false
      case x if inside => result += x
      case _ =>
    }
    result.result()
  }

  def opensConfigStyle(formatToken: FormatToken): Boolean = {
    formatToken.newlinesBetween > 0 && {
      val close = matching(formatToken.left)
      tokens(close, -1).newlinesBetween > 0
    }
  }

  def styleAt(tree: Tree): ScalafmtConfig = {
    val style = styleMap.at(tree.tokens.head)
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
    val toMatch = if (tok.right.is[T.RightParen]) {
      // Hack to allow any annotations with arguments like @foo(1)
      tokens(matching(tok.right), -2)
    } else {
      tok
    }
    toMatch match {
      case FormatToken(T.At(), _: T.Ident, _) => true
      case _ => false
    }
  }

  def distance(left: Token, right: Token): Int = {
    nonWhitespaceOffset(right) - nonWhitespaceOffset(left)
  }

  def ctorWithChain(ownerSet: Set[Tree], lastToken: Token): Policy = {
    if (styleMap.at(tokens(lastToken)).binPack.parentConstructors)
      NoPolicy
    else {
      Policy(
        {
          case d @ Decision(FormatToken(_, right: T.KwWith, _), _)
              if ownerSet.contains(owners(right)) =>
            d.onlyNewlinesWithoutFallback
        },
        lastToken.end
      )
    }
  }

  def binPackParentConstructorSplits(
      owners: Set[Tree],
      lastToken: Token,
      indent: Int
  )(implicit line: sourcecode.Line): Seq[Split] = {
    Seq(
      Split(Space, 0)
        .withPolicy(SingleLineBlock(lastToken))
        .withIndent(Num(indent), lastToken, Left),
      Split(NewlineT(acceptSpace = true), 1)
        .withPolicy(ctorWithChain(owners, lastToken))
        .withIndent(Num(indent), lastToken, Left)
    )
  }

  def delayedBreakPolicy(
      leftCheck: Option[Token => Boolean]
  )(onBreakPolicy: Policy): Policy = {
    object OnBreakDecision {
      def unapply(d: Decision): Option[Decision] =
        if (leftCheck.exists(!_(d.formatToken.left))) None
        else unapplyImpl(d)
      private def unapplyImpl(d: Decision): Option[Decision] = {
        var replaced = false
        def decisionPf(s: Split): Split =
          if (!s.modification.isNewline) s
          else {
            replaced = true
            s.orElsePolicy(onBreakPolicy)
          }
        val splits = d.splits.map(decisionPf)
        if (replaced) Some(d.copy(splits = splits)) else None
      }
    }
    if (onBreakPolicy.isEmpty) onBreakPolicy
    else onBreakPolicy.copy(f = { case OnBreakDecision(d) => d })
  }

  def newlinesOnlyBeforeClosePolicy(close: Token)(
      implicit line: sourcecode.Line
  ): Policy =
    Policy(decideNewlinesOnlyBeforeClose(Split(Newline, 0)))(close)

  def decideNewlinesOnlyBeforeClose(split: Split)(close: Token): Policy.Pf = {
    case d: Decision if d.formatToken.right eq close =>
      d.onlyNewlinesWithFallback(split)
  }

  def decideNewlinesOnlyAfterClose(split: Split)(close: Token): Policy.Pf = {
    case d: Decision if d.formatToken.left eq close =>
      d.onlyNewlinesWithFallback(split)
  }

  def decideNewlinesOnlyAfterToken(token: Token): Policy.Pf = {
    case d: Decision if d.formatToken.left eq token =>
      d.onlyNewlinesWithoutFallback
  }

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
        case left @ T.LeftParen() `:owner:` (app: Term.Apply)
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
      implicit style: ScalafmtConfig
  ): Seq[Split] = {

    val FormatToken(open, r, _) = ft
    val close = matching(open)
    val indentParam = Num(style.continuationIndent.defnSite)
    val indentSep = Num((indentParam.n - 2).max(0))
    val isBracket = open.is[T.LeftBracket]

    @tailrec
    def loop(token: Token): Option[Token] = {
      tokens(matching(token)) match {
        case FormatToken(RightParenOrBracket(), l @ T.LeftParen(), _) =>
          loop(l)
        case f @ FormatToken(left @ RightParenOrBracket(), right, _) =>
          lazy val isCtorModifier =
            owners(right).parent.exists(_.is[meta.Ctor])
          right match {
            // modifier for constructor if class definition has type parameters: [class A[T, K, C] private (a: Int)]
            case Modifier() if isCtorModifier =>
              // This case only applies to classes
              next(f).right match {
                case x @ (_: T.LeftParen | _: T.LeftBracket) =>
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
    val isDef = owner.is[meta.Defn.Def]
    val excludeClass = style.verticalMultiline.excludeDanglingParens
      .contains(DanglingExclude.`class`)
    val excludeTrait = style.verticalMultiline.excludeDanglingParens
      .contains(DanglingExclude.`trait`)
    val excludeDef = style.verticalMultiline.excludeDanglingParens
      .contains(DanglingExclude.`def`)

    val shouldNotDangle =
      (isClassLike && excludeClass) ||
        (isTrait && excludeTrait) ||
        (isDef && excludeDef)

    // Since classes and defs aren't the same (see below), we need to
    // create two (2) OneArgOneLineSplit when dealing with classes. One
    // deals with the type params and the other with the value params.
    val oneLinePerArg = {
      val base = OneArgOneLineSplit(open)
      if (mixedParams) {
        val afterTypes = tokens(matching(open))
        // Try to find the first paren. If found, then we are dealing with
        // a class with type AND value params. Otherwise it is a class with
        // just type params.
        findFirst(afterTypes, lastParen)(t => t.left.is[T.LeftParen])
          .fold(base)(t => base.orElse(OneArgOneLineSplit(t.left)))
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

    val paramGroupSplitter: Policy.Pf = {
      // If this is a class, then don't dangle the last paren unless the line ends with a comment
      case Decision(t @ FormatToken(previous, rp @ RightParenOrBracket(), _), _)
          if shouldNotDangle && rp == lastParen && !isSingleLineComment(
            previous
          ) =>
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
      case Decision(t @ FormatToken(open2 @ T.LeftParen(), right, _), _) =>
        val close2 = matching(open2)
        val prevT = prev(t).left

        val isImplicitArgList = right.is[T.KwImplicit]

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
          )
        )
      case Decision(t @ FormatToken(T.KwImplicit(), _, _), _)
          if (style.verticalMultiline.newlineAfterImplicitKW || style.newlines.afterImplicitKWInVerticalMultiline) =>
        val split = Split(Newline, 0)
        Decision(t, Seq(split))
    }

    // Our policy is a combination of OneArgLineSplit and a custom splitter
    // for parameter groups.
    val policy = oneLinePerArg.orElse(paramGroupSplitter, lastParen.end)

    val firstIndent =
      if (r.is[T.RightParen]) indentSep // An empty param group
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

    val singleLineModification =
      if (style.spaces.inParentheses) Space
      else NoSplit

    Seq(
      Split(
        singleLineModification,
        0,
        ignoreIf = !isBracket && aboveArityThreshold
      ).withPolicy(SingleLineBlock(singleLineExpire)),
      Split(Newline, 1) // Otherwise split vertically
        .withIndent(firstIndent, close, Right)
        .withPolicy(policy)
    )

  }

  // Returns leading comment, if there exists one, otherwise formatToken.right
  @tailrec
  final def leadingComment(formatToken: FormatToken): Token = {
    if (formatToken.newlinesBetween <= 1 && formatToken.left.is[T.Comment])
      leadingComment(prev(formatToken))
    else formatToken.right
  }

  def xmlSpace(owner: Tree): Modification = owner match {
    case _: Term.Xml | _: Pat.Xml => NoSplit
    case _ => Space
  }

  def getSpaceAndNewlineAfterCurlyLambda(
      newlines: Int
  )(implicit style: ScalafmtConfig): NewlineT =
    style.newlines.afterCurlyLambda match {
      case NewlineCurlyLambda.never => Newline
      case NewlineCurlyLambda.always => Newline2x
      case NewlineCurlyLambda.preserve =>
        if (newlines >= 2) Newline2x else Newline
    }

}
