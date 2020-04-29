package org.scalafmt.internal

import java.{util => ju}

import scala.collection.JavaConverters._
import org.scalafmt.Error.UnexpectedTree
import org.scalafmt.config.{NewlineCurlyLambda, Newlines, ScalafmtConfig}
import org.scalafmt.internal.Length.Num
import org.scalafmt.internal.Policy.NoPolicy
import org.scalafmt.util._
import org.scalafmt.util.LoggerOps.{log, logger}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.meta.classifiers.Classifier
import scala.meta.{
  Case,
  Ctor,
  Decl,
  Defn,
  Import,
  Init,
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
class FormatOps(val tree: Tree, baseStyle: ScalafmtConfig) {
  val initStyle = {
    val queue = new mutable.Queue[Tree]
    queue += tree
    var count = 0
    while (queue.nonEmpty) {
      val elem = queue.dequeue()
      queue ++= elem.children
      if (TreeOps.isInfixApp(elem)) count += 1
    }
    val checkedNewlines = baseStyle.newlines.checkInfixConfig(count)
    if (checkedNewlines eq baseStyle.newlines) baseStyle
    else baseStyle.copy(newlines = checkedNewlines)
  }
  val runner = initStyle.runner
  import TokenOps._
  import TreeOps._
  implicit val dialect = initStyle.runner.dialect
  private val ownersMap = getOwners(tree)
  val tokens: FormatTokens = FormatTokens(tree.tokens, owners)
  private val statementStarts = getStatementStarts(tree)
  val dequeueSpots = getDequeueSpots(tree) ++ statementStarts.keys
  private val matchingParentheses: Map[TokenHash, Token] =
    getMatchingParentheses(tree.tokens)
  val styleMap =
    new StyleMap(tokens, initStyle, ownersMap, matchingParentheses)
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
      val ft = tokens(tok)
      val openApply = tokens(ft, 1).right
      def startsOpenApply =
        isOpenApply(
          openApply,
          includeCurly = initStyle.includeCurlyBraceInSelectChains,
          includeNoParens = initStyle.includeNoParensInSelectChains
        )
      def isShortCurlyChain(chain: Vector[Term.Select]): Boolean =
        chain.length == 2 && {
          !(for {
            child <- chain.lastOption
            parent <- child.parent
          } yield isChainApplyParent(parent, child)).getOrElse(false)
        }

      ft.meta.leftOwner match {
        case t: Term.Select
            if startsOpenApply &&
              !existsParentOfType[Import](ft.meta.leftOwner) =>
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

  final def nextNonCommentSameLine(curr: FormatToken): FormatToken =
    findToken(curr, next)(ft => ft.hasBreak || !ft.right.is[T.Comment])
      .fold(identity, identity)

  final def nextNonComment(curr: FormatToken): FormatToken =
    findToken(curr, next) {
      case FormatToken(_, _: T.Comment, _) => false
      case _ => true
    }.fold(identity, identity)

  final def prevNonComment(curr: FormatToken): FormatToken =
    findToken(curr, prev) {
      case FormatToken(_: T.Comment, _, _) => false
      case _ => true
    }.fold(identity, identity)

  final def rhsOptimalToken(
      start: FormatToken
  )(implicit style: ScalafmtConfig): Token =
    findTokenWith(start, next) { start =>
      start.right match {
        case _ if start.hasBlankLine => Some(start.left)
        case _: T.RightParen
            if start.left.is[T.RightParen] || start.left.is[T.LeftParen] =>
          None
        case _: T.RightBracket if start.left.is[T.RightBracket] => None
        case _: T.Comma | _: T.LeftParen | _: T.Semicolon | _: T.RightArrow |
            _: T.Equals
            if (style.activeForEdition_2020_03 && isInfixRhs(start) ||
              !startsNewBlockOnRight(start)) =>
          None
        case c: T.Comment
            if style.activeForEdition_2020_01 && start.noBreak &&
              (!start.left.is[T.LeftParen] || isSingleLineComment(c)) =>
          Some(c)
        case _ => Some(start.left)
      }
    }.fold(_.right, identity)

  @tailrec
  final def endOfSingleLineBlock(
      start: FormatToken
  )(implicit style: ScalafmtConfig): Token = {
    lazy val isInfix = style.activeForEdition_2020_03 && isInfixRhs(start)
    val endFound = start.right match {
      case _: T.Comma | _: T.LeftParen | _: T.Semicolon | _: T.RightArrow |
          _: T.Equals =>
        None
      case _: T.RightParen if start.left.is[T.LeftParen] => None
      case c: T.Comment if isSingleLineComment(c) && start.noBreak =>
        Some(c)
      case _ if start.noBreak && isInfix => None
      case _ => Some(start.left)
    }

    endFound match {
      case Some(t) => t
      case None =>
        if (!tokens.hasNext(start)) start.right
        else if (!isInfix && startsNewBlockOnRight(start)) start.left
        else endOfSingleLineBlock(next(start))
    }
  }

  final def isInfixRhs(ft: FormatToken): Boolean = {
    val tree = ft.meta.rightOwner
    tree.parent.exists {
      case InfixApp(ia) =>
        (ia.op eq tree) || ia.rhs.headOption.forall { arg =>
          (arg eq tree) && arg.tokens.headOption.contains(ft.right)
        }
      case _ => false
    }
  }

  final def startsNewBlockOnRight(ft: FormatToken): Boolean =
    ft.meta.rightOwner.tokens.headOption.contains(ft.right)

  /**
    * js.native is very special in Scala.js.
    *
    * Context: https://github.com/scalameta/scalafmt/issues/108
    */
  def isJsNative(jsToken: Token): Boolean = {
    initStyle.newlines.neverBeforeJsNative && jsToken.syntax == "js" &&
    owners(jsToken).parent.exists(
      _.show[
        Structure
      ].trim == """Term.Select(Term.Name("js"), Term.Name("native"))"""
    )
  }

  @inline
  final def startsStatement(tok: FormatToken): Option[Tree] =
    startsStatement(tok.right)
  @inline
  final def startsStatement(token: Token): Option[Tree] =
    statementStarts.get(hash(token))

  def parensRange(token: Token): Option[Range] =
    matchingOpt(token).map(matchingParensRange(token, _))

  def matchingParensRange(token: Token, other: Token): Range =
    if (token.start < other.end)
      Range(token.start, other.end)
    else
      Range(other.start, token.end)

  def getExcludeIf(
      end: Token,
      cond: Token => Boolean = _.is[T.RightBrace]
  ): Set[Range] = {
    if (cond(end)) // allow newlines in final {} block
      parensRange(end)
    else None
  }.toSet

  def insideBlockRanges[A](start: FormatToken, end: Token)(implicit
      classifier: Classifier[Token, A]
  ): Set[Range] =
    insideBlockRanges(start, end, classifyOnRight(classifier))

  def insideBlockRanges(
      start: FormatToken,
      end: Token,
      matches: FormatToken => Boolean
  ): Set[Range] =
    insideBlock(start, end, matches).map((matchingParensRange _).tupled).toSet

  def insideBlock[A](start: FormatToken, end: Token)(implicit
      classifier: Classifier[Token, A]
  ): Map[Token, Token] =
    insideBlock(start, end, classifyOnRight(classifier))

  def insideBlock(
      start: FormatToken,
      end: Token,
      matches: FormatToken => Boolean
  ): Map[Token, Token] = {
    val result = Map.newBuilder[Token, Token]

    @tailrec
    def run(tok: FormatToken): Unit =
      if (tok.right.start < end.start) {
        val nextTokOpt = Option(tok).filter(matches).flatMap { tok =>
          val open = tok.right
          matchingOpt(open).flatMap { close =>
            if (open.start >= close.end) None
            else {
              result += open -> close
              Some(tokens(close))
            }
          }
        }
        val nextTok = nextTokOpt.getOrElse(next(tok))
        if (nextTok ne tok) run(nextTok)
      }

    run(start)
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

  @inline
  def OneArgOneLineSplit(tok: FormatToken)(implicit
      line: sourcecode.Line,
      style: ScalafmtConfig
  ): Policy =
    if (style.poorMansTrailingCommasInConfigStyle)
      splitOneArgPerLineBeforeComma(tok)
    else
      splitOneArgPerLineAfterComma(tok)

  def splitOneArgPerLineBeforeComma(tok: FormatToken)(implicit
      line: sourcecode.Line,
      style: ScalafmtConfig
  ): Policy = {
    val owner = tok.meta.leftOwner
    // TODO(olafur) clear queue between arguments, they are independent.
    Policy(matching(tok.left)) {
      case Decision(t @ FormatToken(_, _: T.Comma, _), splits)
          if owner == t.meta.rightOwner && !next(t).right.is[T.Comment] =>
        splits.map { x =>
          if (x.modification != NoSplit) x else x.copy(modification = Newline)
        }

      case Decision(t @ FormatToken(_: T.Comma, right, _), splits)
          if owner == t.meta.leftOwner &&
            !right.is[T.LeftBrace] &&
            // If comment is bound to comma, see unit/Comment.
            (!right.is[T.Comment] || t.hasBreak) =>
        val isNewline = right.is[T.Comment]
        splits.filter(_.modification.isNewline == isNewline)
    }
  }

  def splitOneArgPerLineAfterComma(tok: FormatToken)(implicit
      line: sourcecode.Line,
      style: ScalafmtConfig
  ): Policy = {
    val owner = tok.meta.leftOwner
    // TODO(olafur) clear queue between arguments, they are independent.
    Policy(matching(tok.left)) {
      // Newline on every comma.
      case Decision(t @ FormatToken(_: T.Comma, right, _), splits)
          if owner == t.meta.leftOwner &&
            // TODO(olafur) what the right { decides to be single line?
            // If comment is bound to comma, see unit/Comment.
            (!right.is[T.Comment] || t.hasBreak) =>
        if (!right.is[T.LeftBrace])
          splits.filter(_.modification.isNewline)
        else if (!style.activeForEdition_2020_03)
          splits
        else
          SplitTag.OneArgPerLine.activateOnly(splits)
    }
  }

  def UnindentAtExclude(
      exclude: Set[Token],
      indent: Length
  ): Policy.Pf = {
    case Decision(t, s) if exclude.contains(t.left) =>
      val close = matching(t.left)
      s.map(_.withIndent(indent, close, ExpiresOn.After))
  }

  def penalizeAllNewlines(
      expire: Token,
      penalty: Int,
      penalizeLambdas: Boolean = true,
      ignore: FormatToken => Boolean = _ => false,
      penaliseNewlinesInsideTokens: Boolean = false
  )(implicit line: sourcecode.Line): Policy =
    Policy(expire) {
      case Decision(tok, s)
          if tok.right.end < expire.end &&
            (penalizeLambdas || !tok.left.is[T.RightArrow]) && !ignore(tok) =>
        s.map {
          case split
              if split.modification.isNewline ||
                (penaliseNewlinesInsideTokens && tok.leftHasNewline) =>
            split.withPenalty(penalty)
          case x => x
        }
    }

  def penalizeNewlineByNesting(from: Token, to: Token)(implicit
      line: sourcecode.Line
  ): Policy = {
    val range = Range(from.start, to.end).inclusive
    Policy(to) {
      case Decision(t, s) if range.contains(t.right.start) =>
        val nonBoolPenalty =
          if (isBoolOperator(t.left)) 0
          else 5

        val penalty =
          nestedSelect(t.meta.leftOwner) + nestedApplies(t.meta.rightOwner) +
            nonBoolPenalty
        s.map {
          case split if split.modification.isNewline =>
            split.withPenalty(penalty)
          case x => x
        }
    }
  }

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
          val prev = tokens(els, -1)
          prev.left.is[T.RightBrace] && prev.meta.leftOwner != term
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
  def getSelectsLastToken(dot: T.Dot): FormatToken = {
    var curr = tokens(dot, 1)
    while (
      isOpenApply(
        curr.right,
        includeCurly = true,
        includeNoParens = true
      ) &&
      !statementStarts.contains(hash(curr.right))
    ) {
      if (curr.right.is[T.Dot]) {
        curr = tokens(curr, 2)
      } else {
        curr = tokens(matching(curr.right))
      }
    }
    curr
  }

  def getOptimalTokenFor(token: Token): Token =
    getOptimalTokenFor(tokens(token))

  def getOptimalTokenFor(ft: FormatToken): Token =
    if (isAttachedSingleLineComment(ft)) ft.right else ft.left

  def getSelectOptimalToken(tree: Tree): Token = {
    val lastDotOpt = findLast(tree.tokens)(_.is[T.Dot])
    if (lastDotOpt.isEmpty)
      throw new IllegalStateException(s"Missing . in select $tree")
    val lastDot = lastDotOpt.get.asInstanceOf[T.Dot]
    lastToken(getSelectsLastToken(lastDot).meta.leftOwner)
  }

  def infixIndent(
      app: InfixApp,
      formatToken: FormatToken,
      isNewline: Boolean
  )(implicit style: ScalafmtConfig): Int = {
    if (style.verticalAlignMultilineOperators)
      if (InfixApp.isAssignment(formatToken.left.syntax)) 2 else 0
    else if (
      !app.rhs.headOption.exists { x =>
        x.is[Term.Block] || x.is[Term.NewAnonymous]
      } && isInfixTopLevelMatch(app.all, formatToken.left.syntax, false)
    ) 2
    else if (isInfixTopLevelMatch(app.all, app.op.value, true)) 0
    else if (!isNewline && !isSingleLineComment(formatToken.right)) 0
    else if (
      (style.activeForEdition_2020_03 || style.newlines.formatInfix) &&
      app.all.is[Pat] && isChildOfCaseClause(app.all)
    ) 0
    else 2
  }

  private def isInfixTopLevelMatch(
      tree: Tree,
      op: String,
      noindent: Boolean
  )(implicit style: ScalafmtConfig) = {
    def isTopLevel = isTopLevelInfixApplication(tree)
    noindent == style.indentOperator.noindent(op) &&
    noindent == (noindent == style.unindentTopLevelOperators || isTopLevel)
  }

  def beforeInfixSplit(
      owner: Term.ApplyInfix,
      formatToken: FormatToken
  )(implicit line: sourcecode.Line, style: ScalafmtConfig): Seq[Split] = {
    val InfixApp(app) = owner
    infixSplitImpl(app, formatToken, true)
  }

  private def infixSplitImpl(
      app: InfixApp,
      formatToken: FormatToken,
      beforeLhs: Boolean
  )(implicit line: sourcecode.Line, style: ScalafmtConfig): Seq[Split] = {
    // NOTE. Silly workaround because we call infixSplit from assignment =, see #798
    val treeOpt =
      if (!beforeLhs && app.isRightAssoc)
        app.rhs.headOption.collectFirst {
          case InfixApp(ia) if ia.lhs.tokens.nonEmpty => ia.lhs
          case arg if arg.tokens.nonEmpty => arg
        }
      else
        findLast(app.rhs)(_.tokens.nonEmpty)
    val expire = treeOpt.getOrElse(app.all).tokens.last

    // we don't modify line breaks generally around infix expressions
    // TODO: if that ever changes, modify how rewrite rules handle infix
    val modification = getModCheckIndent(formatToken)
    val isNewline = modification.isNewline
    val indent = infixIndent(app, formatToken, isNewline)
    val split =
      Split(modification, 0).withIndent(Num(indent), expire, ExpiresOn.After)

    if (
      !style.activeForEdition_2020_01 || !beforeLhs ||
      isNewline || formatToken.right.is[T.Comment]
    )
      Seq(split)
    else {
      val altIndent = infixIndent(app, formatToken, true)
      Seq(
        split,
        Split(Newline, 1).withIndent(Num(altIndent), expire, ExpiresOn.After)
      )
    }
  }

  def insideInfixSplit(
      app: InfixApp,
      ft: FormatToken
  )(implicit line: sourcecode.Line, style: ScalafmtConfig): Seq[Split] =
    app.all match {
      case t: Type.ApplyInfix
          if style.spaces.neverAroundInfixTypes.contains(t.op.value) =>
        Seq(Split(NoSplit, 0))
      case _ if style.newlines.formatInfix =>
        if (ft.meta.leftOwner ne app.op) Seq(Split(Space, 0))
        else getInfixSplitsBeforeLhsOrRhs(app, ft, findEnclosingInfix(app))
      case _ => infixSplitImpl(app, ft, false)
    }

  @tailrec
  private def findEnclosingInfix(child: InfixApp): InfixApp =
    if (isEnclosedInMatching(child.all)) child
    else
      child.all.parent match {
        case Some(InfixApp(parent)) if !parent.isAssignment =>
          if ((child.all ne parent.lhs) && parent.rhs.length != 1) child
          else findEnclosingInfix(parent)
        case _ => child
      }

  def getInfixSplitsBeforeLhs(
      lhsApp: InfixApp,
      ft: FormatToken,
      newStmtModOrBody: Either[Modification, Tree]
  )(implicit style: ScalafmtConfig): Seq[Split] = {
    val fullInfixTreeOpt = findTreeWithParentSimple(lhsApp.all)(!isInfixApp(_))
    val fullInfix = fullInfixTreeOpt.flatMap(asInfixApp).getOrElse(lhsApp)
    val app = findLeftInfix(fullInfix)
    newStmtModOrBody.fold(
      x => getInfixSplitsBeforeLhsOrRhs(app, ft, fullInfix, Some(x)),
      x => {
        val indent = Indent(Num(2), x.tokens.last, ExpiresOn.After)
        getInfixSplitsBeforeLhsOrRhs(app, ft, fullInfix).map { s =>
          if (s.modification.isNewline) s.withIndent(indent) else s
        }
      }
    )
  }

  private def getInfixSplitsBeforeLhsOrRhs(
      app: InfixApp,
      ft: FormatToken,
      fullInfix: InfixApp,
      newStmtMod: Option[Modification] = None
  )(implicit style: ScalafmtConfig): Seq[Split] = {
    val beforeLhs = ft.meta.leftOwner ne app.op
    val maxPrecedence =
      if (beforeLhs) 0 // not used
      else infixSequenceMaxPrecedence(fullInfix)
    val closeOpt = matchingOpt(ft.right)
    val expiresOpt =
      if (closeOpt.isDefined) None
      else {
        val res = mutable.Buffer.empty[InfixApp]
        findNextInfixes(fullInfix.all, app.lhs, res)
        val infixes = if (beforeLhs) res.toSeq else res.toSeq.tail
        val filtered =
          if (!style.newlines.afterInfixBreakOnNested) infixes
          else
            infixes.takeWhile { x =>
              val close = lastToken(x.lhs)
              !close.is[T.RightParen] ||
              !isEnclosedInMatching(owners(close), matching(close), close)
            }
        if (filtered.isEmpty) None
        else {
          val res = filtered.foldLeft(Seq.empty[(T, Int)]) {
            case (out, ia) =>
              val cost = maxPrecedence - ia.precedence
              if (out.nonEmpty && out.last._2 <= cost) out
              else out :+ getMidInfixToken(ia) -> cost
          }
          Some(res)
        }
      }

    val breakPenalty = if (beforeLhs) 1 else (maxPrecedence - app.precedence)
    val fullExpire = getLastEnclosedToken(fullInfix.all)
    val expires = expiresOpt.fold(Seq(fullExpire -> 0)) { x =>
      if (x.last._2 == 0) x else x :+ fullExpire -> 0
    }
    val firstInfixOp = findLeftInfix(fullInfix).op
    val isFirst = beforeLhs || (firstInfixOp eq app.op)

    val infixTooLong = infixSequenceLength(fullInfix) >
      style.newlines.afterInfixMaxCountPerExprForSome
    val breakMany = infixTooLong ||
      style.newlines.breakAfterInfix == Newlines.AfterInfix.many
    val rightAsInfix = asInfixApp(ft.meta.rightOwner)

    val nlMod = newStmtMod.getOrElse(getModCheckIndent(ft, 1))
    val nlIndentLength = Num(if (beforeLhs) 0 else infixIndent(app, ft, true))
    val fullIndent = Indent(nlIndentLength, fullExpire, ExpiresOn.After)
    val nlIndent =
      if (isFirst || (fullIndent eq Indent.Empty)) fullIndent
      else new Indent.Before(fullIndent, firstInfixOp)
    val nlPolicy =
      if (nlIndent eq Indent.Empty) NoPolicy
      else
        Policy(fullExpire) {
          case Decision(t: FormatToken, s) if isInfixOp(t.meta.leftOwner) =>
            if (isSingleLineComment(t.right)) // will break
              s.map(_.switch(firstInfixOp))
            else
              s.map { x =>
                if (!x.modification.isNewline) x
                else x.switch(firstInfixOp)
              }
        }

    val singleLineExpire = if (isFirst) fullExpire else expires.head._1
    val singleLinePolicy =
      if (infixTooLong || !isFirst) None
      else Some(getSingleLineInfixPolicy(fullExpire))
    val nlSinglelineSplit = Split(nlMod, 0)
      .onlyIf(singleLinePolicy.isDefined && beforeLhs)
      .withIndent(nlIndentLength, singleLineExpire, ExpiresOn.After)
      .withSingleLine(singleLineExpire)
      .andThenPolicyOpt(singleLinePolicy)
    val spaceSingleLine = Split(Space, 0)
      .onlyIf(newStmtMod.isEmpty)
      .withSingleLine(singleLineExpire)
      .andThenPolicyOpt(singleLinePolicy)
    val singleLineSplits = Seq(
      spaceSingleLine.onlyFor(SplitTag.InfixChainNoNL),
      spaceSingleLine.onlyIf(singleLinePolicy.isDefined),
      nlSinglelineSplit
    )

    val otherSplits = closeOpt.fold {
      val nlSplit = Split(nlMod, 1 + breakPenalty)
      Seq(nlSplit.withIndent(nlIndent).withPolicy(nlPolicy))
    } { close =>
      val noSingleLine = newStmtMod.isDefined || breakMany ||
        rightAsInfix.exists(10 < infixSequenceLength(_))
      val nextOp =
        if (!style.newlines.afterInfixBreakOnNested) None
        else if (beforeLhs) Some(app.op)
        else
          getInfixRhsAsInfix(app) match {
            case Some(ia) => Some(findLeftInfix(ia).op)
            case _ => findNextInfixInParent(app.all, fullInfix.all)
          }
      val endOfNextOp = nextOp.map(_.tokens.last)
      val breakAfterClose = endOfNextOp.flatMap { tok =>
        val end = nextNonCommentSameLine(tokens(tok))
        if (end.right.is[T.LeftBrace]) None
        else Some(Policy.map(decideNewlinesOnlyAfterToken)(end.left))
      }

      val nlSplit = Split(nlMod, 0)
        .andThenPolicyOpt(breakAfterClose)
        .withIndent(nlIndent)
        .withPolicy(nlPolicy)
      val singleLineSplit = Split(Space, 0)
        .notIf(noSingleLine)
        .withSingleLine(endOfNextOp.getOrElse(close))
        .andThenPolicyOpt(breakAfterClose)
        .andThenPolicy(getSingleLineInfixPolicy(close))
      Seq(singleLineSplit, nlSplit)
    }

    val spaceSplits: Seq[Split] =
      if (ft.right.is[T.Comment]) Seq.empty
      else if (closeOpt.isDefined) Seq.empty
      else {
        val nextFT = if (rightAsInfix.isDefined) next(ft) else ft
        expires.filter(_._2 <= breakPenalty).takeRight(3).map {
          case (expire, cost) =>
            val exclude =
              if (breakMany) Set.empty[Range]
              else insideBlockRanges[LeftParenOrBrace](nextFT, expire)
            Split(newStmtMod.getOrElse(Space), cost)
              .withSingleLine(expire, exclude)
        }
      }

    singleLineSplits ++ spaceSplits ++ otherSplits
  }

  def getSingleLineInfixPolicy(end: Token) =
    Policy(end) {
      case Decision(t: FormatToken, s) if isInfixOp(t.meta.leftOwner) =>
        SplitTag.InfixChainNoNL.activateOnly(s)
    }

  def getMidInfixToken(app: InfixApp): Token = {
    val opToken = app.op.tokens.head
    val opFollowsComment = tokens(opToken, -1).left.is[T.Comment]
    if (opFollowsComment) lastToken(app.lhs) else opToken
  }

  private def getLastEnclosedToken(tree: Tree): Token = {
    val tokens = tree.tokens
    lastToken(if (isEnclosedInMatching(tree)) tokens.dropRight(1) else tokens)
  }

  @tailrec
  private def findNextInfixes(
      fullTree: Tree,
      tree: Tree,
      res: mutable.Buffer[InfixApp]
  ): Unit =
    tree.parent match {
      case Some(p @ InfixApp(ia)) if tree ne fullTree =>
        if (ia.lhs eq tree) findNestedInfixes(ia.rhs, res += ia)
        findNextInfixes(fullTree, p, res)
      case _ =>
    }

  private def findNestedInfixes(
      tree: Tree,
      res: mutable.Buffer[InfixApp]
  ): Unit =
    if (!isEnclosedInMatching(tree)) {
      asInfixApp(tree).foreach { ia =>
        findNestedInfixes(ia.lhs, res)
        res += ia
        findNestedInfixes(ia.rhs, res)
      }
    }
  private def findNestedInfixes(
      trees: Seq[Tree],
      res: mutable.Buffer[InfixApp]
  ): Unit =
    // multiple RHS parameters are always enclosed
    if (trees.length == 1) findNestedInfixes(trees.head, res)

  @tailrec
  final def findLeftInfix(app: InfixApp): InfixApp =
    app.lhs match {
      case t @ InfixApp(ia) if !isEnclosedInMatching(t) =>
        findLeftInfix(ia)
      case _ => app
    }

  private def getInfixRhsAsInfix(app: InfixApp): Option[InfixApp] =
    app.rhs match {
      case Seq(t @ InfixApp(ia)) if !isEnclosedInMatching(t) => Some(ia)
      case _ => None // multiple parameters to infix are always enclosed
    }

  private def infixSequenceMaxPrecedence(app: InfixApp): Int = {
    val queue = new mutable.Queue[InfixApp]()
    queue += app
    var maxPrecedence = 0
    while (queue.nonEmpty) {
      val elem = queue.dequeue()
      if (maxPrecedence < elem.precedence)
        maxPrecedence = elem.precedence
      queue ++= (elem.lhs +: elem.rhs).collect {
        case t @ InfixApp(ia) if !isEnclosedInMatching(t) => ia
      }
    }
    maxPrecedence
  }

  def isEmptyFunctionBody(tree: Tree): Boolean =
    tree match {
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
        rtoks.tail.find(!_.is[Whitespace]).get -> ExpiresOn.After
      else
        last -> ExpiresOn.After
    }
    def dropComment(rtoks: Seq[Token]) =
      if (rtoks.head.is[T.Comment]) dropWS(rtoks.tail) else rtoks

    def getRToks = dropWS(function.tokens.reverse)
    function.parent match {
      case Some(b: Term.Block) if b.stats.length == 1 =>
        b.tokens.last -> ExpiresOn.Before
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

  def opensConfigStyle(
      ft: => FormatToken,
      whenSourceIgnored: Boolean
  )(implicit style: ScalafmtConfig): Boolean =
    if (style.newlines.sourceIgnored) whenSourceIgnored
    else opensConfigStyleClassic(ft)

  private def opensConfigStyleClassic(
      ft: FormatToken
  )(implicit style: ScalafmtConfig): Boolean = {
    def opensImplicit =
      (style.newlines.forceAfterImplicitParamListModifier ||
        next(ft).hasBreak) && opensConfigStyleImplicitParamList(ft)
    (ft.hasBreak || opensImplicit) && {
      val close = matching(ft.left)
      tokens(close, -1).hasBreak
    }
  }

  def opensConfigStyleImplicitParamList(
      formatToken: FormatToken
  )(implicit style: ScalafmtConfig): Boolean =
    style.activeForEdition_2020_03 &&
      formatToken.right.is[T.KwImplicit] &&
      style.newlines.notBeforeImplicitParamListModifier &&
      opensImplicitParamList(formatToken).isDefined

  def styleAt(tree: Tree): ScalafmtConfig = {
    val style = styleMap.at(tree.tokens.head)
    if (styleMap.forcedBinPack(tree)) // off-by-one
      styleMap.setBinPack(style, callSite = true)
    else style
  }

  def getApplyIndent(
      leftOwner: Tree,
      isConfigStyle: Boolean = false
  )(implicit style: ScalafmtConfig): Num =
    leftOwner match {
      case _: Pat
          if !style.activeForEdition_2020_03 &&
            leftOwner.parent.exists(_.is[Case]) =>
        // The first layer of indentation is provided by the case ensure
        // orpan comments and the case cond is indented correctly.
        Num(0)
      case x if isDefnSite(x) && !x.isInstanceOf[Type.Apply] =>
        if (style.binPack.unsafeDefnSite && !isConfigStyle) Num(0)
        else Num(style.continuationIndent.getDefnSite(x))
      case _ => Num(style.continuationIndent.callSite)
    }

  def isBinPack(owner: Tree): Boolean = {
    implicit val style = styleAt(owner)
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

  def ctorWithChain(ownerSet: Set[Tree], lastToken: Token): Policy =
    if (styleMap.at(tokens(lastToken)).binPack.parentConstructors)
      NoPolicy
    else
      Policy(lastToken) {
        case d @ Decision(t @ FormatToken(_, _: T.KwWith, _), _)
            if ownerSet.contains(t.meta.rightOwner) =>
          d.onlyNewlinesWithoutFallback
      }

  def binPackParentConstructorSplits(
      owners: Set[Tree],
      lastToken: Token,
      indent: Int
  )(implicit line: sourcecode.Line): Seq[Split] = {
    Seq(
      Split(Space, 0)
        .withPolicy(SingleLineBlock(lastToken))
        .withIndent(Num(indent), lastToken, ExpiresOn.After),
      Split(NewlineT(acceptSpace = true), 1)
        .withPolicy(ctorWithChain(owners, lastToken))
        .withIndent(Num(indent), lastToken, ExpiresOn.After)
    )
  }

  def delayedBreakPolicy(
      leftCheck: Option[Token => Boolean]
  )(onBreakPolicy: Policy)(implicit line: sourcecode.Line): Policy = {
    object OnBreakDecision {
      def unapply(d: Decision): Option[Seq[Split]] =
        if (leftCheck.exists(!_(d.formatToken.left))) None
        else unapplyImpl(d)
      private def unapplyImpl(d: Decision): Option[Seq[Split]] = {
        var replaced = false
        def decisionPf(s: Split): Split =
          if (!s.modification.isNewline) s
          else {
            replaced = true
            s.orElsePolicy(onBreakPolicy)
          }
        val splits = d.splits.map(decisionPf)
        if (replaced) Some(splits) else None
      }
    }
    if (onBreakPolicy.isEmpty) onBreakPolicy
    else onBreakPolicy.copy(f = { case OnBreakDecision(d) => d })
  }

  def newlinesOnlyBeforeClosePolicy(close: Token)(implicit
      line: sourcecode.Line
  ): Policy =
    Policy.map(decideNewlinesOnlyBeforeClose(Split(Newline, 0)))(close)

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
          app.args.foreach { arg => clearQueues += hash(arg.tokens.head) }
        case _ =>
      }
      (forces.result(), clearQueues.result())
    }
  }

  /**
    * Implementation for `verticalMultiline`
    */
  def verticalMultiline(owner: Tree, ft: FormatToken)(implicit
      style: ScalafmtConfig
  ): Seq[Split] = {

    val FormatToken(open, r, _) = ft
    val close = matching(open)
    val indentParam = Num(style.continuationIndent.getDefnSite(owner))
    val indentSep = Num((indentParam.n - 2).max(0))
    val isBracket = open.is[T.LeftBracket]

    @tailrec
    def loop(token: Token): Option[FormatToken] = {
      tokens(matching(token)) match {
        case FormatToken(RightParenOrBracket(), l @ T.LeftParen(), _) =>
          loop(l)
        case f @ FormatToken(RightParenOrBracket(), right, _) =>
          lazy val isCtorModifier =
            f.meta.rightOwner.parent.exists(_.is[meta.Ctor])
          right match {
            // modifier for constructor if class definition has type parameters: [class A[T, K, C] private (a: Int)]
            case Modifier() if isCtorModifier =>
              // This case only applies to classes
              next(f).right match {
                case x @ (_: T.LeftParen | _: T.LeftBracket) =>
                  loop(x)
                case _ =>
                  Some(f)
              }
            case _ =>
              Some(f)
          }
        case _ => None
      }
    }

    // find the last param on the defn so that we can apply our `policy`
    // till the end.
    val lastParenFt = loop(open).get
    val lastParen = lastParenFt.left

    val mixedParams = {
      owner match {
        case cls: Defn.Class =>
          cls.tparams.nonEmpty && cls.ctor.paramss.nonEmpty
        case _ => false
      }
    }

    val shouldNotDangle = shouldNotDangleAtDefnSite(owner, true)

    // Since classes and defs aren't the same (see below), we need to
    // create two (2) OneArgOneLineSplit when dealing with classes. One
    // deals with the type params and the other with the value params.
    val oneLinePerArg = {
      val base = OneArgOneLineSplit(ft)
      if (mixedParams) {
        val afterTypes = tokens(matching(open))
        // Try to find the first paren. If found, then we are dealing with
        // a class with type AND value params. Otherwise it is a class with
        // just type params.
        findFirst(afterTypes, lastParen)(t => t.left.is[T.LeftParen])
          .fold(base)(t => base.orElse(OneArgOneLineSplit(t)))
      } else base
    }

    // DESNOTE(2017-03-28, pjrt) Classes and defs aren't the same.
    // For defs, type params and value param have the same `owners`. However
    // this is not the case for classes. Type params have the class itself
    // as the owner, but value params have the Ctor as the owner, so a
    // simple check isn't enough. Instead we check against the owner of the
    // `lastParen` as well, which will be the same as the value param's
    // owner.
    val valueParamsOwner = lastParenFt.meta.leftOwner
    @inline def ownerCheck(rpOwner: Tree): Boolean = {
      rpOwner == owner || rpOwner == valueParamsOwner
    }

    val paramGroupSplitter: Policy.Pf = {
      // If this is a class, then don't dangle the last paren unless the line ends with a comment
      case Decision(t @ FormatToken(previous, rp @ RightParenOrBracket(), _), _)
          if shouldNotDangle && rp == lastParen && !isSingleLineComment(
            previous
          ) =>
        Seq(Split(NoSplit, 0))
      // Indent seperators `)(` and `](` by `indentSep`
      case Decision(t @ FormatToken(_, rp @ RightParenOrBracket(), _), _)
          if ownerCheck(t.meta.rightOwner) =>
        Seq(Split(Newline, 0).withIndent(indentSep, rp, ExpiresOn.After))
      // Add a newline after left paren if:
      // - There's an implicit keyword and newlineBeforeImplicitKW is enabled
      // - newlineAfterOpenParen is enabled
      // - Mixed-params case with constructor modifier `] private (`
      case Decision(t @ FormatToken(open2 @ T.LeftParen(), right, _), _) =>
        val close2 = matching(open2)

        // We don't want to create newlines for default values.
        def isDefinition = ownerCheck(owners(close2))

        val shouldAddNewline = {
          if (right.is[T.KwImplicit])
            style.newlines.forceBeforeImplicitParamListModifier ||
            style.verticalMultiline.newlineBeforeImplicitKW
          else
            style.verticalMultiline.newlineAfterOpenParen && isDefinition
        } || (mixedParams && prev(t).meta.leftOwner.is[CtorModifier])

        Seq(
          Split(NoSplit.orNL(!shouldAddNewline), 0)
            .withIndent(indentParam, close2, ExpiresOn.Before)
        )
      case Decision(t @ FormatToken(T.KwImplicit(), _, _), _)
          if style.newlines.forceAfterImplicitParamListModifier ||
            style.verticalMultiline.newlineAfterImplicitKW =>
        Seq(Split(Newline, 0))
    }

    // Our policy is a combination of OneArgLineSplit and a custom splitter
    // for parameter groups.
    val policy = oneLinePerArg.orElse(paramGroupSplitter, lastParen.end)

    val firstIndent =
      if (r.is[T.RightParen]) // An empty param group
        Indent(indentSep, close, ExpiresOn.After)
      else
        Indent(indentParam, close, ExpiresOn.Before)

    val singleLineExpire =
      if (isBracket) close // If we can fit the type params, make it so
      else lastParen // If we can fit all in one block, make it so

    def maxArity =
      valueParamsOwner match {
        case d: Decl.Def if d.paramss.nonEmpty => d.paramss.map(_.size).max
        case d: Defn.Def if d.paramss.nonEmpty => d.paramss.map(_.size).max
        case m: Defn.Macro if m.paramss.nonEmpty => m.paramss.map(_.size).max
        case c: Ctor.Primary if c.paramss.nonEmpty => c.paramss.map(_.size).max
        case c: Ctor.Secondary if c.paramss.nonEmpty =>
          c.paramss.map(_.size).max
        case _ => 0
      }

    def configStyle =
      style.optIn.configStyleArguments &&
        style.activeForEdition_2020_03 && ft.hasBreak

    def belowArityThreshold =
      maxArity < style.verticalMultiline.arityThreshold

    Seq(
      Split(Space(style.spaces.inParentheses), 0)
        .onlyIf(isBracket || !configStyle && belowArityThreshold)
        .withPolicy(SingleLineBlock(singleLineExpire)),
      // Otherwise split vertically
      Split(Newline, 1).withIndent(firstIndent).withPolicy(policy)
    )

  }

  // Returns leading comment, if there exists one, otherwise formatToken.right
  final def leadingComment(formatToken: FormatToken): FormatToken =
    findToken(formatToken, prev) { formatToken =>
      formatToken.hasBlankLine || !formatToken.left.is[T.Comment]
    }.fold(identity, identity)

  def xmlSpace(owner: Tree): Modification =
    owner match {
      case _: Term.Xml | _: Pat.Xml => NoSplit
      case _ => Space
    }

  def getSpaceAndNewlineAfterCurlyLambda(
      newlines: Int
  )(implicit style: ScalafmtConfig): (Boolean, NewlineT) =
    style.newlines.afterCurlyLambda match {
      case NewlineCurlyLambda.squash => (true, Newline)
      case NewlineCurlyLambda.never =>
        val space = style.newlines.source match {
          case Newlines.fold => true
          case Newlines.unfold => false
          case _ => newlines == 0
        }
        (space, Newline)
      case NewlineCurlyLambda.always => (false, Newline2x)
      case NewlineCurlyLambda.preserve =>
        (newlines == 0, if (newlines >= 2) Newline2x else Newline)
    }

  def getNoSplit(
      ft: FormatToken,
      spaceOk: Boolean
  )(implicit style: ScalafmtConfig): Modification =
    ft.right match {
      case c: T.Comment =>
        val isDetachedSlc = ft.hasBreak && isSingleLineComment(c)
        if (isDetachedSlc || next(ft).leftHasNewline) null else Space
      case _ =>
        Space(style.spaces.inParentheses && spaceOk)
    }

  def getLambdaAtSingleArgCallSite(
      ft: FormatToken
  )(implicit style: ScalafmtConfig): Option[Term.Function] =
    ft.meta.leftOwner match {
      case Term.Apply(_, List(fun: Term.Function)) => Some(fun)
      case fun: Term.Function if fun.parent.exists({
            case Term.ApplyInfix(_, _, _, List(`fun`)) => true
            case _ => false
          }) =>
        Some(fun)
      case t: Init if style.activeForEdition_2020_01 =>
        findArgsFor(ft.left, t.argss).collect {
          case List(f: Term.Function) => f
        }
      case _ => None
    }

  def findArgsFor[A <: Tree](
      token: Token,
      argss: Seq[Seq[A]]
  ): Option[Seq[A]] =
    TokenOps.findArgsFor(token, argss, matchingParentheses)

  // look for arrow before body, if any, else after params
  def getFuncArrow(term: Term.Function): Option[FormatToken] =
    term.body.tokens.headOption
      .map(x => prevNonComment(tokens(x, -1)))
      .orElse {
        val lastParam = term.params.lastOption
        lastParam.flatMap(_.tokens.lastOption).map { x =>
          val maybeArrow = tokens(nextNonComment(tokens(x)), 1)
          if (maybeArrow.left.is[T.RightArrow]) maybeArrow
          else tokens(nextNonComment(maybeArrow), 1)
        }
      }

  // look for arrow before body, if any, else after cond/pat
  def getCaseArrow(term: Case): FormatToken =
    term.body.tokens.headOption.fold {
      val endOfPat = term.cond.getOrElse(term.pat).tokens.last
      val maybeArrow = tokens(nextNonComment(tokens(endOfPat)), 1)
      if (maybeArrow.left.is[T.RightArrow]) maybeArrow
      else tokens(nextNonComment(maybeArrow), 1)
    }(x => prevNonComment(tokens(x, -1)))

  def getApplyArgs(
      ft: FormatToken,
      isRight: Boolean
  )(implicit style: ScalafmtConfig): (Tree, Seq[Tree]) = {
    val paren = if (isRight) ft.right else ft.left
    val owner = if (isRight) ft.meta.rightOwner else ft.meta.leftOwner
    def getArgs(argss: Seq[Seq[Tree]]): Seq[Tree] =
      if (!style.activeForEdition_2020_03) argss.flatten
      else findArgsFor(paren, argss).getOrElse(Seq.empty)
    owner match {
      case InfixApp(ia) if style.newlines.formatInfix => (ia.op, ia.rhs)
      case t @ SplitDefnIntoParts(_, name, tparams, paramss) =>
        if (if (isRight) paren.is[T.RightParen] else paren.is[T.LeftParen])
          (name, getArgs(paramss))
        else {
          // XXX: backwards-compatible hack
          val useTParams = t.is[Defn.Def] ||
            t.is[Type.Param] || t.is[Decl.Type] || t.is[Defn.Type]
          (name, if (useTParams) tparams else paramss.flatten)
        }
      case SplitCallIntoParts(tree, either) =>
        either match {
          case Left(args) => (tree, args)
          case Right(argss) => (tree, getArgs(argss))
        }
      case _ =>
        logger.debug(s"""Unknown tree
          |${log(owner.parent.get)}
          |${isDefnSite(owner)}""".stripMargin)
        throw UnexpectedTree[Term.Apply](owner)
    }
  }

  def opensImplicitParamList(ft: FormatToken): Option[Seq[Term.Param]] = {
    val paramsOpt = splitDefnIntoParts.lift(ft.meta.leftOwner).flatMap {
      case (_, _, _, paramss) =>
        findArgsFor(ft.left, paramss)
    }
    // make sure there's no other param with implicit
    paramsOpt.filter(!_.exists(TreeOps.hasExplicitImplicit))
  }

  def opensImplicitParamList(ft: FormatToken, args: Seq[Tree]): Boolean =
    ft.right.is[T.KwImplicit] && args.forall {
      case t: Term.Param => !hasExplicitImplicit(t)
      case _ => true
    }

  def isEnclosedInMatching(tree: Tree, open: T, close: T): Boolean =
    tree.tokens.headOption.contains(open) && (tree.tokens.last eq close)

  def getClosingIfEnclosedInMatching(tree: Tree): Option[T] =
    tree.tokens.lastOption.filter(matchingOpt(_).contains(tree.tokens.head))

  def isEnclosedInMatching(tree: Tree): Boolean =
    getClosingIfEnclosedInMatching(tree).isDefined

  @tailrec
  final def findPrevSelect(tree: Tree): Option[Term.Select] =
    tree match {
      case t: Term.Select => Some(t)
      case t @ SplitCallIntoParts(fun, _) if t ne fun =>
        if (isEnclosedInMatching(t)) None else findPrevSelect(fun)
      case _ => None
    }
  def findPrevSelect(tree: Term.Select): Option[Term.Select] =
    findPrevSelect(tree.qual)

  @tailrec
  final def findLastApplyAndNextSelect(
      tree: Tree,
      select: Option[Term.Select] = None
  ): (Tree, Option[Term.Select]) =
    tree.parent match {
      case Some(p: Term.Select) =>
        findLastApplyAndNextSelect(p, select.orElse(Some(p)))
      case Some(p @ SplitCallIntoParts(`tree`, _)) =>
        if (isEnclosedInMatching(p)) (p, select)
        else findLastApplyAndNextSelect(p, select)
      case _ => (tree, select)
    }

  @tailrec
  final def findTokenWith[A](
      ft: FormatToken,
      iter: FormatToken => FormatToken
  )(f: FormatToken => Option[A]): Either[FormatToken, A] =
    f(ft) match {
      case Some(a) => Right(a)
      case _ =>
        val nextFt = iter(ft)
        if (nextFt eq ft) Left(ft)
        else findTokenWith(nextFt, iter)(f)
    }

  final def findToken(
      ft: FormatToken,
      iter: FormatToken => FormatToken
  )(f: FormatToken => Boolean): Either[FormatToken, FormatToken] =
    findTokenWith(ft, iter)(Some(_).filter(f))

}
