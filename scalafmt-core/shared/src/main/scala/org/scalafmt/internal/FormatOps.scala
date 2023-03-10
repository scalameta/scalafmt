package org.scalafmt.internal

import org.scalafmt.Error.UnexpectedTree
import org.scalafmt.config.{
  BinPack,
  IndentOperator,
  Newlines,
  ScalafmtConfig,
  ScalafmtRunner,
  TrailingCommas
}
import org.scalafmt.internal.Length.Num
import org.scalafmt.internal.Policy.NoPolicy
import org.scalafmt.util._
import org.scalafmt.util.InfixApp._
import org.scalafmt.util.LoggerOps._
import org.scalameta.FileLine

import scala.annotation.tailrec
import scala.collection.mutable
import scala.meta.classifiers.Classifier
import scala.meta._
import scala.meta.tokens.{Token => T}

/** Helper functions for generating splits/policies for a given tree.
  */
class FormatOps(
    val topSourceTree: Tree,
    baseStyle: ScalafmtConfig,
    val filename: String = ""
) {
  import FormatOps._
  import PolicyOps._
  import TokenOps._
  import TreeOps._

  private val (initStyle, ownersMap) =
    getStyleAndOwners(topSourceTree, baseStyle)

  val runner: ScalafmtRunner = initStyle.runner
  implicit val dialect = initStyle.dialect
  val (tokens, styleMap) = FormatTokens(topSourceTree.tokens, owners)(initStyle)
  import tokens.{
    matching,
    matchingOpt,
    isEnclosedInMatching,
    isEnclosedInParens,
    findTokenWith,
    tokenBefore,
    tokenAfter,
    prev,
    next,
    prevNonComment,
    prevNonCommentBefore,
    nextNonComment,
    nextNonCommentSameLine
  }
  private val usedTokens = tokens.head.left +: tokens.map(_.right)

  private[internal] val soft = new SoftKeywordClasses(dialect)
  private[internal] val statementStarts =
    getStatementStarts(topSourceTree, tokens, soft)
  // Maps token to number of non-whitespace bytes before the token's position.
  private final val nonWhitespaceOffset: Map[T, Int] = {
    val resultB = Map.newBuilder[T, Int]
    var curr = 0
    usedTokens.foreach { t =>
      resultB += (t -> curr)
      curr += (t.end - t.start)
    }
    resultB.result()
  }

  val (forceConfigStyle, emptyQueueSpots) = getForceConfigStyle

  @inline
  def owners(token: T): Tree = ownersMap(hash(token))
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
  val (argumentStarts, optionalNewlines) = {
    val arguments = mutable.Map.empty[TokenHash, Tree]
    val optional = Set.newBuilder[TokenHash]
    def getHeadHash(tree: Tree): Option[TokenHash] =
      tokens.getHeadOpt(tree).map(x => hash(x.left))
    def add(tree: Tree): Unit =
      getHeadHash(tree).foreach { x =>
        if (!arguments.contains(x)) arguments += x -> tree
      }
    def addOptional(tree: Tree): Unit =
      getHeadHash(tree).foreach { optional += _ }

    val queue = new mutable.ListBuffer[Seq[Tree]]
    queue += topSourceTree :: Nil
    while (queue.nonEmpty) queue.remove(0).foreach { tree =>
      tree match {
        case _: Lit.Unit | _: Member.SyntaxValuesClause =>
        case t: Term.Param =>
          add(t)
          t.mods.foreach(addOptional)
          addOptional(t.name)
        case t: Term => add(t)
        case t: Pat.Extract => t.argClause.values.foreach(add)
        case _ =>
      }
      queue += tree.children
    }
    (arguments.toMap, optional.result())
  }

  class ExtractFromMeta[A](f: FormatToken => Option[A]) {
    def unapply(meta: FormatToken.Meta): Option[A] = f(tokens(meta.idx))
  }

  @inline
  final def findFirst(start: FormatToken, end: T)(
      f: FormatToken => Boolean
  ): Option[FormatToken] = {
    findFirst(start, end.end)(f)
  }

  @tailrec
  final def findFirst(start: FormatToken, end: Int)(
      f: FormatToken => Boolean
  ): Option[FormatToken] = {
    if (start.left.end >= end) None
    else if (f(start)) Some(start)
    else {
      val next_ = next(start)
      if (next_ == start) None
      else findFirst(next_, end)(f)
    }
  }

  def findFirstOnRight[A](start: FormatToken, end: T)(implicit
      classifier: Classifier[T, A]
  ): Option[T] =
    findFirst(start, end.start)(x => classifier(x.right)).map(_.right)

  final def rhsOptimalToken(
      start: FormatToken,
      end: Int = Int.MaxValue
  ): T =
    findTokenWith(start, next) { start =>
      start.right match {
        case t if t.end >= end => Some(start.left)
        case _ if start.hasBlankLine => Some(start.left)
        case _: T.RightParen
            if start.left.is[T.RightParen] || start.left.is[T.LeftParen] =>
          None
        case _: T.RightBracket if start.left.is[T.RightBracket] => None
        case _: T.Comma | _: T.LeftParen | _: T.Semicolon | _: T.RightArrow |
            _: T.Equals if isInfixRhs(start) || !startsNewBlockOnRight(start) =>
          None
        case c: T.Comment
            if start.noBreak && (!start.left.is[T.LeftParen] ||
              tokens.isBreakAfterRight(start)) =>
          Some(c)
        case _ => Some(start.left)
      }
    }.fold(_.right, identity)

  @tailrec
  final def endOfSingleLineBlock(
      start: FormatToken
  )(implicit style: ScalafmtConfig): T = {
    lazy val isInfix = isInfixRhs(start)
    val endFound = start.right match {
      case _: T.Comma | _: T.LeftParen | _: T.Semicolon | _: T.RightArrow |
          _: T.Equals =>
        None
      case _: T.RightParen if start.left.is[T.LeftParen] => None
      case c: T.Comment if start.noBreak && tokens.isBreakAfterRight(start) =>
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
    @inline def checkToken = tokens.tokenJustBeforeOpt(tree).contains(ft)
    tree.parent.exists {
      case ia: Member.Infix =>
        (ia.op eq tree) || (ia.arg eq tree) &&
        (ft.right.is[T.LeftParen] || checkToken)
      case t: Member.ArgClause =>
        t.parent.exists(_.is[Member.Infix]) && checkToken
      case _ => false
    }
  }

  final def startsNewBlockOnRight(ft: FormatToken): Boolean =
    ft.meta.rightOwner match {
      case _: Member.ArgClause => false
      case t => tokens.tokenBeforeOpt(t).contains(ft)
    }

  /** js.native is very special in Scala.js.
    *
    * Context: https://github.com/scalameta/scalafmt/issues/108
    */
  def isJsNative(body: Tree): Boolean =
    initStyle.newlines.neverBeforeJsNative && (body match {
      case Term.Select(Term.Name("js"), Term.Name("native")) => true
      case _ => false
    })

  @inline
  final def startsStatement(tok: FormatToken): Option[Tree] =
    startsStatement(tok.right)
  @inline
  final def startsStatement(token: T): Option[Tree] =
    statementStarts.get(hash(token))
  val StartsStatementRight = new ExtractFromMeta[Tree](startsStatement)

  def parensTuple(token: T): TokenRanges =
    matchingOpt(token).fold(TokenRanges.empty) { other =>
      TokenRanges(TokenRange(token, other))
    }

  def insideBlock[A](start: FormatToken, end: T)(implicit
      classifier: Classifier[T, A]
  ): TokenRanges =
    insideBlock(start, end, x => classifier(x.left))

  def insideBlock(
      start: FormatToken,
      end: T,
      matches: FormatToken => Boolean
  ): TokenRanges =
    insideBlock { x =>
      if (matches(x)) matchingOpt(x.left) else None
    }(start, end)

  def insideBracesBlock(
      start: FormatToken,
      end: T,
      parensToo: Boolean = false
  )(implicit style: ScalafmtConfig): TokenRanges =
    insideBlock(x => getEndOfBlock(x, parensToo))(start, end)

  def insideBlock(matches: FormatToken => Option[T])(
      start: FormatToken,
      end: T
  ): TokenRanges = {
    var result = TokenRanges.empty

    @tailrec
    def run(tok: FormatToken): Unit =
      if (tok.left.start < end.start) {
        val nextTokOpt = matches(tok).flatMap { close =>
          val open = tok.left
          if (open.start >= close.end) None
          else {
            result = result.append(TokenRange(open, close))
            Some(tokens(close))
          }
        }
        val nextTok = nextTokOpt.getOrElse(next(tok))
        if (nextTok ne tok) run(nextTok)
      }

    run(next(start))
    result
  }

  // invoked on closing paren, part of ParamClause
  @tailrec
  final def defnSiteLastToken(close: FormatToken, t: Tree): T = {
    def alt = getLastToken(t)
    t match {
      case _: Term.ParamClause | _: Type.FuncParamClause |
          _: Member.ParamClauseGroup =>
        t.parent match {
          case Some(p) => defnSiteLastToken(close, p)
          case _ => alt
        }
      case t: Defn.Def =>
        tokens.getHeadOpt(t.body).fold(alt) { ft =>
          if (ft.left.is[T.LeftBrace] && t.body.is[Term.Block]) ft.left
          else prevNonCommentBefore(ft).left
        }
      case _: Ctor.Primary =>
        close match {
          // This is a terrible terrible hack. Please consider removing this.
          // The RightParen() LeftBrace() pair is presumably a ") {" combination
          // at a class definition
          case FormatToken(_: T.RightParen, b: T.LeftBrace, _) => b
          case _ => close.left
        }
      case t =>
        t.tokens.find(x => x.is[T.Equals] && owners(x) == t).getOrElse(alt)
    }
  }

  @inline
  def splitOneArgOneLine(close: T, owner: Tree)(implicit
      fileLine: FileLine,
      style: ScalafmtConfig
  ): Policy = {
    val pf =
      if (style.poorMansTrailingCommasInConfigStyle)
        splitOneArgPerLineBeforeComma(owner)
      else
        splitOneArgPerLineAfterComma(owner)
    Policy.before(close)(pf)
  }

  def splitOneArgPerLineBeforeComma(owner: Tree): Policy.Pf = {
    // TODO(olafur) clear queue between arguments, they are independent.
    case Decision(t @ FormatToken(_, _: T.Comma, _), splits)
        if owner == t.meta.rightOwner && !next(t).right.is[T.Comment] =>
      splits.map(x => if (x.modExt.mod ne NoSplit) x else x.withMod(Newline))

    case Decision(t @ FormatToken(_: T.Comma, right, _), splits)
        if owner == t.meta.leftOwner &&
          !right.is[T.LeftBrace] &&
          // If comment is bound to comma, see unit/Comment.
          (!right.is[T.Comment] || t.hasBreak) =>
      val isNewline = right.is[T.Comment]
      splits.filter(_.isNL == isNewline)
  }

  def splitOneArgPerLineAfterComma(owner: Tree): Policy.Pf = {
    // Newline on every comma.
    case Decision(t @ FormatToken(_: T.Comma, right, m), splits)
        if owner == m.leftOwner &&
          // TODO(olafur) what the right { decides to be single line?
          // If comment is bound to comma, see unit/Comment.
          (!right.is[T.Comment] || t.hasBreak) =>
      getOneArgPerLineSplitsAfterComma(right, splits)
  }

  def splitOneArgPerLineAfterCommaOnBreak(comma: T): Policy =
    delayedBreakPolicyBefore(comma) {
      Policy.after(comma) {
        case Decision(t @ FormatToken(`comma`, right, _), splits)
            if !right.is[T.Comment] || t.hasBreak =>
          getOneArgPerLineSplitsAfterComma(right, splits)
      }
    }

  private def getOneArgPerLineSplitsAfterComma(r: T, s: Seq[Split]) =
    if (r.is[T.LeftBrace]) SplitTag.OneArgPerLine.activateOnly(s)
    else Decision.onlyNewlineSplits(s)

  def penalizeNewlineByNesting(from: T, to: T)(implicit
      fileLine: FileLine
  ): Policy = {
    val policy = Policy.before(to) { case Decision(FormatToken(l, _, m), s) =>
      val nonBoolPenalty = if (isBoolOperator(l)) 0 else 5
      val penalty = nestedSelect(m.leftOwner) +
        nestedApplies(m.rightOwner) + nonBoolPenalty
      s.map { x => if (x.isNL) x.withPenalty(penalty) else x }
    }
    new Policy.Delay(policy, Policy.End.Before(from))
  }

  def templateCurlyFt(template: Template): Option[FormatToken] =
    getStartOfTemplateBody(template).map(tokenBefore)

  def templateCurly(template: Template): Option[T] =
    templateCurlyFt(template).map(_.left)

  def templateCurlyOrLastNonTrivial(template: Template): T =
    templateCurly(template).getOrElse(getLastNonTrivialToken(template))

  def templateDerivesOrCurlyOrLastNonTrivial(
      template: Template
  )(implicit ft: FormatToken): T =
    findTemplateGroupOnRight(_.getExpireToken)(template)
      .getOrElse(templateCurlyOrLastNonTrivial(template))

  private def findTreeInGroup[A](
      trees: Seq[Tree],
      func: TemplateSupertypeGroup => A
  )(expireFunc: Seq[Tree] => T)(implicit ft: FormatToken): Option[A] =
    trees.find(_.pos.end >= ft.right.end).map { x =>
      func(TemplateSupertypeGroup(x, trees, expireFunc))
    }

  def findTemplateGroupOnRight[A](
      func: TemplateSupertypeGroup => A
  )(template: Template)(implicit ft: FormatToken): Option[A] = {
    @tailrec
    def iter(groups: Seq[Seq[Tree]]): Option[A] =
      if (isSeqSingle(groups))
        // for the last group, return '{' or ':'
        findTreeInGroup(groups.head, func) { x =>
          templateCurly(template).getOrElse(getLastNonTrivialToken(x.last))
        }
      else {
        // for intermediate groups, return its last token
        val res = findTreeInGroup(groups.head, func)(x => tokenAfter(x).left)
        if (res.isDefined) res else iter(groups.tail)
      }
    getTemplateGroups(template).flatMap(iter)
  }

  @inline
  def getBreakBeforeElsePolicy(term: Term.If): Policy =
    getElseChain(term, Nil).foldLeft(Policy.noPolicy) { case (res, els) =>
      val policy = Policy.on(els) {
        case d @ Decision(FormatToken(_, `els`, _), _) =>
          d.onlyNewlinesWithFallback(Split(Newline, 0))
      }
      Policy.Relay(policy, res)
    }

  @tailrec
  private final def getElseChain(term: Term.If, res: Seq[T]): Seq[T] = {
    term.tokens.find(x => x.is[T.KwElse] && owners(x) == term) match {
      case Some(els @ T.KwElse()) =>
        val tuck = !initStyle.newlines.alwaysBeforeElseAfterCurlyIf && {
          val prev = tokens(els, -1)
          prev.left.is[T.RightBrace] && prev.meta.leftOwner != term
        }
        val newRes = if (tuck) res else els +: res
        term.elsep match {
          case t: Term.If => getElseChain(t, newRes)
          case _ => newRes
        }
      case _ => res
    }
  }

  def getOptimalTokenFor(token: T): T =
    getOptimalTokenFor(tokens(token))

  def getOptimalTokenFor(ft: FormatToken): T =
    if (tokens.isAttachedCommentThenBreak(ft)) ft.right else ft.left

  def insideInfixSplit(
      app: Member.Infix,
      ft: FormatToken
  )(implicit style: ScalafmtConfig): Seq[Split] =
    app match {
      case t: Type.ApplyInfix
          if style.spaces.neverAroundInfixTypes.contains(t.op.value) =>
        Seq(Split(NoSplit, 0))
      case t =>
        val afterInfix = style.breakAfterInfix(t)
        if (afterInfix ne Newlines.AfterInfix.keep) {
          if (ft.meta.leftOwner ne app.op) Seq(Split(Space, 0))
          else {
            val fullInfix = InfixSplits.findEnclosingInfix(app)
            val ok = isEnclosedInParens(fullInfix) || fullInfix.parent.forall {
              case t: Defn.Val => t.rhs eq fullInfix
              case t: Defn.Var => t.body eq fullInfix
              case _ => true
            }
            if (ok)
              InfixSplits(app, ft, fullInfix).getBeforeLhsOrRhs(afterInfix)
            else Seq(Split(Space, 0))
          }
        } else {
          // we don't modify line breaks generally around infix expressions
          // TODO: if that ever changes, modify how rewrite rules handle infix
          Seq(InfixSplits.withNLIndent(Split(getMod(ft), 0))(app, ft))
        }
    }

  def getInfixSplitsBeforeLhs(
      lhsApp: Member.Infix,
      ft: FormatToken,
      afterInfix: Newlines.AfterInfix,
      newStmtMod: Option[Modification] = None
  )(implicit style: ScalafmtConfig): Seq[Split] = {
    val fullInfixTreeOpt =
      findTreeWithParentSimple(lhsApp, false)(isInfixApp)
    val fullInfix = fullInfixTreeOpt.flatMap(asInfixApp).getOrElse(lhsApp)
    val app = findLeftInfix(fullInfix)
    new InfixSplits(app, ft, fullInfix, app)
      .getBeforeLhsOrRhs(afterInfix, newStmtMod)
  }

  final def maybeGetInfixSplitsBeforeLhs(
      ft: FormatToken,
      mod: => Option[Modification] = None
  )(nonInfixSplits: => Seq[Split])(implicit
      style: ScalafmtConfig
  ): Seq[Split] = {
    val tree = ft.meta.rightOwner
    val ai = style.breakAfterInfix(tree)
    val app = if (ai eq Newlines.AfterInfix.keep) None else asInfixApp(tree)
    app.fold(nonInfixSplits)(getInfixSplitsBeforeLhs(_, ft, ai, mod))
  }

  private[internal] object InfixSplits {

    def apply(app: Member.Infix, ft: FormatToken)(implicit
        style: ScalafmtConfig
    ): InfixSplits =
      apply(app, ft, findEnclosingInfix(app))

    def apply(app: Member.Infix, ft: FormatToken, fullInfix: Member.Infix)(
        implicit style: ScalafmtConfig
    ): InfixSplits = {
      val leftInfix = findLeftInfix(fullInfix)
      new InfixSplits(app, ft, fullInfix, leftInfix)
    }

    private def switch(splits: Seq[Split], triggers: T*): Seq[Split] =
      splits.map { x =>
        triggers.foldLeft(x) { case (y, trigger) => y.switch(trigger, false) }
      }

    @tailrec
    private def findEnclosingInfix(
        child: Member.Infix,
        childTree: Tree
    ): Member.Infix =
      if (isEnclosedInParens(childTree)) child
      else
        childTree.parent match {
          case Some(p: Member.Infix) if !p.isAssignment =>
            findEnclosingInfix(p, p)
          case Some(p @ Member.ArgClause(_ :: Nil)) =>
            findEnclosingInfix(child, p)
          case _ => child
        }

    private[FormatOps] def findEnclosingInfix(app: Member.Infix): Member.Infix =
      findEnclosingInfix(app, app)

    def withNLIndent(split: Split)(app: Member.Infix, ft: FormatToken)(implicit
        style: ScalafmtConfig
    ): Split = {
      val noNL = !split.isNL && {
        val nextFt = nextNonCommentSameLine(ft)
        nextFt.eq(ft) || nextFt.noBreak
      }
      if (noNL) split else apply(app, ft).withNLIndent(split)
    }

  }

  private[internal] class InfixSplits(
      app: Member.Infix,
      ft: FormatToken,
      fullInfix: Member.Infix,
      leftInfix: Member.Infix
  )(implicit style: ScalafmtConfig) {
    private val beforeLhs = ft.left.start < app.pos.start
    private val fullExpire = getLastEnclosedToken(fullInfix)
    private val isFirstOp = beforeLhs || (leftInfix.op eq app.op)

    private val assignBodyExpire = {
      val prevFt = tokenBefore(fullInfix)
      val prevOwner = prevFt.meta.leftOwner
      prevFt.left match {
        case _: T.Equals => Some(getLastToken(prevOwner))
        case _: T.LeftParen | _: T.LeftBracket
            if fullInfix.parent.contains(prevOwner) && !(prevOwner match {
              case po: Member.ArgClause => po.parent.exists(isInfixApp)
              case po => isInfixApp(po)
            }) && Option(getArgs(prevOwner, orNull = true))
              .exists(isSeqSingle) =>
          Some(getLastToken(fullInfix))
        case _ => None
      }
    }

    private val skipInfixIndent: Boolean = {
      @tailrec
      def getLastPat(t: Tree): Tree =
        t.parent match {
          case Some(p @ (_: Pat | _: Pat.ArgClause)) => getLastPat(p)
          case _ => t
        }
      def getChild = fullInfix match {
        case t: Pat => getLastPat(t)
        case t => t
      }
      def isOldTopLevel(child: Tree) = child.parent.exists {
        case _: Term.Block | _: Term.If | _: Term.While | _: Source => true
        case fun: Term.FunctionTerm if isBlockFunction(fun) => true
        case t: Case => t.pat.eq(child) || t.body.eq(child)
        case _ => false
      }
      def isAloneEnclosed(child: Tree) = child.parent.exists {
        case p: Case => p.pat eq child
        case p: Term.If => p.cond eq child
        case p: Term.While => p.expr eq child
        case p: Term.Do => p.expr eq child
        case p: Term.Block => isSingleElement(p.stats, child)
        case p: Term.FunctionTerm => isBlockFunction(p)
        case p @ Member.ArgClause(`child` :: Nil) => isEnclosedInMatching(p)
        case Member.Tuple(`child` :: Nil) => true
        case _ => false
      }
      def isAloneArgOrBody(child: Tree) = child.parent.exists {
        case t: Case => t.pat.eq(child) || t.body.eq(child)
        case _: Term.If | _: Term.While | _: Term.Do => true
        case _: Member.ArgClause => true
        case p: Term.Block => isSingleElement(p.stats, child)
        case _: Init | _: Term.Super | _: Member.Tuple => true
        case t: Tree.WithBody => t.body eq child
        case t: Term.Param => t.default.contains(child)
        case _ => false
      }
      val allowNoIndent = style.indentOperator.getExemptScope match {
        case IndentOperator.Exempt.all => true
        case IndentOperator.Exempt.oldTopLevel => isOldTopLevel(getChild)
        case IndentOperator.Exempt.aloneEnclosed => isAloneEnclosed(getChild)
        case IndentOperator.Exempt.aloneArgOrBody => isAloneArgOrBody(getChild)
      }
      def isInfixTopLevelMatch(op: String, noindent: Boolean): Boolean = {
        noindent == style.indentOperator.noindent(op) &&
        noindent == allowNoIndent
      }
      if (style.verticalAlignMultilineOperators)
        !isAssignment(ft.meta.left.text)
      else if (beforeLhs) assignBodyExpire.isEmpty
      else if (
        !app.singleArg.exists { x =>
          x.is[Term.Block] || x.is[Term.NewAnonymous]
        } && isInfixTopLevelMatch(ft.meta.left.text, false)
      ) false
      else if (isInfixTopLevelMatch(app.op.value, true)) true
      else if (app.is[Pat] && isChildOfCaseClause(app)) true
      else false
    }

    private val fullIndent: Indent = {
      val expire = assignBodyExpire match {
        case Some(x) if beforeLhs => x
        case _ => fullExpire
      }
      Indent(Num(style.indent.main), expire, ExpiresOn.After)
    }

    val (nlIndent, nlPolicy) = {
      def policy(triggers: T*)(implicit fileLine: FileLine) =
        if (triggers.isEmpty) Policy.NoPolicy
        else
          Policy.on(fullExpire) {
            case Decision(t: FormatToken, s)
                if isInfixOp(t.meta.leftOwner) ||
                  isInfixOp(t.meta.rightOwner) &&
                  !t.meta.rightOwner.parent.exists(style.formatInfix(_)) =>
              InfixSplits.switch(s, triggers: _*)
          }

      val fullTok = getIndentTrigger(fullInfix)
      val noAssign = assignBodyExpire.isEmpty
      if (!noAssign && beforeLhs) (fullIndent, policy(fullTok))
      else if (skipInfixIndent) {
        if (noAssign) (Indent.Empty, Policy.NoPolicy)
        else (Indent.before(fullIndent, fullTok), policy(fullTok))
      } else {
        val opTok = getIndentTrigger(leftInfix.op)
        val ind =
          if (isFirstOp) fullIndent else Indent.before(fullIndent, opTok)
        if (noAssign) (ind, policy(opTok))
        else (Indent.Switch(fullIndent, fullTok, ind), policy(fullTok, opTok))
      }
    }

    private def withNLIndent(split: Split): Split =
      split.withIndent(nlIndent).andPolicy(nlPolicy)

    def getBeforeLhsOrRhs(
        afterInfix: Newlines.AfterInfix,
        newStmtMod: Option[Modification] = None
    ): Seq[Split] = {
      val beforeLhs = ft.meta.leftOwner ne app.op
      val maxPrecedence =
        if (beforeLhs) 0 // not used
        else infixSequenceMaxPrecedence(fullInfix)
      val closeOpt = matchingOpt(ft.right)
      val expiresOpt =
        if (closeOpt.isDefined) None
        else {
          val res = mutable.Buffer.empty[Member.Infix]
          findNextInfixes(fullInfix, app.lhs, res)
          val infixes = if (beforeLhs) res.toSeq else res.toSeq.tail
          val filtered =
            if (!style.newlines.afterInfixBreakOnNested) infixes
            else infixes.takeWhile(x => !isEnclosedInParens(x.lhs))
          if (filtered.isEmpty) None
          else {
            val res = filtered.foldLeft(Seq.empty[(T, Int)]) { case (out, ia) =>
              val cost = maxPrecedence - ia.precedence
              if (out.nonEmpty && out.head._2 <= cost) out
              else (getMidInfixToken(ia) -> cost) +: out
            }
            Some(res)
          }
        }

      val breakPenalty = if (beforeLhs) 1 else maxPrecedence - app.precedence
      val expires = expiresOpt.fold(Seq(fullExpire -> 0)) { x =>
        (if (x.head._2 == 0) x else (fullExpire -> 0) +: x).reverse
      }

      val infixTooLong = infixSequenceLength(fullInfix) >
        style.newlines.afterInfixMaxCountPerExprForSome
      val breakMany = infixTooLong || afterInfix == Newlines.AfterInfix.many
      val rightAsInfix = asInfixApp(ft.meta.rightOwner)

      def breakAfterComment(t: FormatToken) = {
        val end = nextNonCommentSameLine(t)
        if (end.right.is[T.LeftBrace] || end.right.is[T.Comment]) None
        else if (end eq t) Some(decideNewlinesOnlyAfterToken(end.left))
        else Some(decideNewlinesOnlyAfterClose(end.left))
      }
      val nlMod = newStmtMod.getOrElse {
        Space.orNL(ft.noBreak && ft.right.is[T.Comment])
      }
      val delayedBreak = if (nlMod.isNewline) None else breakAfterComment(ft)

      val (singleLineExpire, singleLineIndent) = {
        val skip = skipInfixIndent
        if (isFirstOp) (fullExpire, if (skip) Indent.Empty else fullIndent)
        else {
          val expire = expires.head._1
          val indentLen = if (skip) 0 else style.indent.main
          val indent = Indent(Num(indentLen), expire, ExpiresOn.After)
          (expire, indent)
        }
      }

      val singleLinePolicy =
        if (infixTooLong || !isFirstOp) None
        else Some(getSingleLineInfixPolicy(fullExpire))
      val nlSinglelineSplit = Split(nlMod, 0)
        .onlyIf(singleLinePolicy.isDefined && beforeLhs)
        .withIndent(singleLineIndent)
        .withSingleLine(singleLineExpire)
        .andPolicyOpt(singleLinePolicy)
        .andPolicyOpt(delayedBreak)
      val spaceSingleLine = Split(Space, 0)
        .onlyIf(newStmtMod.isEmpty)
        .withSingleLine(singleLineExpire)
        .andPolicyOpt(singleLinePolicy)
      val singleLineSplits = Seq(
        spaceSingleLine.onlyFor(SplitTag.InfixChainNoNL),
        spaceSingleLine.onlyIf(singleLinePolicy.isDefined),
        nlSinglelineSplit
      )

      val otherSplits = closeOpt.fold {
        val nlSplit = Split(nlMod, 1 + breakPenalty)
        Seq(nlSplit.withIndent(nlIndent).withPolicy(nlPolicy & delayedBreak))
      } { close =>
        val noSingleLine = newStmtMod.isDefined || breakMany ||
          rightAsInfix.exists(10 < infixSequenceLength(_))
        val nextOp =
          if (!style.newlines.afterInfixBreakOnNested) None
          else if (beforeLhs) Some(app.op)
          else
            getInfixRhsAsInfix(app) match {
              case Some(ia) => Some(findLeftInfix(ia).op)
              case _ => findNextInfixInParent(app, fullInfix)
            }
        val endOfNextOp = nextOp.map(tokens.getLast)
        val breakAfterClose = endOfNextOp.flatMap(breakAfterComment)

        val nlSplit = Split(nlMod, 0)
          .andPolicyOpt(breakAfterClose)
          .withIndent(nlIndent)
          .withPolicy(nlPolicy)
        val singleLineSplit = Split(Space, 0)
          .notIf(noSingleLine)
          .withSingleLine(endOfNextOp.fold(close)(_.left))
          .andPolicyOpt(breakAfterClose)
          .andPolicy(getSingleLineInfixPolicy(close))
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
                if (breakMany) TokenRanges.empty
                else insideBracesBlock(nextFT, expire, true)
              Split(ModExt(newStmtMod.getOrElse(Space)), cost)
                .withSingleLine(expire, exclude)
          }
        }

      singleLineSplits ++ spaceSplits ++ otherSplits
    }

  }

  def getSingleLineInfixPolicy(end: T) =
    Policy.on(end) {
      case Decision(t: FormatToken, s) if isInfixOp(t.meta.leftOwner) =>
        SplitTag.InfixChainNoNL.activateOnly(s)
    }

  def getMidInfixToken(app: Member.Infix): T = {
    val opToken = app.op.tokens.head
    val opFollowsComment = tokens(opToken, -1).left.is[T.Comment]
    if (opFollowsComment) getLastNonTrivialToken(app.lhs) else opToken
  }

  def getLastEnclosedToken(tree: Tree): T = {
    tokens.getLastExceptParen(tree.tokens).left
  }

  @tailrec
  private def findNextInfixes(
      fullTree: Tree,
      tree: Tree,
      res: mutable.Buffer[Member.Infix]
  ): Unit = if (tree ne fullTree)
    tree.parent match {
      case Some(ia: Member.Infix) =>
        if (ia.lhs eq tree) {
          res += ia
          findNestedInfixes(ia.arg, res)
        }
        findNextInfixes(fullTree, ia, res)
      case Some(p: Member.ArgClause) =>
        p.parent match {
          case Some(pp: Member.Infix) =>
            findNextInfixes(fullTree, pp, res)
          case _ =>
        }
      case _ =>
    }

  private def findNestedInfixes(
      tree: Tree,
      res: mutable.Buffer[Member.Infix]
  ): Unit = tree match {
    case Member.ArgClause(arg :: Nil) if !isEnclosedInParens(tree) =>
      findNestedInfixes(arg, res)
    case ia: Member.Infix if !isEnclosedInParens(tree) =>
      findNestedInfixes(ia.lhs, res)
      res += ia
      ia.singleArg.foreach(findNestedInfixes(_, res))
    case _ =>
  }

  @tailrec
  final def findLeftInfix(app: Member.Infix): Member.Infix =
    app.lhs match {
      case t: Member.Infix if !isEnclosedInParens(t) =>
        findLeftInfix(t)
      case _ => app
    }

  private def getInfixRhsAsInfix(app: Member.Infix): Option[Member.Infix] =
    app.singleArg match {
      case Some(t: Member.Infix) if !isEnclosedInParens(t) => Some(t)
      case _ => None // multiple parameters to infix are always enclosed
    }

  private def infixSequenceMaxPrecedence(app: Member.Infix): Int = {
    val queue = new mutable.Queue[Member.Infix]()
    queue += app
    var maxPrecedence = 0
    while (queue.nonEmpty) {
      val elem = queue.dequeue()
      val elemPrecedence = elem.precedence
      if (maxPrecedence < elemPrecedence)
        maxPrecedence = elemPrecedence
      queue ++= elem.nestedInfixApps.filter(x => !isEnclosedInParens(x))
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

  def functionExpire(function: Term.FunctionTerm): (T, ExpiresOn) = {
    def dropWS(rtoks: Seq[T]): Seq[T] =
      rtoks.dropWhile(_.is[Whitespace])
    def orElse(rtoks: Seq[T]) = {
      val last = rtoks.head
      if (last.is[T.RightParen] && matchingOpt(last).contains(rtoks.last))
        rtoks.tail.find(!_.is[Whitespace]).get -> ExpiresOn.After
      else
        last -> ExpiresOn.After
    }
    def dropComment(rtoks: Seq[T]) =
      if (rtoks.head.is[T.Comment]) dropWS(rtoks.tail) else rtoks

    def getRToks = dropWS(function.tokens.reverse)
    function.parent match {
      case Some(b @ Term.Block(_ :: Nil)) =>
        getLastToken(b) -> ExpiresOn.Before
      case Some(Case(_, _, `function`)) =>
        orElse(dropComment(getRToks))
      case _ =>
        orElse(getRToks)
    }
  }

  def noOptimizationZones(): Set[T] = {
    val result = Set.newBuilder[T]
    var expire: T = null
    tokens.foreach {
      case FormatToken(x, _, _) if expire ne null =>
        if (x eq expire) expire = null else result += x
      case FormatToken(t: T.LeftParen, _, m) if (m.leftOwner match {
            // TODO(olafur) https://github.com/scalameta/scalameta/issues/345
            case lo: Term.ArgClause => !lo.parent.exists(_.is[Term.ApplyInfix])
            case _: Term.Apply => true // legacy: when enclosed in parens
            case _ => false
          }) =>
        expire = matching(t)
      case FormatToken(t: T.LeftBrace, _, m) if (m.leftOwner match {
            // Type compounds can be inside defn.defs
            case _: Type.Refine => true
            case _ => false
          }) =>
        expire = matching(t)
      case _ =>
    }
    result.result()
  }

  def mustUseConfigStyle(
      ft: FormatToken,
      allowForce: => Boolean = true
  )(implicit style: ScalafmtConfig): Boolean =
    style.optIn.configStyleArguments && couldUseConfigStyle(ft, allowForce)

  def couldUseConfigStyle(
      ft: FormatToken,
      allowForce: => Boolean = true
  )(implicit style: ScalafmtConfig): Boolean =
    opensConfigStyle(ft) || allowForce && forceConfigStyle(hash(ft.left))

  def opensConfigStyle(
      ft: => FormatToken,
      whenSourceIgnored: Boolean = false
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

  /** Works for `using` as well */
  def opensConfigStyleImplicitParamList(
      formatToken: FormatToken
  )(implicit style: ScalafmtConfig): Boolean =
    formatToken.right.is[soft.ImplicitOrUsing] &&
      style.newlines.notBeforeImplicitParamListModifier &&
      hasImplicitParamList(formatToken.meta.rightOwner)

  val ImplicitUsingOnLeft =
    new ExtractFromMeta(ft => getImplicitParamList(ft.meta.leftOwner))

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

  def distance(left: T, right: T): Int = {
    nonWhitespaceOffset(right) - nonWhitespaceOffset(left)
  }

  def typeTemplateSplits(template: Template, indentIfSecond: Int)(implicit
      fileLine: FileLine,
      ft: FormatToken,
      style: ScalafmtConfig
  ): Seq[Split] = {
    def getPolicy(expire: T) = expire match {
      case lb: T.LeftBrace if template.self.tokens.isEmpty =>
        Policy.after(lb) {
          // Force template to be multiline.
          case d @ Decision(ftd @ FormatToken(`lb`, right, _), _)
              if !right.is[T.RightBrace] && // corner case, body is {}
                !tokens.isAttachedCommentThenBreak(ftd) =>
            d.onlyNewlinesWithoutFallback
        }
      case _ => NoPolicy
    }
    findTemplateGroupOnRight { x =>
      // this method is called on a `with` or comma; hence, it can
      // only refer to second or subsequent init/derive in a group
      // we'll indent only the second, but not any subsequent ones
      val expire = x.getExpireToken
      val indent =
        if (!x.isSecond) Indent.Empty
        else Indent(Num(indentIfSecond), expire, ExpiresOn.After)
      def nlSplit(cost: Int) =
        Split(Newline, cost).withPolicy(getPolicy(expire)).withIndent(indent)
      if (!style.binPack.keepParentConstructors)
        Seq(Split(Space, 0).withIndent(indent), nlSplit(1))
      else if (ft.hasBreak)
        Seq(nlSplit(0))
      else {
        val slbEnd = getLastToken(x.superType)
        Seq(
          Split(Space, 0)
            .withIndent(indent)
            .withSingleLine(
              slbEnd,
              exclude = insideBlock[T.LeftParen](ft, slbEnd),
              noSyntaxNL = true
            ),
          nlSplit(1)
        )
      }
    }(template).getOrElse {
      Seq(Split(Space, 0)) // shouldn't happen
    }
  }

  def ctorWithChain(
      ownerSet: Set[Tree],
      lastToken: T
  )(implicit style: ScalafmtConfig): Policy =
    if (style.binPack.parentConstructors eq BinPack.ParentCtors.Always) NoPolicy
    else if (ownerSet.isEmpty) NoPolicy
    else
      Policy.after(lastToken) {
        case d @ Decision(t @ FormatToken(_, _: T.KwWith, _), _)
            if ownerSet.contains(t.meta.rightOwner) =>
          d.onlyNewlinesWithoutFallback
        case d @ Decision(t @ FormatToken(T.Comma(), _, _), _)
            if ownerSet.contains(t.meta.leftOwner) =>
          d.onlyNewlinesWithoutFallback
      }

  def binPackParentConstructorSplits(
      isFirstCtor: Boolean,
      owners: => Set[Tree],
      rhs: => Option[Tree],
      lastToken: T,
      indentLen: Int,
      extendsThenWith: => Boolean = false
  )(implicit
      fileLine: FileLine,
      ft: FormatToken,
      style: ScalafmtConfig
  ): Seq[Split] = {
    val nlMod = NewlineT(alt = Some(Space))
    val indent =
      if (!isFirstCtor) Indent.Empty
      else Indent(Num(indentLen), lastToken, ExpiresOn.After)
    if (style.binPack.keepParentConstructors) {
      if (ft.hasBreak)
        Seq(Split(nlMod, 0).withIndent(indent))
      else {
        val slbEnd = rhs.fold(lastToken)(getLastToken)
        Seq(
          Split(Space, 0)
            .withIndent(indent)
            .withSingleLine(
              slbEnd,
              exclude = insideBracesBlock(ft, slbEnd, true),
              noSyntaxNL = extendsThenWith
            ),
          Split(nlMod, 1).withIndent(indent)
        )
      }
    } else if (isFirstCtor) {
      val nlPolicy = ctorWithChain(owners, lastToken)
      val nlOnelineTag = style.binPack.parentConstructors match {
        case BinPack.ParentCtors.Oneline => Right(true)
        case BinPack.ParentCtors.OnelineIfPrimaryOneline =>
          Left(SplitTag.OnelineWithChain)
        case BinPack.ParentCtors.Always | BinPack.ParentCtors.Never =>
          Right(false)
        case _ =>
          Right(style.newlines.source eq Newlines.fold)
      }
      val exclude = style.binPack.parentConstructors match {
        case BinPack.ParentCtors.Always
            if ft.noBreak || style.newlines.sourceIgnored =>
          insideBracesBlock(ft, lastToken, true)
        case _ => TokenRanges.empty
      }
      val noSyntaxNL = extendsThenWith
      val pnlPolicy = PenalizeAllNewlines(lastToken, 1, noSyntaxNL = noSyntaxNL)
      Seq(
        Split(Space, 0)
          .withSingleLine(lastToken, exclude = exclude, noSyntaxNL = noSyntaxNL)
          .orPolicy(pnlPolicy)
          .withIndent(indent),
        Split(nlMod, 0)
          .onlyIf(nlOnelineTag != Right(false))
          .preActivateFor(nlOnelineTag.left.toOption)
          .withSingleLine(lastToken, noSyntaxNL = noSyntaxNL)
          .withIndent(indent),
        Split(nlMod, 1).withPolicy(nlPolicy & pnlPolicy).withIndent(indent)
      )
    } else Seq(Split(Space, 0), Split(Newline, 1))
  }

  def getForceConfigStyle: (Set[TokenHash], Set[TokenHash]) = {
    val maxDistance = runner.optimizer.forceConfigStyleOnOffset
    if (maxDistance < 0)
      (Set.empty, Set.empty)
    else {
      val clearQueues = Set.newBuilder[TokenHash]
      val forces = Set.newBuilder[TokenHash]
      val minArgs = runner.optimizer.forceConfigStyleMinArgCount
      def process(args: Seq[Tree], open: T, close: T): Unit =
        if (
          args.lengthCompare(minArgs) >= 0 &&
          distance(open, close) > maxDistance
        ) {
          forces += hash(open)
          args.foreach { arg =>
            clearQueues += hash(tokens.getHead(arg).left)
          }
        }
      tokens.foreach {
        case FormatToken(left: T.LeftParen, _, meta) =>
          meta.leftOwner match {
            case t: Term.ArgClause if !t.parent.exists(_.is[Term.ApplyInfix]) =>
              matchingOpt(left).foreach(process(t.values, left, _))
            case _ =>
          }
        case FormatToken(left: T.LeftBracket, _, meta) =>
          meta.leftOwner match {
            case t: Type.ArgClause =>
              matchingOpt(left).foreach(process(t.values, left, _))
            case _ =>
          }
        case _ =>
      }
      (forces.result(), clearQueues.result())
    }
  }

  /** Implementation for `verticalMultiline`
    */
  def verticalMultiline(
      ft: FormatToken
  )(implicit style: ScalafmtConfig): Seq[Split] = {
    val lpOwner = ft.meta.leftOwner
    val lpParent = lpOwner.parent

    val FormatToken(open, r, _) = ft
    val close = matching(open)
    val indentParam = Num(style.indent.getDefnSite(lpOwner))
    val indentSep = Num((indentParam.n - 2).max(0))
    val isBracket = open.is[T.LeftBracket]

    def getClausesFromClauseGroup(tree: Member.ParamClauseGroup) = {
      val tparams = tree.tparamClause
      val params = tree.paramClauses
      if (tparams.isEmpty) params else tparams :: params
    }
    val allClauses = lpOwner match {
      case ParamClauseParent(p) =>
        p match {
          case t: Tree.WithParamClauseGroups =>
            t.paramClauseGroups.flatMap(getClausesFromClauseGroup)
          case t: Tree.WithParamClauseGroup =>
            t.paramClauseGroup.map(getClausesFromClauseGroup).getOrElse(Nil)
          case t: Stat.WithCtor with Tree.WithTParamClause =>
            t.tparamClause +: t.ctor.paramClauses
          case t: Tree.WithParamClauses => t.paramClauses
          case t: Stat.WithCtor => t.ctor.paramClauses
          case t: Tree.WithTParamClause => t.tparamClause :: Nil
          case _ => Nil
        }
      case _ => Nil
    }
    val allParenOwners = allClauses.filter(_.pos.start > open.start)
    val allOwners = lpOwner +: allParenOwners

    // find the last param on the defn so that we can apply our `policy`
    // till the end.
    val lastParens = allParenOwners.map(getLastToken)
    val lastParen = lastParens.lastOption.getOrElse(close)

    val shouldNotDangle = shouldNotDangleAtDefnSite(lpParent, true)

    // Since classes and defs aren't the same (see below), we need to
    // create two (2) OneArgOneLineSplit when dealing with classes. One
    // deals with the type params and the other with the value params.
    val oneLinePerArg = allOwners
      .zip(close +: lastParens)
      .map { case (owner, end) => splitOneArgOneLine(end, owner) }
      .reduceRight(new Policy.Relay(_, _))

    // DESNOTE(2017-03-28, pjrt) Classes and defs aren't the same.
    // For defs, type params and value param have the same `owners`. However
    // this is not the case for classes. Type params have the class itself
    // as the owner, but value params have the Ctor as the owner, so a
    // simple check isn't enough. Instead we check against the owner of the
    // `lastParen` as well, which will be the same as the value param's
    // owner.

    val paramGroupSplitter = Policy.on(lastParen) {
      // If this is a class, then don't dangle the last paren unless the line ends with a comment
      case Decision(ftd @ FormatToken(_, `lastParen`, _), _)
          if shouldNotDangle && !isLeftCommentThenBreak(ftd) =>
        Seq(Split(NoSplit, 0))
      // Indent separators `)(` and `](` by `indentSep`
      case Decision(FormatToken(_, `close`, _), _) =>
        Seq(Split(Newline, 0).withIndent(indentSep, close, ExpiresOn.After))
      case Decision(FormatToken(LeftParenOrBracket(), _, m), ss)
          if allParenOwners.contains(m.leftOwner) =>
        ss.filter(!_.isActiveFor(SplitTag.VerticalMultilineSingleLine))
      case Decision(ftd @ FormatToken(soft.ImplicitOrUsing(), _, m), _)
          if style.newlines.forceAfterImplicitParamListModifier &&
            !tokens.isRightCommentThenBreak(ftd) &&
            hasImplicitParamList(m.leftOwner) =>
        Seq(Split(Newline, 0))
    }

    // Our policy is a combination of OneArgLineSplit and a custom splitter
    // for parameter groups.
    val policy = oneLinePerArg | paramGroupSplitter

    val firstIndent =
      if (r.is[T.RightParen]) // An empty param group
        Indent(indentSep, close, ExpiresOn.After)
      else
        Indent(indentParam, close, ExpiresOn.Before)

    def aboveArityThreshold = {
      val threshold = style.verticalMultiline.arityThreshold
      allOwners.exists {
        case Member.ParamClause(v) => v.lengthCompare(threshold) >= 0
        case _ => false
      }
    }

    val space = Space(style.spaces.inParentheses)
    val slbEnd =
      if (style.newlines.sometimesBeforeColonInMethodReturnType) lastParen
      else {
        val afterLastParen = tokens.before(lastParen).right
        if (afterLastParen.is[T.Colon]) afterLastParen else lastParen
      }
    val slbSplit = Split(space, 0)
      .withSingleLine(slbEnd, killOnFail = true)
      .preActivateFor(SplitTag.VerticalMultilineSingleLine)

    if (isBracket) {
      val noSlbPolicy = Policy.on(lastParen) {
        case Decision(FormatToken(LeftParenOrBracket(), _, m), ss)
            if allParenOwners.contains(m.leftOwner) =>
          ss.filter(!_.isActiveFor(SplitTag.VerticalMultilineSingleLine))
      }
      val noSplit =
        if (allParenOwners.isEmpty)
          Split(space, 0).withSingleLine(close)
        else {
          val lpNext = tokens.getHead(allParenOwners.head)
          val lpNextLeft = lpNext.left
          val slbPolicy =
            if (tokens.isRightCommentThenBreak(lpNext)) Policy.NoPolicy
            else decideNewlinesOnlyAfterToken(lpNextLeft)
          // If we can fit the type params, make it so
          Split(space, 0).withSingleLine(lpNextLeft).orPolicy(slbPolicy)
        }
      val nlSplit = Split(Newline, 1, policy = policy).withIndent(firstIndent)
      Seq(slbSplit, noSplit.andPolicy(noSlbPolicy), nlSplit)
    } else {
      val rightIsImplicit = r.is[soft.ImplicitOrUsing]
      val implicitNL = rightIsImplicit &&
        style.newlines.forceBeforeImplicitParamListModifier
      val implicitParams =
        if (!rightIsImplicit) Nil
        else getImplicitParamList(ft.meta.rightOwner).getOrElse(Nil)
      val noSlb = implicitNL || aboveArityThreshold || ft.hasBreak &&
        !style.newlines.sourceIgnored && style.optIn.configStyleArguments ||
        implicitParams.nonEmpty &&
        style.newlines.forceAfterImplicitParamListModifier
      val nlNoAlt = implicitNL ||
        !rightIsImplicit && style.verticalMultiline.newlineAfterOpenParen
      val nlMod = NewlineT(alt = if (nlNoAlt) None else Some(slbSplit.modExt))
      val spaceImplicit = !implicitNL && implicitParams.lengthCompare(1) > 0 &&
        style.newlines.notBeforeImplicitParamListModifier
      Seq(
        // If we can fit all in one block, make it so
        slbSplit.notIf(noSlb),
        Split(space, 0, policy = policy)
          .onlyIf(spaceImplicit)
          .andPolicy(
            decideNewlinesOnlyAfterClose(r),
            tokens.isRightCommentThenBreak(next(ft))
          )
          .withIndent(firstIndent),
        // Otherwise split vertically
        Split(nlMod, 1, policy = policy).withIndent(firstIndent)
      )
    }

  }

  // Returns leading comment, if there exists one, otherwise formatToken
  @inline
  final def leadingComment(tree: Tree): FormatToken =
    leadingComment(tokens.tokenJustBefore(tree))
  @tailrec
  final def leadingComment(ft: FormatToken): FormatToken =
    if (ft.hasBlankLine || !ft.left.is[T.Comment]) ft
    else {
      val pft = tokens.prevNonCommentSameLine(prev(ft))
      if (pft.noBreak) ft else leadingComment(pft)
    }

  // Returns trailing comment, if there exists one, otherwise formatToken
  @inline
  final def trailingComment(ft: FormatToken, end: Int): FormatToken = {
    @inline
    def isDone(x: FormatToken) = x.hasBlankLine || x.right.end >= end
    @tailrec
    def iter(x: FormatToken): FormatToken = {
      val nft = tokens.nextNonCommentSameLine(next(x))
      if (isDone(nft)) nft
      else if (!nft.right.is[T.Comment]) ft // original
      else iter(nft)
    }
    if (!ft.right.is[T.Comment] || isDone(ft)) ft else iter(ft)
  }

  def xmlSpace(owner: Tree): Modification =
    owner match {
      case _: Term.Xml | _: Pat.Xml => NoSplit
      case _ => Space
    }

  def getSpaceAndNewlineAfterCurlyLambda(
      newlines: Int
  )(implicit style: ScalafmtConfig): (Boolean, NewlineT) =
    style.newlines.afterCurlyLambdaParams match {
      case Newlines.AfterCurlyLambdaParams.squash => (true, Newline)
      case Newlines.AfterCurlyLambdaParams.never =>
        val space = style.newlines.source match {
          case Newlines.fold => true
          case Newlines.unfold => false
          case _ => newlines == 0
        }
        (space, Newline)
      case Newlines.AfterCurlyLambdaParams.always => (false, Newline2x)
      case Newlines.AfterCurlyLambdaParams.preserve =>
        val noBlanks = newlines < 2
        val space = style.newlines.source match {
          case Newlines.fold => noBlanks
          case Newlines.unfold => false
          case _ => newlines == 0
        }
        (space, if (noBlanks) Newline else Newline2x)
    }

  def getNoSplit(
      ft: FormatToken,
      spaceOk: Boolean
  )(implicit style: ScalafmtConfig): Modification =
    ft.right match {
      case _: T.Comment =>
        val isDetachedSlc = ft.hasBreak && tokens.isBreakAfterRight(ft)
        if (isDetachedSlc || ft.rightHasNewline) null else Space
      case _ =>
        Space(style.spaces.inParentheses && spaceOk)
    }

  // look for arrow before body, if any, else after params
  def getFuncArrow(term: Term.FunctionTerm): Option[FormatToken] =
    tokens
      .tokenBeforeOpt(term.body)
      .orElse(tokens.tokenAfterOpt(term.paramClause).map(getArrowAfter))
      .orElse {
        findFirst(tokens.getHead(term), term.pos.end)(_.left.is[T.RightArrow])
      }

  // look for arrow before body, if any, else after cond/pat
  def getCaseArrow(term: Case): FormatToken =
    tokens.tokenBeforeOpt(term.body).getOrElse {
      getArrowAfter(tokens.tokenAfter(term.cond.getOrElse(term.pat)))
    }

  // look for arrow before body, if any, else after cond/pat
  def getCaseArrow(term: TypeCase): FormatToken =
    next(tokens.tokenAfter(term.pat))

  private def getArrowAfter(ft: FormatToken): FormatToken = {
    val maybeArrow = next(ft)
    if (maybeArrow.left.is[T.RightArrow]) maybeArrow
    else next(nextNonComment(maybeArrow))
  }

  def getArgs(owner: Tree, orNull: Boolean = false): Seq[Tree] =
    owner match {
      case _: Lit.Unit => Nil
      case t: Term.Super => t.superp :: Nil
      case Member.Tuple(v) => v
      case Member.SyntaxValuesClause(v) => v
      case t: Member.Function => t.paramClause.values
      case _ if orNull => null
      case t =>
        logger.debug(
          s"""getApplyArgs: unknown tree
            |Tree: ${log(t)}
            |Parent: ${log(t.parent)}
            |GrandParent: ${log(t.parent.flatMap(_.parent))}
            |""".stripMargin
        )
        throw UnexpectedTree[Member.SyntaxValuesClause](t)
    }

  @tailrec
  final def findPrevSelectAndApply(
      tree: Tree,
      enclosed: Boolean,
      applyTree: Option[Member.Apply] = None
  ): (Option[SelectLike], Option[Member.Apply]) = {
    @inline def isEnclosed: Boolean = enclosed && isEnclosedInParens(tree)
    tree match {
      case GetSelectLike(t) if !isEnclosed =>
        (Some(t), applyTree)
      case t: Member.Apply if !isEnclosed =>
        findPrevSelectAndApply(t.fun, enclosed, applyTree.orElse(Some(t)))
      case Term.AnonymousFunction(body) if !enclosed =>
        findPrevSelectAndApply(body, false, applyTree)
      case _ => (None, applyTree)
    }
  }

  def findPrevSelect(
      tree: SelectLike,
      enclosed: Boolean = true
  ): Option[SelectLike] =
    findPrevSelectAndApply(tree.qual, enclosed)._1

  @tailrec
  private def findLastApplyAndNextSelectEnclosed(
      tree: Tree,
      select: Option[SelectLike] = None
  ): (Tree, Option[SelectLike]) =
    if (isEnclosedInParens(tree)) (tree, select)
    else
      tree.parent match {
        case Some(GetSelectLike(p)) =>
          findLastApplyAndNextSelectEnclosed(p.tree, select.orElse(Some(p)))
        case Some(p: Member.Apply) if p.fun eq tree =>
          findLastApplyAndNextSelectEnclosed(p, select)
        case _ => (tree, select)
      }

  @tailrec
  private def findLastApplyAndNextSelectPastEnclosed(
      tree: Tree,
      select: Option[SelectLike] = None,
      prevEnclosed: Option[Tree] = None
  ): (Tree, Option[SelectLike]) =
    tree.parent match {
      case Some(GetSelectLike(p)) =>
        findLastApplyAndNextSelectPastEnclosed(p.tree, select.orElse(Some(p)))
      case Some(p: Member.Apply) if p.fun eq tree =>
        prevEnclosed match {
          case Some(t) => (t, select)
          case _ =>
            val nextEnclosed = Some(tree).filter(isEnclosedInParens)
            findLastApplyAndNextSelectPastEnclosed(p, select, nextEnclosed)
        }
      case Some(p: Term.AnonymousFunction) =>
        findLastApplyAndNextSelectPastEnclosed(p, select, Some(p))
      case _ => (prevEnclosed.getOrElse(tree), select)
    }

  final def findLastApplyAndNextSelect(
      tree: Tree,
      enclosed: Boolean
  ): (Tree, Option[SelectLike]) =
    if (enclosed) findLastApplyAndNextSelectEnclosed(tree)
    else findLastApplyAndNextSelectPastEnclosed(tree)

  def canStartSelectChain(
      thisSelectLike: SelectLike,
      nextSelect: Option[Term],
      lastApply: Tree
  )(implicit style: ScalafmtConfig): Boolean = {
    val thisTree = thisSelectLike.tree
    val ok = thisTree.ne(lastApply) &&
      !cannotStartSelectChainOnExpr(thisSelectLike.qual)
    def checkParent = thisTree.parent match {
      case `nextSelect` => style.includeNoParensInSelectChains
      case Some(p: Term.Apply)
          if tokens.getHead(p.argClause).left.is[T.LeftBrace] =>
        style.includeCurlyBraceInSelectChains &&
        !nextSelect.contains(lastApply) // exclude short curly
      case Some(p: Member.Apply) => p.fun eq thisTree
      case _ => false
    }
    ok && (thisTree match {
      case _: Term.Match => // like select and apply in one
        val hasBrace =
          nextNonComment(tokens(thisSelectLike.nameToken)).right.is[T.LeftBrace]
        !hasBrace ||
        style.includeCurlyBraceInSelectChains &&
        nextSelect.isDefined && !nextSelect.contains(lastApply)
      case _ => checkParent
    })
  }

  /** Checks if an earlier select started the chain */
  @tailrec
  final def inSelectChain(
      prevSelect: Option[SelectLike],
      thisSelect: SelectLike,
      lastApply: Tree
  )(implicit style: ScalafmtConfig): Boolean =
    prevSelect match {
      case None => false
      case Some(p)
          if canStartSelectChain(p, Some(thisSelect.tree), lastApply) =>
        true
      case Some(p) =>
        val prevPrevSelect = findPrevSelect(p, style.encloseSelectChains)
        inSelectChain(prevPrevSelect, p, lastApply)
    }

  @tailrec
  final def findXmlLastLineIndent(ft: FormatToken): Int =
    ft.left match {
      case _: T.Xml.Start => 0
      case t: T.Xml.Part =>
        TokenOps.getXmlLastLineIndent(t) match {
          case Some(x) => x
          case None => findXmlLastLineIndent(prev(ft))
        }
      case t: T.Xml.SpliceEnd =>
        findXmlLastLineIndent(tokens(matching(t), -1))
      case _ =>
        findXmlLastLineIndent(prev(ft))
    }

  def withIndentOnXmlStart(tok: T.Xml.Start, splits: Seq[Split])(implicit
      style: ScalafmtConfig
  ): Seq[Split] = {
    if (style.xmlLiterals.assumeFormatted) {
      val end = matching(tok)
      val indent = Num(findXmlLastLineIndent(tokens(end, -1)), true)
      splits.map(_.withIndent(indent, end, ExpiresOn.After))
    } else splits
  }

  def withIndentOnXmlSpliceStart(ft: FormatToken, splits: Seq[Split])(implicit
      style: ScalafmtConfig
  ): Seq[Split] = {
    ft.left match {
      case t: T.Xml.SpliceStart if style.xmlLiterals.assumeFormatted =>
        val end = matching(t)
        val indent = Num(findXmlLastLineIndent(prev(ft)), true)
        splits.map(_.withIndent(indent, end, ExpiresOn.After))
      case _ => splits
    }
  }

  object CtrlBodySplits {

    private object CallSite {

      private val penalizeOpenNL: Policy.Pf = { case Decision(_, s) =>
        s.map(x => if (x.isNL) x.withPenalty(1) else x)
      }

      @tailrec
      private def getNestedOpens(
          tree: Member.ArgClause,
          res: List[FormatToken]
      ): List[FormatToken] = {
        val args = tree.values
        if (args.isEmpty) res
        else {
          val newres = tokens.getHead(tree) :: res
          args match {
            case (t: Member.Apply) :: Nil => getNestedOpens(t.argClause, newres)
            case _ => newres
          }
        }
      }

      private def getNestedOpensPolicy(
          opens: List[FormatToken],
          policy: Policy
      ): Policy = {
        opens.foldLeft(policy) { case (res, x) =>
          val onOpen = Policy(tokens.nextNonComment(x).right match {
            case t: T.LeftBrace => Policy.End.After(t)
            case t => Policy.End.On(t)
          })(penalizeOpenNL)
          val delayed = new Policy.Delay(onOpen, Policy.End.On(x.left))
          new Policy.Relay(delayed, res)
        }
      }

      @tailrec
      def getFoldedPolicy(
          body: Tree,
          policy: Policy = Policy.NoPolicy
      ): Policy =
        body match {
          case t: Member.Apply if t.fun ne body =>
            val opens = getNestedOpens(t.argClause, Nil)
            getFoldedPolicy(t.fun, getNestedOpensPolicy(opens, policy))
          case t: Init =>
            val opens = t.argClauses.foldLeft(List.empty[FormatToken]) {
              case (res, x) => getNestedOpens(x, res)
            }
            getNestedOpensPolicy(opens, policy)
          case t: Term.Select => getFoldedPolicy(t.qual, policy)
          case _ => policy
        }

    }

    def foldedNonEmptyNonComment(
        body: Tree,
        nlSplitFunc: Int => Split,
        isKeep: Boolean,
        spaceIndents: Seq[Indent] = Seq.empty
    ): Seq[Split] = {
      def bheadFT = tokens.getHead(body)
      val blastFT = tokens.getLastNonTrivial(body)
      val blast = blastFT.left
      val expire = nextNonCommentSameLine(blastFT).left
      def penalize(penalty: Int) =
        if (penalty <= 0) Policy.NoPolicy
        else new PolicyOps.PenalizeAllNewlines(Policy.End.On(blast), penalty)
      def getNlSplit(penalty: Int)(implicit fileLine: FileLine): Split = {
        nlSplitFunc(1).andPolicy(penalize(penalty)).forThisLine(nextLine)
      }
      def getSplits(spaceSplit: Split) =
        (
          spaceSplit.withIndents(spaceIndents),
          getNlSplit(1)(nextLine(spaceSplit.fileLine))
        )
      def getSlb(end: T, excl: TokenRanges)(implicit fileLine: FileLine) =
        SingleLineBlock(end, exclude = excl, noSyntaxNL = true)
      def getSlbSplit(
          end: T,
          exclude: TokenRanges = TokenRanges.empty,
          policy: Policy = Policy.NoPolicy
      )(implicit fileLine: FileLine) =
        Split(Space, 0)
          .withPolicy(policy | getSlb(end, exclude))
          .withOptimalToken(end, ignore = blast.start > end.start)
      def getSpaceSplit(
          penalty: Int,
          policy: Policy = Policy.NoPolicy
      )(implicit fileLine: FileLine) = {
        val spacePolicy = policy | penalize(penalty)
        Split(Space, 0).withPolicy(spacePolicy).withOptimalToken(blast)
      }
      def getPolicySplits(
          penalty: Int,
          policy: Policy
      )(implicit fileLine: FileLine) =
        getSplits(getSpaceSplit(penalty, policy))
      def getSlbSplits(
          exclude: TokenRanges = TokenRanges.empty,
          policy: Policy = Policy.NoPolicy
      )(implicit fileLine: FileLine) =
        (
          getSlbSplit(expire, exclude, policy),
          getNlSplit(if (policy.isEmpty) 0 else 1)
        )
      def hasStateColumn = spaceIndents.exists(_.hasStateColumn)
      @tailrec
      def getBlockStat(t: Tree): Tree = t match {
        case b: Term.Block =>
          getSingleStatExceptEndMarker(b.stats) match {
            case Some(s) if !isEnclosedInMatching(b) => getBlockStat(s)
            case _ => t
          }
        case _ => t
      }
      val adjustedBody = getBlockStat(body)
      val (spaceSplit, nlSplit) = adjustedBody match {
        case t: Term.If if isKeep || ifWithoutElse(t) || hasStateColumn =>
          val thenBeg = tokens.getHead(t.thenp)
          val thenHasLB = thenBeg.left.is[T.LeftBrace]
          val end = if (thenHasLB) thenBeg else prevNonCommentBefore(thenBeg)
          getSplits(getSlbSplit(end.left))
        case _: Term.If => getSlbSplits()
        case _: Term.Try | _: Term.TryWithHandler =>
          if (hasStateColumn) getSplits(getSpaceSplit(1))
          else getSlbSplits()
        case _: Term.Block | _: Term.Match | _: Type.Match |
            _: Term.NewAnonymous =>
          getSplits(getSpaceSplit(1))
        case Term.ForYield(_, b) =>
          nextNonComment(bheadFT).right match { // skipping `for`
            case x @ LeftParenOrBrace() =>
              val exclude = TokenRanges(TokenRange(x, matching(x)))
              if (b.is[Term.Block])
                getPolicySplits(1, getSlb(b.tokens.head, exclude))
              else getSlbSplits(exclude)
            case _ => getSlbSplits()
          }
        case ia: Member.Infix =>
          val lia = findLeftInfix(ia)
          val callPolicy = CallSite.getFoldedPolicy(lia.lhs)
          if (callPolicy.nonEmpty) getPolicySplits(0, callPolicy)
          else {
            // lia is enclosed in parens if and only if lia == ia (== body)
            val ok = isEnclosedInParens(body)
            getSplits(getSlbSplit(getLastToken(if (ok) lia.lhs else lia.op)))
          }
        case _ =>
          val callPolicy = CallSite.getFoldedPolicy(body)
          getPolicySplits(if (callPolicy.nonEmpty) 0 else 1, callPolicy)
      }

      Seq(spaceSplit, nlSplit)
    }

    private def foldedNonComment(
        body: Tree,
        nlSplitFunc: Int => Split,
        isKeep: Boolean,
        spaceIndents: Seq[Indent]
    ): Seq[Split] =
      if (body.tokens.isEmpty) Seq(Split(Space, 0))
      else foldedNonEmptyNonComment(body, nlSplitFunc, isKeep, spaceIndents)

    private def unfoldedSpaceNonEmptyNonComment(
        body: Tree,
        slbOnly: Boolean
    ): Split = {
      val expire = nextNonCommentSameLine(tokens.getLastNonTrivial(body)).left
      def slbSplit(end: T)(implicit fileLine: FileLine) =
        Split(Space, 0).withSingleLine(end, noSyntaxNL = true)
      body match {
        // we force newlines in for/yield
        case _: Term.ForYield => Split.ignored
        // we force newlines in try/catch/finally
        case _: Term.Try | _: Term.TryWithHandler => Split.ignored
        // don't tuck curried apply
        case t: Term.Apply if t.fun.is[Term.Apply] => slbSplit(expire)
        case EndOfFirstCall(end) if !slbOnly => slbSplit(end)
        case _ => slbSplit(expire)
      }
    }

    private def unfoldedNonComment(
        body: Tree,
        nlSplitFunc: Int => Split,
        spaceIndents: Seq[Indent],
        slbOnly: Boolean
    ): Seq[Split] =
      if (body.tokens.isEmpty) Seq(Split(Space, 0).withIndents(spaceIndents))
      else {
        val spaceSplit = unfoldedSpaceNonEmptyNonComment(body, slbOnly)
        Seq(spaceSplit.withIndents(spaceIndents), nlSplitFunc(1).forThisLine)
      }

    def checkComment(
        ft: FormatToken,
        nlSplitFunc: Int => Split
    )(splitsFunc: FormatToken => Seq[Split]): Seq[Split] =
      if (!ft.right.is[T.Comment]) splitsFunc(ft)
      else if (ft.hasBreak) Seq(nlSplitFunc(0).forThisLine)
      else {
        val nextFt = nextNonCommentSameLine(next(ft))
        val splits =
          if (nextFt.noBreak) splitsFunc(nextFt)
          else {
            val split = nlSplitFunc(0).forThisLine
            Seq(if (rhsIsCommentedOut(nextFt)) split.withNoIndent else split)
          }
        val policy = Policy.on(nextFt.right) { case Decision(`nextFt`, _) =>
          splits
        }
        Seq(Split(Space, 0, policy = policy))
      }

    def folded(
        ft: FormatToken,
        body: Tree,
        isKeep: Boolean,
        spaceIndents: Seq[Indent] = Seq.empty
    )(nlSplitFunc: Int => Split): Seq[Split] =
      checkComment(ft, nlSplitFunc) { _ =>
        foldedNonComment(body, nlSplitFunc, isKeep, spaceIndents)
      }

    def slbOnly(
        ft: FormatToken,
        body: Tree,
        spaceIndents: Seq[Indent] = Seq.empty
    )(nlSplitFunc: Int => Split): Seq[Split] =
      checkComment(ft, nlSplitFunc) { _ =>
        unfoldedNonComment(body, nlSplitFunc, spaceIndents, true)
      }

    def get(
        ft: FormatToken,
        body: Tree,
        spaceIndents: Seq[Indent] = Seq.empty
    )(classicNoBreakFunc: => Split)(nlSplitFunc: Int => Split)(implicit
        style: ScalafmtConfig
    ): Seq[Split] =
      checkComment(ft, nlSplitFunc) { x =>
        style.newlines.getBeforeMultiline match {
          case Newlines.unfold =>
            unfoldedNonComment(body, nlSplitFunc, spaceIndents, false)
          case Newlines.classic | Newlines.keep if x.hasBreak =>
            Seq(nlSplitFunc(0).forThisLine)
          case Newlines.classic =>
            Option(classicNoBreakFunc).fold {
              foldedNonComment(body, nlSplitFunc, isKeep = true, spaceIndents)
            } { func =>
              val spcSplit = func.forThisLine
              val nlSplit = nlSplitFunc(spcSplit.getCost(_ + 1, 0)).forThisLine
              Seq(spcSplit, nlSplit)
            }
          case sh => // fold or keep without break
            val isKeep = sh eq Newlines.keep
            foldedNonComment(body, nlSplitFunc, isKeep, spaceIndents)
        }
      }

    def getWithIndent(
        ft: FormatToken,
        body: Tree,
        spaceIndents: Seq[Indent] = Seq.empty
    )(classicNoBreakFunc: => Split)(nlSplitFunc: Int => Split)(implicit
        style: ScalafmtConfig
    ): Seq[Split] =
      get(ft, body, spaceIndents)(classicNoBreakFunc)(x =>
        withIndent(nlSplitFunc(x), ft, body)
      )

    def withIndent(nlSplit: Split, ft: FormatToken, body: Tree)(implicit
        style: ScalafmtConfig
    ): Split =
      asInfixApp(body).fold {
        val expire = tokens.nextNonCommentSameLine(tokens.getLast(body)).left
        nlSplit.withIndent(Num(style.indent.main), expire, ExpiresOn.After)
      }(app => InfixSplits.withNLIndent(nlSplit)(app, ft))

  }

  // Redundant () delims around case statements
  def isCaseBodyEnclosedAsBlock(ft: FormatToken, caseStat: CaseTree)(implicit
      style: ScalafmtConfig
  ): Boolean = {
    val body = caseStat.body
    (ft.noBreak || style.newlines.getBeforeMultiline.ignoreSourceSplit) &&
    body.eq(ft.meta.rightOwner) && (body match {
      case _: Lit.Unit | _: Term.Tuple => false
      case t: Term.ApplyInfix =>
        val op = t.op.value
        op != "->" && op != "→"
      case _ => true
    }) && isEnclosedInParens(body)
  }

  def getMatchDot(tree: Term.Match): Option[FormatToken] =
    if (dialect.allowMatchAsOperator) {
      val ft = tokens.tokenAfter(tree.expr)
      if (ft.right.is[T.Dot]) Some(ft) else None
    } else None

  def getKwMatchAfterDot(ft: FormatToken): T.KwMatch =
    nextNonComment(next(ft)).right.asInstanceOf[T.KwMatch]

  object GetSelectLike {
    def unapply(tree: Tree): Option[SelectLike] = tree match {
      case t: Term.Select => Some(SelectLike(t))
      case t: Term.Match =>
        getMatchDot(t).map { ft => SelectLike(t, getKwMatchAfterDot(ft)) }
      case _ => None
    }
  }

  def getSplitsForTypeBounds(
      ft: FormatToken,
      noNLMod: => Modification,
      tparam: Type.Param,
      bounds: Type.Param => Seq[Type]
  )(implicit style: ScalafmtConfig): Seq[Split] = {
    val boundOpt = bounds(tparam).find(_.pos.start > ft.right.end)
    val expireOpt = boundOpt.map(getLastNonTrivialToken)
    getSplitsForTypeBounds(ft, noNLMod, tparam, expireOpt)
  }

  def getSplitsForTypeBounds(
      ft: FormatToken,
      noNLMod: => Modification,
      typeOwner: Tree,
      boundEndOpt: Option[T]
  )(implicit style: ScalafmtConfig): Seq[Split] = {
    val typeEnd = getLastNonTrivialToken(typeOwner)
    val boundEnd = boundEndOpt.getOrElse(typeEnd)
    def indent = Indent(Num(style.indent.main), boundEnd, ExpiresOn.After)
    def unfoldPolicy = typeOwner match {
      case tparam: Type.Param =>
        Policy.on(typeEnd) {
          case Decision(t @ FormatToken(_, _: T.Colon | _: T.Viewbound, _), s)
              if t.meta.rightOwner eq tparam =>
            Decision.onlyNewlineSplits(s)
        }
      case _ => NoPolicy
    }
    style.newlines.beforeTypeBounds match {
      case Newlines.classic =>
        Seq(Split(noNLMod, 0))
      case Newlines.unfold =>
        Seq(
          Split(noNLMod, 0).withSingleLine(typeEnd),
          Split(Newline, 1).withIndent(indent).withPolicy(unfoldPolicy)
        )
      case Newlines.keep if ft.hasBreak =>
        Seq(Split(Newline, 1).withIndent(indent))
      case _ =>
        Seq(
          Split(noNLMod, 0).withSingleLine(boundEnd),
          Split(Newline, 1).withIndent(indent)
        )
    }
  }

  object OptionalBraces {

    private trait Factory {
      def create(ft: FormatToken, nft: FormatToken)(implicit
          style: ScalafmtConfig
      ): Option[OptionalBracesRegion]
    }

    // Optional braces in templates after `:|with`
    // Optional braces after any token that can start indentation:
    // )  =  =>  ?=>  <-  catch  do  else  finally  for
    // if  match  return  then  throw  try  while  yield
    def unapply(
        ftMeta: FormatToken.Meta
    )(implicit style: ScalafmtConfig): Option[Seq[Split]] =
      get(tokens(ftMeta.idx)).flatMap(_.splits)

    def get(ft: FormatToken)(implicit
        style: ScalafmtConfig
    ): Option[OptionalBracesRegion] = {
      if (!style.dialect.allowSignificantIndentation) None
      else
        Option(ft.left match {
          case _: T.Colon | _: T.KwWith => ColonEolImpl
          case _: T.RightArrow => RightArrowImpl
          case _: T.RightParen => RightParenImpl
          case _: T.KwFor => ForImpl
          case _: T.KwWhile => WhileImpl
          case _: T.KwDo => DoImpl
          case _: T.Equals => EqualsImpl
          case _: T.KwTry => TryImpl
          case _: T.KwCatch => CatchImpl
          case _: T.KwFinally => FinallyImpl
          case _: T.KwMatch => MatchImpl
          case _: T.KwThen => ThenImpl
          case _: T.KwIf => IfImpl
          case _: T.KwElse => ElseImpl
          case _: T.KwReturn | _: T.ContextArrow | _: T.LeftArrow |
              _: T.KwThrow | _: T.KwYield =>
            BlockImpl
          case _ => null
        }).flatMap { impl =>
          val nft = nextNonComment(ft)
          impl.create(ft, nft).filter { ob =>
            !nft.right.is[T.LeftBrace] || nft.meta.rightOwner.parent != ob.owner
          }
        }
    }

    private def getSplits(
        ft: FormatToken,
        tree: Tree,
        forceNL: Boolean,
        danglingKeyword: Boolean = true,
        indentOpt: Option[Int] = None
    )(implicit fileLine: FileLine, style: ScalafmtConfig): Seq[Split] = {
      val treeTokens = tree.tokens
      val end = tokens.getLast(treeTokens, tree)
      val slbExpire = nextNonCommentSameLine(end).left
      val closeOpt =
        if (isTuple(tree)) None
        else {
          val maybeClose = prevNonComment(end)
          tokens
            .getClosingIfInParens(maybeClose)(tokens.getHead(treeTokens, tree))
            .map(prevNonComment(_).left)
        }
      def nlPolicy(implicit fileLine: FileLine) =
        if (danglingKeyword)
          decideNewlinesOnlyAfterClose(closeOpt.getOrElse(slbExpire))
        else NoPolicy
      val indentLen = indentOpt.getOrElse(style.indent.getSignificant)
      val indent =
        Indent(Num(indentLen), closeOpt.getOrElse(end.left), ExpiresOn.After)
      if (ft.hasBlankLine)
        Seq(Split(Newline2x, 0).withIndent(indent).withPolicy(nlPolicy))
      else if (forceNL)
        Seq(Split(Newline, 0).withIndent(indent).withPolicy(nlPolicy))
      else {
        Seq(
          Split(Space, 0).withSingleLine(slbExpire),
          Split(Newline, 1)(nextLine).withIndent(indent).withPolicy(nlPolicy)
        )
      }
    }

    // https://dotty.epfl.ch/docs/reference/other-new-features/indentation.html#variant-indentation-marker-
    // TODO: amend for additional cases when the parser supports them
    private object ColonEolImpl extends Factory {
      def create(ft: FormatToken, nft: FormatToken)(implicit
          style: ScalafmtConfig
      ): Option[OptionalBracesRegion] =
        ft.meta.leftOwner match {
          case t: Template if templateCurlyFt(t).contains(ft) =>
            Some(new OptionalBracesRegion {
              def owner = t.parent
              def splits = Some(getSplits(ft, t, forceNL = true))
              def rightBrace = if (isSeqMulti(t.stats)) treeLast(t) else None
            })
          case t: Pkg if tokenAfter(t.ref).right eq ft.left =>
            Some(new OptionalBracesRegion {
              def owner = Some(t)
              def splits = Some(getSplits(ft, t, forceNL = true))
              def rightBrace = if (isSeqMulti(t.stats)) treeLast(t) else None
            })
          case t: Type.Refine =>
            Some(new OptionalBracesRegion {
              def owner = Some(t)
              def splits = Some(getSplits(ft, t, forceNL = true))
              def rightBrace = treeLast(t)
            })
          case t: Term.ArgClause if tokens.getHead(t) eq ft =>
            def funcSplit(arg: Term.FunctionTerm)(implicit fl: FileLine) = {
              val end = tokens.getLast(arg)
              val opt = getOptimalTokenFor(getFuncArrow(arg).getOrElse(end))
              Split(Space, 0)
                .withSingleLine(opt)
                .andPolicy(decideNewlinesOnlyAfterToken(opt))
            }
            val indent = t.parent match {
              case Some(p: Term.Apply) =>
                @tailrec
                def isSelect(ma: Member.Apply): Boolean = ma.fun match {
                  case x: Member.Apply => isSelect(x)
                  case x => x.is[Term.Select]
                }
                if (isSelect(p)) None // select is taken care off elsewhere
                else Some(style.indent.main + style.indent.getSignificant)
              case _ => None
            }
            Some(new OptionalBracesRegion {
              def owner = t.parent
              def splits = Some(t.values match {
                case (tf: Term.FunctionTerm) :: Nil
                    if !style.newlines.alwaysBeforeCurlyLambdaParams &&
                      t.parent.exists(_.is[Term.Apply]) =>
                  getSplits(ft, t, forceNL = false, indentOpt = indent) match {
                    case s +: rs if !s.isNL => funcSplit(tf)(s.fileLine) +: rs
                    case ss => ss
                  }
                case _ => getSplits(ft, t, forceNL = true, indentOpt = indent)
              })
              def rightBrace = treeLast(t)
            })
          case _ => None
        }
    }

    private object BlockImpl extends Factory {
      def create(ft: FormatToken, nft: FormatToken)(implicit
          style: ScalafmtConfig
      ): Option[OptionalBracesRegion] = {
        val leftOwner = ft.meta.leftOwner
        findTreeWithParentSimple(nft.meta.rightOwner)(_ eq leftOwner) match {
          case Some(t: Term.Block)
              if !hasSingleTermStat(t) && isBlockStart(t, nft) =>
            Some(new OptionalBracesRegion {
              def owner = t.parent
              def splits = Some(getSplitsMaybeBlock(ft, nft, t))
              def rightBrace = treeLast(t)
            })
          case _ => None
        }
      }
    }

    private object RightParenImpl extends Factory {
      def create(ft: FormatToken, nft: FormatToken)(implicit
          style: ScalafmtConfig
      ): Option[OptionalBracesRegion] =
        ft.meta.leftOwner match {
          case ParamClauseParent(t: Defn.ExtensionGroup)
              if isBlockStart(t.body, nft) =>
            Some(new OptionalBracesRegion {
              def owner = Some(t)
              def splits = Some(getSplitsMaybeBlock(ft, nft, t.body))
              def rightBrace = blockLast(t.body)
            })
          case t: Term.If if !nft.right.is[T.KwThen] && {
                isTreeMultiStatBlock(t.thenp) || !ifWithoutElse(t) &&
                (isElsePWithOptionalBraces(t) ||
                  existsBlockIfWithoutElse(t.thenp, false))
              } =>
            Some(new OptionalBracesRegion {
              def owner = Some(t)
              def splits = Some(getSplitsForIf(ft, nft, t))
              def rightBrace = blockLast(t.thenp)
            })
          case t @ Term.For(_, b) if !nft.right.is[T.KwDo] =>
            Some(new OptionalBracesRegion {
              def owner = Some(t)
              def splits =
                if (!isTreeMultiStatBlock(b)) None
                else Some(getSplits(ft, b, true))
              def rightBrace = blockLast(b)
            })
          case t @ Term.While(_, b) if !nft.right.is[T.KwDo] =>
            Some(new OptionalBracesRegion {
              def owner = Some(t)
              def splits =
                if (!isTreeMultiStatBlock(b)) None
                else Some(getSplits(ft, b, true))
              def rightBrace = blockLast(b)
            })
          case _ => None
        }
    }

    private object RightArrowImpl extends Factory {
      def create(ft: FormatToken, nft: FormatToken)(implicit
          style: ScalafmtConfig
      ): Option[OptionalBracesRegion] =
        ft.meta.leftOwner match {
          case t: Case => // unsupported except for right brace
            Some(new OptionalBracesRegion {
              def owner = None
              def splits = None
              def rightBrace = blockLast(t.body)
            })
          case _ => BlockImpl.create(ft, nft)
        }
    }

    private object ForImpl extends Factory {
      def create(ft: FormatToken, nft: FormatToken)(implicit
          style: ScalafmtConfig
      ): Option[OptionalBracesRegion] =
        ft.meta.leftOwner match {
          case t @ Term.For(enums, _) if isSeqMulti(enums) =>
            Some(new OptionalBracesRegion {
              def owner = Some(t)
              def splits = getSplitsForStats(ft, nft, enums)
              def rightBrace = seqLast(enums)
            })
          case t @ Term.ForYield(enums, _) if isSeqMulti(enums) =>
            Some(new OptionalBracesRegion {
              def owner = Some(t)
              def splits = getSplitsForStats(ft, nft, enums)
              def rightBrace = seqLast(enums)
            })
          case _ => BlockImpl.create(ft, nft)
        }
    }

    private object WhileImpl extends Factory {
      def create(ft: FormatToken, nft: FormatToken)(implicit
          style: ScalafmtConfig
      ): Option[OptionalBracesRegion] =
        ft.meta.leftOwner match {
          case t @ Term.While(b: Term.Block, _)
              if !matchingOpt(nft.right).exists(_.end >= b.pos.end) =>
            Some(new OptionalBracesRegion {
              def owner = Some(t)
              def splits = Some {
                val dangle = style.danglingParentheses.ctrlSite
                val forceNL = !nft.right.is[T.LeftParen]
                getSplits(ft, b, forceNL = forceNL, danglingKeyword = dangle)
              }
              def rightBrace = blockLast(b)
            })
          case _ => None
        }
    }

    private object DoImpl extends Factory {
      def create(ft: FormatToken, nft: FormatToken)(implicit
          style: ScalafmtConfig
      ): Option[OptionalBracesRegion] =
        ft.meta.leftOwner match {
          case t @ Term.While(_, body) =>
            Some(new OptionalBracesRegion {
              def owner = Some(t)
              def splits = Some(getSplitsMaybeBlock(ft, nft, body))
              def rightBrace = blockLast(body)
            })
          case t @ Term.For(_, body) =>
            Some(new OptionalBracesRegion {
              def owner = Some(t)
              def splits = Some(getSplitsMaybeBlock(ft, nft, body))
              def rightBrace = blockLast(body)
            })
          case _ => None
        }
    }

    private object EqualsImpl extends Factory {
      def create(ft: FormatToken, nft: FormatToken)(implicit
          style: ScalafmtConfig
      ): Option[OptionalBracesRegion] =
        ft.meta.leftOwner match {
          case t: Ctor.Secondary =>
            Some(new OptionalBracesRegion {
              def owner = Some(t)
              def splits =
                if (t.stats.isEmpty) None
                else getSplitsForStatsImpl(ft, nft, t.init, t.stats)
              def rightBrace = treeLast(t)
            })
          case t @ Tree.WithBody(x: Term.PartialFunction) =>
            Some(new OptionalBracesRegion {
              def owner = Some(t)
              def splits = getSplitsForStats(ft, nft, x.cases, nlOnly = true)
              def rightBrace = treeLast(x)
            })
          case _ => BlockImpl.create(ft, nft)
        }
    }

    private object TryImpl extends Factory {
      def create(ft: FormatToken, nft: FormatToken)(implicit
          style: ScalafmtConfig
      ): Option[OptionalBracesRegion] = {
        def trySplits(expr: Term, finallyp: Option[Term], usesOB: => Boolean) =
          if (isTreeMultiStatBlock(expr)) Some(getSplits(ft, expr, true))
          else if (finallyp.exists(isTreeUsingOptionalBraces) || usesOB)
            Some(getSplits(ft, expr, shouldBreakInOptionalBraces(nft)))
          else None
        ft.meta.leftOwner match {
          case t @ Term.Try(expr, _, finallyp) =>
            Some(new OptionalBracesRegion {
              def owner = Some(t)
              def splits =
                trySplits(expr, finallyp, isCatchUsingOptionalBraces(t))
              def rightBrace = blockLast(expr)
            })
          case t @ Term.TryWithHandler(expr, _, finallyp) =>
            Some(new OptionalBracesRegion {
              def owner = Some(t)
              def splits = trySplits(expr, finallyp, false)
              def rightBrace = blockLast(expr)
            })
          case _ => None
        }
      }
    }

    private def isCatchUsingOptionalBraces(tree: Term.Try): Boolean =
      tree.catchp.headOption.exists(x => !tokenBefore(x).left.is[T.LeftBrace])

    private object CatchImpl extends Factory {
      def create(ft: FormatToken, nft: FormatToken)(implicit
          style: ScalafmtConfig
      ): Option[OptionalBracesRegion] =
        ft.meta.leftOwner match {
          case t @ Term.Try(_, catchp, _) =>
            Some(new OptionalBracesRegion {
              def owner = Some(t)
              def splits = getSplitsForStats(ft, nft, catchp)
              def rightBrace = seqLast(catchp)
            })
          case _ => None
        }
    }

    private object FinallyImpl extends Factory {
      def create(ft: FormatToken, nft: FormatToken)(implicit
          style: ScalafmtConfig
      ): Option[OptionalBracesRegion] = {
        val forceNL = shouldBreakInOptionalBraces(nft)
        ft.meta.leftOwner match {
          case t: Term.Try =>
            t.finallyp.map { x =>
              val usesOB = isTreeMultiStatBlock(x) ||
                isCatchUsingOptionalBraces(t) ||
                isTreeUsingOptionalBraces(t.expr)
              new OptionalBracesRegion {
                def owner = Some(t)
                def splits =
                  if (usesOB) Some(getSplits(ft, x, forceNL)) else None
                def rightBrace = blockLast(x)
              }
            }
          case t: Term.TryWithHandler =>
            t.finallyp.map { x =>
              val usesOB = isTreeMultiStatBlock(x) ||
                isTreeUsingOptionalBraces(t.expr)
              new OptionalBracesRegion {
                def owner = Some(t)
                def splits =
                  if (usesOB) Some(getSplits(ft, x, forceNL)) else None
                def rightBrace = blockLast(x)
              }
            }
          case _ => None
        }
      }
    }

    private object MatchImpl extends Factory {
      def create(ft: FormatToken, nft: FormatToken)(implicit
          style: ScalafmtConfig
      ): Option[OptionalBracesRegion] = {
        def result(tree: Tree, cases: Seq[Tree]): Option[Seq[Split]] =
          if (tokens.tokenJustBeforeOpt(cases).contains(nft))
            Some(getSplits(ft, tree, true, indentOpt = style.indent.matchSite))
          else None
        ft.meta.leftOwner match {
          case t: Term.Match =>
            Some(new OptionalBracesRegion {
              def owner = Some(t)
              def splits = result(t, t.cases)
              def rightBrace = treeLast(t)
            })
          case t: Type.Match =>
            Some(new OptionalBracesRegion {
              def owner = Some(t)
              def splits = result(t, t.cases)
              def rightBrace = treeLast(t)
            })
          case _ => None
        }
      }
    }

    private object ThenImpl extends Factory {
      def create(ft: FormatToken, nft: FormatToken)(implicit
          style: ScalafmtConfig
      ): Option[OptionalBracesRegion] =
        ft.meta.leftOwner match {
          case t: Term.If =>
            Some(new OptionalBracesRegion {
              def owner = Some(t)
              def splits = Some(getSplitsForIf(ft, nft, t))
              def rightBrace = blockLast(t.thenp)
            })
          case _ => None
        }
    }

    private object IfImpl extends Factory {
      def create(ft: FormatToken, nft: FormatToken)(implicit
          style: ScalafmtConfig
      ): Option[OptionalBracesRegion] = {
        ft.meta.leftOwner match {
          case t: Term.If =>
            t.cond match {
              case b: Term.Block
                  if !matchingOpt(nft.right).exists(_.end >= b.pos.end) =>
                Some(new OptionalBracesRegion {
                  def owner = Some(t)
                  def splits = Some {
                    val dangle = style.danglingParentheses.ctrlSite
                    val forceNL = !nft.right.is[T.LeftParen]
                    getSplits(ft, b, forceNL, dangle)
                  }
                  def rightBrace = blockLast(b)
                })
              case _ => None
            }
          case _ => None
        }
      }
    }

    private object ElseImpl extends Factory {
      def create(ft: FormatToken, nft: FormatToken)(implicit
          style: ScalafmtConfig
      ): Option[OptionalBracesRegion] =
        ft.meta.leftOwner match {
          case t: Term.If =>
            (t.elsep match {
              case _: Term.If => None
              case x if isTreeMultiStatBlock(x) => Some(true)
              case _ if isThenPWithOptionalBraces(t) =>
                Some(shouldBreakInOptionalBraces(nft))
              case _ => None
            }).map { forceNL =>
              new OptionalBracesRegion {
                def owner = Some(t)
                def splits = Some(getSplits(ft, t.elsep, forceNL))
                def rightBrace = blockLast(t.elsep)
              }
            }
          case _ => None
        }
    }

    private def getSplitsMaybeBlock(
        ft: FormatToken,
        nft: FormatToken,
        tree: Tree,
        danglingKeyword: Boolean = true
    )(implicit fileLine: FileLine, style: ScalafmtConfig): Seq[Split] = {
      val forceNL =
        !hasSingleTermStatIfBlock(tree) || shouldBreakInOptionalBraces(nft)
      getSplits(ft, tree, forceNL, danglingKeyword)
    }

    private def getSplitsForStatsImpl(
        ft: FormatToken,
        nft: FormatToken,
        head: => Tree,
        tail: Seq[Tree],
        nlOnly: Boolean = false
    )(implicit
        fileLine: FileLine,
        style: ScalafmtConfig
    ): Option[Seq[Split]] =
      if (tokens.tokenJustBeforeOpt(head).contains(nft)) Some {
        val forceNL = nlOnly || shouldBreakInOptionalBraces(ft)
        getSplits(ft, tail.lastOption.getOrElse(head), forceNL)
      }
      else None

    private def getSplitsForStats(
        ft: FormatToken,
        nft: FormatToken,
        trees: Seq[Tree],
        nlOnly: Boolean = false
    )(implicit
        fileLine: FileLine,
        style: ScalafmtConfig
    ): Option[Seq[Split]] = trees.headOption.flatMap { head =>
      getSplitsForStatsImpl(ft, nft, head, trees.tail, nlOnly)
    }

    private def getSplitsForIf(
        ft: FormatToken,
        nft: FormatToken,
        t: Term.If
    )(implicit fileLine: FileLine, style: ScalafmtConfig): Seq[Split] = {
      def nestedIf(x: Term.If) = {
        val forceNL = shouldBreakInOptionalBraces(nft) ||
          !ifWithoutElse(t) && existsIfWithoutElse(x)
        getSplits(ft, t.thenp, forceNL)
      }
      t.thenp match {
        case x: Term.If => nestedIf(x)
        case Term.Block(List(x: Term.If)) => nestedIf(x)
        case x => getSplitsMaybeBlock(ft, nft, x, false)
      }
    }

    private def isThenPWithOptionalBraces(tree: Term.If): Boolean = {
      val thenp = tree.thenp
      val before = tokens.tokenJustBefore(thenp)
      prevNonComment(before).left match {
        case _: T.KwThen => true
        case _: T.LeftBrace => false
        case _ =>
          isTreeMultiStatBlock(thenp) && (!before.right.is[T.LeftBrace] ||
            matchingOpt(before.right).exists(rb => rb.end < thenp.pos.end))
      }
    }

    @tailrec
    private def isElsePWithOptionalBraces(tree: Term.If): Boolean = {
      val elsep = tree.elsep
      !tokens.getHead(elsep).left.is[T.LeftBrace] && (elsep match {
        case t: Term.If =>
          isThenPWithOptionalBraces(t) ||
          !ifWithoutElse(t) && isElsePWithOptionalBraces(t)
        case Term.Block(List(t: Term.If)) =>
          isThenPWithOptionalBraces(t) ||
          !ifWithoutElse(t) && isElsePWithOptionalBraces(t)
        case t => isTreeMultiStatBlock(t)
      })
    }

    private def shouldBreakInOptionalBraces(
        ft: FormatToken
    )(implicit style: ScalafmtConfig): Boolean =
      style.newlines.source match {
        case Newlines.unfold => true
        case Newlines.keep => ft.hasBreak
        case _ => false
      }

    private def isTreeUsingOptionalBraces(tree: Tree): Boolean =
      isTreeMultiStatBlock(tree) && !tokenBefore(tree).left.is[T.LeftBrace]

    private def isBlockStart(tree: Term.Block, ft: FormatToken): Boolean =
      tokens.tokenJustBeforeOpt(tree.stats).contains(ft)

    private def isBlockStart(tree: Tree, ft: FormatToken): Boolean =
      tree match {
        case t: Term.Block => isBlockStart(t, ft)
        case _ => false
      }

    @inline private def treeLast(tree: Tree): Option[T] =
      tokens.getLastOpt(tree).map(_.left)
    @inline private def blockLast(tree: Tree): Option[T] =
      if (isTreeMultiStatBlock(tree)) treeLast(tree) else None
    @inline private def blockLast(tree: Term.Block): Option[T] =
      if (isMultiStatBlock(tree)) treeLast(tree) else None
    @inline private def seqLast(seq: Seq[Tree]): Option[T] =
      if (isSeqMulti(seq)) treeLast(seq.last) else None

    def indentAndBreakBeforeCtrl[A](tree: Tree, split: Split)(implicit
        style: ScalafmtConfig,
        classifier: Classifier[T, A]
    ): Option[Split] =
      if (
        !style.dialect.allowSignificantIndentation ||
        tree.is[Term.Block] && !split.isNL && isEnclosedInMatching(tree)
      ) None
      else {
        val kw = tokenAfter(tree).right
        if (kw.is[A]) Some {
          val indent =
            style.indent.ctrlSite.getOrElse(style.indent.getSignificant)
          def policy =
            if (split.isNL) decideNewlinesOnlyBeforeClose(kw)
            else decideNewlinesOnlyBeforeCloseOnBreak(kw)
          split
            .withIndent(Num(indent), kw, ExpiresOn.Before)
            .andPolicy(policy, !style.danglingParentheses.ctrlSite)
        }
        else None
      }

  }

  object MissingBraces {

    type Ranges = Seq[(Tree, Tree)]
    type Result = Option[(Tree, Ranges)]

    private trait Factory {
      def getBlocks(ft: FormatToken, nft: FormatToken, all: Boolean): Result
    }

    def getBlocks(ft: FormatToken, all: Boolean): Result = {
      val nft = nextNonComment(ft)
      if (nft.right.is[T.LeftBrace]) None
      else {
        val impl = ft.left match {
          case _: T.RightArrow => RightArrowImpl
          case _: T.RightParen => RightParenImpl
          case _: T.RightBrace => RightBraceImpl
          case _: T.KwDo => DoImpl
          case _: T.Equals => EqualsImpl
          case _: T.KwTry => TryImpl
          case _: T.KwCatch => CatchImpl
          case _: T.KwFinally => FinallyImpl
          case _: T.KwElse => ElseImpl
          case _: T.KwYield => YieldImpl
          case _ => null
        }
        Option(impl).flatMap(_.getBlocks(ft, nft, all))
      }
    }

    private def seq(all: Boolean, t: Tree): Ranges =
      if (all) Seq(t -> t) else Nil

    private def seq(all: Boolean, t: Option[Tree]): Ranges =
      t.map(seq(all, _)).getOrElse(Nil)

    private def seq(all: Boolean, t: Seq[Tree]): Ranges =
      if (all && t.nonEmpty) Seq(t.head -> t.last) else Nil

    private def seq(all: Boolean, t: Tree, ts: Seq[Tree]): Ranges =
      if (all) Seq(t -> ts.lastOption.getOrElse(t)) else Nil

    private object BlockImpl extends Factory {
      def getBlocks(ft: FormatToken, nft: FormatToken, all: Boolean): Result = {
        def ok(stat: Tree): Boolean =
          tokens.tokenJustBeforeOpt(stat).contains(nft)
        val leftOwner = ft.meta.leftOwner
        findTreeWithParentSimple(nft.meta.rightOwner)(_ eq leftOwner) match {
          case Some(t: Term.Block) =>
            if (t.stats.headOption.exists(ok)) Some((t, Nil)) else None
          case x => x.filter(ok).map((_, Nil))
        }
      }
    }

    private object RightArrowImpl extends Factory {
      def getBlocks(ft: FormatToken, nft: FormatToken, all: Boolean): Result =
        ft.meta.leftOwner match {
          case t: Term.Function =>
            val skip = t.parent.exists(_.is[Term.Block])
            if (skip) None else Some((t.body, seq(all, t.paramClause.values)))
          case _ => None
        }
    }

    private object RightParenImpl extends Factory {
      def getBlocks(ft: FormatToken, nft: FormatToken, all: Boolean): Result =
        ft.meta.leftOwner match {
          case x: Term.If if !nft.right.is[T.KwThen] =>
            val hasElse = all && !ifWithoutElse(x)
            Some((x.thenp, seq(hasElse, x.elsep) ++ seq(all, x.cond)))
          case Term.For(s, b) if !nft.right.is[T.KwDo] =>
            Some((b, seq(all, s)))
          case Term.While(c, b) if !nft.right.is[T.KwDo] =>
            Some((b, seq(all, c)))
          case _ => None
        }
    }

    private object RightBraceImpl extends Factory {
      def getBlocks(ft: FormatToken, nft: FormatToken, all: Boolean): Result =
        ft.meta.leftOwner match {
          case t @ Term.For(s, b)
              if !nft.right.is[T.KwDo] && !isTokenLastOrAfter(ft.left, t) =>
            Some((b, seq(all, s)))
          case _ => None
        }
    }

    private object DoImpl extends Factory {
      def getBlocks(ft: FormatToken, nft: FormatToken, all: Boolean): Result =
        ft.meta.leftOwner match {
          case Term.Do(b, c) => Some((b, seq(all, c)))
          case _ => None
        }
    }

    private object EqualsImpl extends Factory {
      def getBlocks(ft: FormatToken, nft: FormatToken, all: Boolean): Result =
        ft.meta.leftOwner match {
          case t: Ctor.Secondary => Some((t, seq(all, t.init, t.stats)))
          case t: Tree.WithBody => Some((t.body, Nil))
          case _ => BlockImpl.getBlocks(ft, nft, all)
        }
    }

    private object TryImpl extends Factory {
      def getBlocks(ft: FormatToken, nft: FormatToken, all: Boolean): Result =
        ft.meta.leftOwner match {
          case t: Term.Try =>
            Some((t.expr, seq(all, t.catchp) ++ seq(all, t.finallyp)))
          case t: Term.TryWithHandler =>
            Some((t.expr, seq(all, t.catchp) ++ seq(all, t.finallyp)))
          case _ => None
        }
    }

    private object CatchImpl extends Factory {
      def getBlocks(ft: FormatToken, nft: FormatToken, all: Boolean): Result =
        ft.meta.leftOwner match {
          case t: Term.Try =>
            Some((t.catchp.last, seq(all, t.expr) ++ seq(all, t.finallyp)))
          case t: Term.TryWithHandler =>
            Some((t.catchp, seq(all, t.expr) ++ seq(all, t.finallyp)))
          case _ => None
        }
    }

    private object FinallyImpl extends Factory {
      def getBlocks(ft: FormatToken, nft: FormatToken, all: Boolean): Result =
        ft.meta.leftOwner match {
          case t: Term.Try =>
            t.finallyp.map(x => (x, seq(all, t.expr) ++ seq(all, t.catchp)))
          case t: Term.TryWithHandler =>
            t.finallyp.map(x => (x, seq(all, t.expr) ++ seq(all, t.catchp)))
          case _ => None
        }
    }

    private object ElseImpl extends Factory {
      def getBlocks(ft: FormatToken, nft: FormatToken, all: Boolean): Result =
        ft.meta.leftOwner match {
          case x: Term.If if !x.elsep.is[Term.If] =>
            Some((x.elsep, seq(all, x.thenp) ++ seq(all, x.cond)))
          case _ => None
        }
    }

    private object YieldImpl extends Factory {
      def getBlocks(ft: FormatToken, nft: FormatToken, all: Boolean): Result =
        ft.meta.leftOwner match {
          case Term.ForYield(s, b) => Some((b, seq(all, s)))
          case _ => None
        }
    }

  }

  def isBlockWithoutBraces(t: Term.Block): Boolean =
    t.tokens.head match {
      case lb: T.LeftBrace => lb ne tokens(lb).left
      case _ => true
    }

  def existsBlockIfWithoutElse(t: Term.If): Boolean =
    existsBlockIfWithoutElse(t.thenp, false) ||
      existsBlockIfWithoutElse(t.elsep, ifWithoutElse(t))

  def existsBlockIfWithoutElse(t: Tree, other: => Boolean): Boolean = t match {
    case x: Term.If => existsBlockIfWithoutElse(x)
    case b @ Term.Block(List(x: Term.If)) =>
      isBlockWithoutBraces(b) && existsBlockIfWithoutElse(x)
    case _ => other
  }

  def getLastToken(tree: Tree): T =
    tokens.getLast(tree).left

  def getLastTokenOpt(tree: Tree): Option[T] =
    tokens.getLastOpt(tree).map(_.left)

  def getLastNonTrivialToken(tree: Tree): T =
    tokens.getLastNonTrivial(tree).left

  def getLastNonTrivialTokenOpt(tree: Tree): Option[T] =
    tokens.getLastNonTrivialOpt(tree).map(_.left)

  def getEndOfBlock(ft: FormatToken, parensToo: Boolean)(implicit
      style: ScalafmtConfig
  ): Option[T] =
    ft.left match {
      case x: T.LeftBrace => matchingOpt(x)
      case x: T.LeftParen => if (parensToo) matchingOpt(x) else None
      case _ => OptionalBraces.get(ft).flatMap(_.rightBrace)
    }

  def isCloseDelimForTrailingCommasMultiple(ft: FormatToken): Boolean =
    ft.meta.rightOwner match {
      case x: Importer => x.importees.lengthCompare(1) > 0
      case x => // take last arg when multiple
        getArgs(x).view.drop(1).lastOption match {
          case None | Some(_: Term.Repeated) => false
          case Some(t: Term.Param) => !t.decltpe.exists(_.is[Type.Repeated])
          case _ => true
        }
    }

  def rightIsCloseDelimToAddTrailingComma(left: T, ft: => FormatToken)(implicit
      style: ScalafmtConfig
  ): Boolean =
    style.getTrailingCommas match {
      case TrailingCommas.keep =>
        left.is[T.Comma] &&
        TreeOps.rightIsCloseDelimForTrailingComma(left, ft)
      case TrailingCommas.always =>
        TreeOps.rightIsCloseDelimForTrailingComma(left, ft)
      case TrailingCommas.multiple =>
        val ftEval = ft
        TreeOps.rightIsCloseDelimForTrailingComma(left, ftEval) &&
        isCloseDelimForTrailingCommasMultiple(ftEval)
      case _ => false
    }

  def getMustDangleForTrailingCommas(close: T)(implicit
      style: ScalafmtConfig
  ): Boolean =
    getMustDangleForTrailingCommas(tokens.justBefore(close))

  def getMustDangleForTrailingCommas(getCloseFt: => FormatToken)(implicit
      style: ScalafmtConfig
  ): Boolean =
    !style.rewrite.trailingCommas.allowFolding && {
      val closeFt = getCloseFt
      val beforeClose =
        if (!closeFt.left.is[T.Comment]) Some(closeFt.left)
        else {
          val tok = tokens.prevNonCommentSameLine(prev(closeFt)).left
          if (tok.is[T.Comment]) None else Some(tok)
        }
      beforeClose.exists(rightIsCloseDelimToAddTrailingComma(_, closeFt))
    }

}

object FormatOps {
  class SelectLike(val tree: Term, val qual: Term, val nameToken: T)

  object SelectLike {
    def apply(tree: Term.Select): SelectLike =
      new SelectLike(tree, tree.qual, tree.name.tokens.head)
    def apply(tree: Term.Match, kw: T.KwMatch): SelectLike =
      new SelectLike(tree, tree.expr, kw)
  }

  case class TemplateSupertypeGroup(
      superType: Tree,
      superTypeGroup: Seq[Tree],
      expireTokenFunc: Seq[Tree] => T
  ) {
    def getExpireToken = expireTokenFunc(superTypeGroup)
    def isSecond = superTypeGroup.drop(1).headOption.contains(superType)
  }

  def nextLine(implicit fl: FileLine): FileLine = {
    val line = fl.line
    new FileLine(fl.file, line.copy(value = line.value + 1))
  }

  abstract class OptionalBracesRegion {
    def owner: Option[Tree]
    def splits: Option[Seq[Split]]
    def rightBrace: Option[T]
  }

  def getOpenParenAlignIndents(
      end: T
  )(implicit style: ScalafmtConfig): Seq[Indent] =
    if (style.align.closeParenSite)
      Seq(
        Indent(Length.StateColumn, end, ExpiresOn.After),
        Indent(Length.Num(1), end, ExpiresOn.Before),
        Indent(Length.Num(-1), end, ExpiresOn.After)
      )
    else
      Seq(Indent(Length.StateColumn, end, ExpiresOn.Before))

}
