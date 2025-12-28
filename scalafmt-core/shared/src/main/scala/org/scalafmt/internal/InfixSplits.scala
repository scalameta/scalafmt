package org.scalafmt.internal

import org.scalafmt.config.{IndentOperator, Newlines, ScalafmtConfig}
import org.scalafmt.util.InfixApp._
import org.scalafmt.util.TreeOps._
import org.scalafmt.util.{PolicyOps, TokenOps, TreeOps}

import scala.meta._
import scala.meta.tokens.{Token => T}

import scala.annotation.tailrec
import scala.collection.mutable

object InfixSplits {

  def apply(app: Member.Infix, ft: FT)(implicit
      style: ScalafmtConfig,
      ftoks: FormatTokens,
  ): InfixSplits = apply(app, ft, findEnclosingInfix(app))

  def apply(app: Member.Infix, ft: FT, fullInfix: Member.Infix)(implicit
      style: ScalafmtConfig,
      ftoks: FormatTokens,
  ): InfixSplits = {
    val leftInfix = findLeftInfix(fullInfix)
    new InfixSplits(app, ft, fullInfix, leftInfix)
  }

  private def switch(splits: Seq[Split], triggers: T*): Seq[Split] = splits.map(
    x => triggers.foldLeft(x) { case (y, trigger) => y.switch(trigger, false) },
  )

  @tailrec
  private def findMaybeEnclosingInfix(child: Member.Infix, childTree: Tree)(
      implicit ftoks: FormatTokens,
  ): (Member.Infix, Option[Either[FT, FT]]) = {
    val inParensOrBraces = ftoks.getClosingIfWithinParensOrBraces(childTree)
    if (inParensOrBraces.isDefined) (child, inParensOrBraces)
    else childTree.parent match {
      case Some(p: Member.Infix) if !p.isAssignment =>
        findMaybeEnclosingInfix(p, p)
      case Some(p @ Member.ArgClause(_ :: Nil)) =>
        findMaybeEnclosingInfix(child, p)
      case Some(p @ Tree.Block(`childTree` :: Nil)) =>
        findMaybeEnclosingInfix(child, p)
      case _ => (child, None)
    }
  }

  def findMaybeEnclosingInfix(app: Member.Infix)(implicit
      ftoks: FormatTokens,
  ): (Member.Infix, Option[Either[FT, FT]]) = findMaybeEnclosingInfix(app, app)

  private def findEnclosingInfix(app: Member.Infix)(implicit
      ftoks: FormatTokens,
  ): Member.Infix = findMaybeEnclosingInfix(app)._1

  def withNLIndent(split: Split)(
      app: Member.Infix,
  )(implicit ft: FT, style: ScalafmtConfig, ftoks: FormatTokens): Split =
    withNLIndent(split, app, findEnclosingInfix(app))

  def withNLIndent(
      split: Split,
      app: Member.Infix,
      fullInfix: => Member.Infix,
  )(implicit ft: FT, style: ScalafmtConfig, ftoks: FormatTokens): Split = {
    val noNL = !split.isNL && {
      val nextFt = ftoks.nextNonCommentSameLine(ft)
      nextFt.eq(ft) || nextFt.noBreak
    }
    if (noNL) split else apply(app, ft, fullInfix).withNLIndent(split)
  }

  private def getInfixRhsPossiblyEnclosed(app: Member.Infix)(implicit
      ftoks: FormatTokens,
  ): Option[Either[Member.Infix, Member.Infix]] = app.singleArg
    .map(TreeOps.getBlockStat).flatMap {
      case t: Member.Infix =>
        Some(Either.cond(ftoks.isEnclosedWithinParens(t), t, t))
      case _ => None
    }

  @tailrec
  private def findRightmostArgIfEnclosedInfix(app: Member.Infix)(implicit
      ftoks: FormatTokens,
  ): Option[Member.Infix] = getInfixRhsPossiblyEnclosed(app) match {
    case Some(Left(ia)) => findRightmostArgIfEnclosedInfix(ia)
    case Some(Right(ia)) => Some(ia)
    case _ => None
  }

  private def infixSequenceMaxPrecedence(
      app: Member.Infix,
  )(implicit ftoks: FormatTokens): Int = {
    val queue = new mutable.Queue[Member.Infix]()
    queue += app
    var maxPrecedence = 0
    while (queue.nonEmpty) {
      val elem = queue.dequeue()
      val elemPrecedence = elem.precedence
      if (maxPrecedence < elemPrecedence) maxPrecedence = elemPrecedence
      queue ++= elem.nestedInfixApps.filter(x => !ftoks.isEnclosedWithinParens(x))
    }
    maxPrecedence
  }

  def getSingleLineInfixPolicy(end: FT) = Policy
    .onLeft(end, prefix = "INFSLB", terminal = true) {
      case Decision(t: FT, s) if isInfixOp(t.meta.leftOwner) =>
        SplitTag.InfixChainNoNL.activateOnly(s)
    }

  private def getMidInfixToken(
      app: Member.Infix,
  )(implicit ftoks: FormatTokens): FT = {
    val opToken = ftoks.getHead(app.op)
    val beforeOp = ftoks.prev(opToken)
    val lhsLast = ftoks.prevNonComment(beforeOp)
    if (beforeOp eq lhsLast) opToken else lhsLast
  }

  @tailrec
  private def findNextInfixes(fullTree: Tree, tree: Tree)(implicit
      ftoks: FormatTokens,
      res: mutable.Buffer[Member.Infix],
  ): Unit = if (tree ne fullTree) tree.parent match {
    case Some(ia: Member.Infix) =>
      val ok = (ia.lhs ne tree) || {
        res += ia
        findNestedInfixes(ia.arg)
      }
      if (ok) findNextInfixes(fullTree, ia)
    case Some(p: Member.ArgClause) => p.parent match {
        case Some(pp: Member.Infix) => findNextInfixes(fullTree, pp)
        case _ =>
      }
    case Some(p @ Tree.Block(`tree` :: Nil)) if !ftoks.isEnclosedInBraces(p) =>
      findNextInfixes(fullTree, p)
    case _ =>
  }

  private def findNestedInfixes(
      tree: Tree,
  )(implicit ftoks: FormatTokens, res: mutable.Buffer[Member.Infix]): Boolean =
    TreeOps.getBlockStat(tree) match {
      case Member.ArgClause(arg :: Nil)
          if !ftoks.isEnclosedWithinParensOrBraces(tree) =>
        findNestedInfixes(arg)
      case ia: Member.Infix if !ftoks.isEnclosedWithinParens(tree) =>
        findNestedInfixes(ia.lhs) && {
          res += ia
          ia.singleArg match {
            case None => true
            case Some(arg) => findNestedInfixes(arg)
          }
        }
      case _ => true
    }

  @tailrec
  final def findLeftInfix(app: Member.Infix)(implicit
      ftoks: FormatTokens,
  ): Member.Infix = TreeOps.getBlockStat(app.lhs) match {
    case ia: Member.Infix if !ftoks.isEnclosedWithinParens(ia) =>
      findLeftInfix(ia)
    case _ => app
  }

  private def getInfixSplitsBeforeLhs(
      lhsApp: Member.Infix,
      afterInfix: Newlines.Infix.Site,
      newStmtMod: Option[Modification],
  )(implicit style: ScalafmtConfig, ft: FT, ftoks: FormatTokens): Seq[Split] = {
    val fullInfix = findTreeWithParentSimple(lhsApp, false)(isInfixApp)
      .flatMap(asInfixApp).getOrElse(lhsApp)
    val app = findLeftInfix(fullInfix)
    new InfixSplits(app, ft, fullInfix, app)
      .getBeforeLhsOrRhs(afterInfix, newStmtMod)
  }

  final def maybeGetInfixSplitsBeforeLhs(mod: => Option[Modification] = None)(
      nonInfixSplits: => Seq[Split],
  )(implicit style: ScalafmtConfig, ft: FT, ftoks: FormatTokens): Seq[Split] =
    asInfixApp(ft.meta.rightOwner).fold(nonInfixSplits) { ia =>
      val infixSite = style.newlines.infix.get(ia)
      if (infixSite.isKeep) nonInfixSplits
      else getInfixSplitsBeforeLhs(ia, infixSite, mod)
    }

}

class InfixSplits(
    app: Member.Infix,
    ft: FT,
    fullInfix: Member.Infix,
    leftInfix: Member.Infix,
)(implicit style: ScalafmtConfig, ftoks: FormatTokens) {
  private val isLeftInfix = leftInfix eq app
  private val isAfterOp = ft.meta.leftOwner eq app.op
  private val beforeLhs = !isAfterOp && ft.left.start < app.pos.start
  private val isFirstOp = beforeLhs || isLeftInfix && isAfterOp
  private val fullExpire = ftoks.getLastExceptParen(fullInfix)

  private val assignBodyExpire = {
    val prevFt = ftoks.tokenBefore(fullInfix)
    val prevOwner = prevFt.meta.leftOwner
    prevFt.left match {
      case _: T.Equals => Some(ftoks.getLast(prevOwner))
      case _: T.LeftParen | _: T.LeftBracket
          if fullInfix.parent.contains(prevOwner) && !(prevOwner match {
            case po: Member.ArgClause => po.parent.exists(isInfixApp)
            case po => isInfixApp(po)
          }) && isSeqSingle(getArgsOrNil(prevOwner)) =>
        Some(ftoks.getLast(fullInfix))
      case _ => None
    }
  }

  private val skipInfixIndent: Boolean = {
    @tailrec
    def getLastPat(t: Tree): Tree = t.parent match {
      case Some(p @ (_: Pat | _: Pat.ArgClause)) => getLastPat(p)
      case _ => t
    }
    def getChild = fullInfix match {
      case t: Pat => getLastPat(t)
      case t => t
    }
    @tailrec
    def isOldTopLevelWithParent(child: Tree)(p: Tree): Boolean = p match {
      case _: Term.If | _: Term.While | _: Source => true
      case Term.Block(_ :: rest) => rest.nonEmpty ||
        (p.parent match {
          case Some(pp) => p.tokens.head match { // check brace was not rewritten
              case head: T.LeftBrace => (ftoks.before(head).left eq head) ||
                isOldTopLevelWithParent(p)(pp)
              case _ => true
            }
          case None => true
        })
      case fun: Member.Function => isBlockFunction(fun)
      case t: Case => t.pat.eq(child) || t.body.eq(child)
      case SingleArgInBraces(_, arg, _) => child eq arg
      case _ => false
    }
    def isOldTopLevel(child: Tree) = child.parent
      .exists(isOldTopLevelWithParent(child))
    @tailrec
    def isAloneEnclosed(child: Tree): Boolean = child.parent.orNull match {
      case p: Case => p.pat eq child
      case p: Term.If => p.cond eq child
      case p: Term.While => p.expr eq child
      case p: Term.Do => p.expr eq child
      case p: Term.Block => hasSingleElement(p, child) &&
        (p.tokens.head match {
          case head: T.LeftBrace => // check brace was not rewritten
            (ftoks.before(head).left eq head) || isAloneEnclosed(p)
          case _ => true
        })
      case p: Member.Function => isBlockFunction(p)
      case p @ Member.ArgClause(`child` :: Nil) => ftoks.isEnclosedInMatching(p)
      case Member.Tuple(`child` :: Nil) => true
      case _ => false
    }
    @tailrec
    def isAloneArgOrBody(child: Tree): Boolean = child.parent.orNull match {
      case t: Case => t.pat.eq(child) || t.body.eq(child)
      case _: Term.If | _: Term.While | _: Term.Do => true
      case _: Member.ArgClause => true
      case p: Term.Block => hasSingleElement(p, child) &&
        (p.tokens.head match {
          case head: T.LeftBrace => // check brace was not rewritten
            (ftoks.before(head).left eq head) || isAloneArgOrBody(p)
          case _ => true
        })
      case _: Init | _: Term.Super | _: Member.Tuple => true
      case t: Tree.WithBody => t.body eq child
      case t: Term.Param => t.default.contains(child)
      case _ => false
    }
    val cfg = style.indent.infix
    def allowNoIndent = cfg.exemptScope match {
      case IndentOperator.Exempt.all => true
      case IndentOperator.Exempt.oldTopLevel => isOldTopLevel(getChild)
      case IndentOperator.Exempt.aloneEnclosed => isAloneEnclosed(getChild)
      case IndentOperator.Exempt.aloneArgOrBody => isAloneArgOrBody(getChild)
      case IndentOperator.Exempt.notAssign => isAfterAssignmentOp(false)
      case IndentOperator.Exempt.notWithinAssign => !app.isAssignment &&
        // fullInfix itself is never an assignment
        fullInfix.parent.exists {
          case _: Member.Infix => false
          case p: Member.ArgClause => !p.parent.is[Member.Infix]
          case _ => true
        }
    }
    if (beforeLhs) assignBodyExpire.isEmpty
    else app.is[Pat] || allowNoIndent && cfg.noindent(app.op.value)
  }

  private val fullIndent: Indent = assignBodyExpire match {
    case Some(x) if beforeLhs => Indent(style.indent.main, x, ExpiresOn.After)
    case None if isLeftInfix && isAfterAssignmentOp(true) =>
      Indent(style.indent.main, fullExpire, ExpiresOn.After)
    case _ =>
      val len = style.indent.getAfterInfixSite
      Indent(len, fullExpire, ExpiresOn.After)
  }

  val (nlIndent, nlPolicy) = {
    def policy(triggers: T*) = Policy ? triggers.isEmpty ||
      Policy.onLeft(fullExpire, prefix = "INF") {
        case Decision(FT(_: T.Ident, _, m), s) if isInfixOp(m.leftOwner) =>
          InfixSplits.switch(s, triggers: _*)
        case Decision(FT(_, _: T.Ident, m), s)
            if AsInfixOp(m.rightOwner).exists(style.newlines.infix.keep) =>
          InfixSplits.switch(s, triggers: _*)
        case Decision(xft @ FT(_, _: T.Comment, _), s)
            if AsInfixOp(ftoks.nextNonCommentAfter(xft).rightOwner)
              .exists(style.newlines.infix.keep) =>
          InfixSplits.switch(s, triggers: _*)
      }

    val fullTok = TokenOps.getIndentTrigger(fullInfix)
    val noAssign = assignBodyExpire.isEmpty
    if (!noAssign && beforeLhs) (fullIndent, policy(fullTok))
    else if (skipInfixIndent)
      if (noAssign) (Indent.Empty, Policy.NoPolicy)
      else (Indent.before(fullIndent, fullTok), policy(fullTok))
    else {
      val opTok = TokenOps.getIndentTrigger(leftInfix.op)
      val ind = if (isFirstOp) fullIndent else Indent.before(fullIndent, opTok)
      if (noAssign) (ind, policy(opTok))
      else (Indent.Switch(fullIndent, fullTok, ind), policy(fullTok, opTok))
    }
  }

  @inline
  private def isAfterAssignmentOp(isAssignment: Boolean): Boolean = isAfterOp &&
    app.isAssignment == isAssignment

  private def withNLIndent(split: Split): Split = split.withIndent(nlIndent)
    .andPolicy(nlPolicy)

  def getBeforeLhsOrRhs(
      afterInfix: Newlines.Infix.Site,
      newStmtMod: Option[Modification] = None,
      spaceMod: Modification = Space,
  ): Seq[Split] = {
    val (maxPrecedence, appPrecedence, breakPenalty) =
      if (isAfterOp) {
        val maxPrecedence = InfixSplits.infixSequenceMaxPrecedence(fullInfix)
        val appPrecedence = app.precedence
        val breakPenalty = maxPrecedence - appPrecedence
        (maxPrecedence, appPrecedence, breakPenalty)
      } else (0, Int.MaxValue, 1)

    val closeOpt = ftoks.matchingOptRight(ft)
    val finalExpireCost = fullExpire -> 0
    val expires =
      if (closeOpt.isDefined) finalExpireCost :: Nil
      else {
        implicit val res = mutable.Buffer.empty[Member.Infix]
        InfixSplits.findNextInfixes(fullInfix, app.lhs)
        val infixes = if (isAfterOp) res.toSeq.drop(1) else res.toSeq
        if (infixes.isEmpty) finalExpireCost :: Nil
        else {
          val out = new mutable.ListBuffer[(FT, Int)]
          var minCost = Int.MaxValue
          infixes.foreach { ia =>
            val cost = maxPrecedence - ia.precedence
            if (cost < minCost) {
              out += InfixSplits.getMidInfixToken(ia) -> cost
              minCost = cost
            }
          }
          if (0 < minCost) out += finalExpireCost
          out.toList
        }
      }

    val infixTooLong = infixSequenceLength(fullInfix) >
      afterInfix.maxCountPerExprForSome
    val breakMany = infixTooLong || (afterInfix.style eq Newlines.Infix.many)
    val rightAsInfix = asInfixApp(ft.meta.rightOwner)

    def breakAfterComment(t: FT) = {
      val end = ftoks.nextNonCommentSameLine(t)
      Policy ? end.right.isAny[T.LeftBrace, T.Comment] || {
        if (end eq t) PolicyOps.decideNewlinesOnlyAfterToken(end)
        else PolicyOps.decideNewlinesOnlyAfterClose(end)
      }
    }
    val nlMod = newStmtMod
      .getOrElse(Space.orNL(ft.noBreak && ft.right.is[T.Comment]))
    val delayedBreak = Policy ? nlMod.isNL || breakAfterComment(ft)

    val (singleLineExpire, singleLineIndent) = {
      val skip = skipInfixIndent
      if (isFirstOp) (fullExpire, if (skip) Indent.Empty else fullIndent)
      else {
        val expire = expires.head._1
        val indentLen = if (skip) 0 else style.indent.main
        val indent = Indent(indentLen, expire, ExpiresOn.After)
        (expire, indent)
      }
    }

    val singleLinePolicy = Policy ? (infixTooLong || !isFirstOp) ||
      InfixSplits.getSingleLineInfixPolicy(fullExpire)
    val nlSinglelineSplit = Split(nlMod, 0)
      .onlyIf(singleLinePolicy.nonEmpty && !isAfterOp)
      .withIndent(singleLineIndent).withSingleLine(singleLineExpire)
      .andPolicy(singleLinePolicy).andPolicy(delayedBreak)
    val spaceSingleLine = Split(spaceMod, 0).onlyIf(newStmtMod.isEmpty)
      .withSingleLine(singleLineExpire).andPolicy(singleLinePolicy)
    val singleLineSplits = Seq(
      spaceSingleLine.onlyFor(SplitTag.InfixChainNoNL),
      spaceSingleLine.onlyIf(singleLinePolicy.nonEmpty),
      nlSinglelineSplit,
    )

    val rhsPossiblyEnclosedInfix =
      if (closeOpt.isEmpty) None
      else InfixSplits.getInfixRhsPossiblyEnclosed(app)

    // returns end of next LHS, really
    def getNextOp: Option[FT] = (rhsPossiblyEnclosedInfix match {
      case Some(Left(ia)) => Some(InfixSplits.findLeftInfix(ia))
      case _ if app eq fullInfix => None
      case _ => findNextInfixInParent(app, fullInfix)
    }).map(ia => ftoks.tokenBefore(ia.op))

    val isAfterOpBreakOnNested = isAfterOp && afterInfix.breakOnNested
    def mustBreakAfterNested = isAfterOpBreakOnNested &&
      (app.lhs match {
        case ia: Member.Infix =>
          val iaOpt =
            if (isLeftInfix) Some(ia)
            else InfixSplits.findRightmostArgIfEnclosedInfix(ia)
          iaOpt.exists(_.precedence <= appPrecedence)
        case _ => false
      })

    def otherSplitsNoDelims = {
      val nlSplit = Split(nlMod, 1 + breakPenalty).withIndent(nlIndent)
        .withPolicy(nlPolicy & delayedBreak)
      val spaceSplits: Seq[Split] =
        if (ft.right.is[T.Comment] || mustBreakAfterNested) Seq.empty
        else {
          val nextFT = if (rightAsInfix.isDefined) ftoks.next(ft) else ft
          expires.filter(_._2 <= breakPenalty).takeRight(3)
            .map { case (expire, cost) =>
              val exclude =
                if (breakMany) TokenRanges.empty
                else TokenOps.insideBracesBlock(nextFT, expire, true)
              val ignore = exclude.isEmpty && singleLinePolicy.nonEmpty &&
                (expire eq fullExpire)
              Split(ignore, cost)(ModExt(newStmtMod.getOrElse(spaceMod)))
                .withSingleLine(expire, exclude, noOptimal = cost != 0)
            }
        }
      spaceSplits :+ nlSplit
    }

    def otherSplitsWithParens(closeFt: FT) = {
      val noSingleLine = newStmtMod.isDefined || breakMany ||
        isAfterOpBreakOnNested && rhsPossiblyEnclosedInfix.exists {
          case Right(ia) => ia.precedence <= appPrecedence
          case _ => false
        } || mustBreakAfterNested ||
        rightAsInfix.exists(10 < infixSequenceLength(_))
      val endOfNextOp = if (afterInfix.breakOnNested) getNextOp else None

      val nlSplit = Split(nlMod, 0, policy = nlPolicy).withIndent(nlIndent)
      val singleLineSplit = Split(noSingleLine, 0)(spaceMod)
        .withSingleLine(endOfNextOp.getOrElse(closeFt))
        .andPolicy(InfixSplits.getSingleLineInfixPolicy(closeFt))
      Seq(singleLineSplit, nlSplit)
    }

    def otherSplitsWithBraces(closeFt: FT) = {
      val slbEnd = getNextOp.getOrElse(fullExpire)
      val slbPolicy = InfixSplits.getSingleLineInfixPolicy(closeFt)
      // check if enclosed
      if (slbEnd eq closeFt) Seq(
        Split(spaceMod, 0),
        Split(nlMod, 1).withSingleLineNoOptimal(slbEnd)
          .andPolicy(nlPolicy & slbPolicy).withIndent(nlIndent),
      )
      else Seq(
        Split(spaceMod, 0).withSingleLine(slbEnd).andPolicy(slbPolicy),
        Split(nlMod, 0, policy = nlPolicy).withIndent(nlIndent),
      )
    }

    val otherSplits = closeOpt.fold(otherSplitsNoDelims)(closeFt =>
      if (closeFt.left.is[T.RightBrace]) otherSplitsWithBraces(closeFt)
      else otherSplitsWithParens(closeFt),
    )

    singleLineSplits ++ otherSplits
  }

}
