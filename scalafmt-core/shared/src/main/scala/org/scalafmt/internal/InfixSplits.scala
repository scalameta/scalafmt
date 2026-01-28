package org.scalafmt.internal

import org.scalafmt.config.{IndentOperator, Newlines, ScalafmtConfig}
import org.scalafmt.util.InfixApp._
import org.scalafmt.util.TreeOps._
import org.scalafmt.util.{PolicyOps, TokenOps, TreeOps}

import org.scalameta.FileLine
import scala.meta._
import scala.meta.tokens.{Token => T}

import scala.annotation.tailrec
import scala.collection.mutable

object InfixSplits {

  def apply(app: Member.Infix, ft: FT)(implicit
      style: ScalafmtConfig,
      ftoks: FormatTokens,
  ): InfixSplits = apply(app, ft, findMaybeEnclosingInfix(app))

  def apply(
      app: Member.Infix,
      ft: FT,
      fullInfixWithEnclosedIn: => (Member.Infix, Option[Either[FT, FT]]),
  )(implicit style: ScalafmtConfig, ftoks: FormatTokens): InfixSplits = {
    val (fullInfix, fullInfixEnclosed) = findMaybeEnclosingInfix(app)
    apply(app, ft, fullInfix, fullInfixEnclosed)
  }

  def apply(
      app: Member.Infix,
      ft: FT,
      fullInfix: Member.Infix,
      fullInfixEnclosedIn: Option[Either[FT, FT]],
  )(implicit style: ScalafmtConfig, ftoks: FormatTokens): InfixSplits = {
    val leftInfix = findLeftInfix(fullInfix)
    new InfixSplits(app, ft, fullInfix, leftInfix, fullInfixEnclosedIn)
  }

  private def switch(splits: Seq[Split], triggers: T*): Seq[Split] = splits
    .map(triggers.foldLeft(_)(_.switch(_, on = false)))

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

  def withNLIndent(split: Split)(
      app: Member.Infix,
  )(implicit ft: FT, style: ScalafmtConfig, ftoks: FormatTokens): Split =
    withNLIndent(split, app, findMaybeEnclosingInfix(app))

  def withNLIndent(
      split: Split,
      app: Member.Infix,
      fullInfixWithEnclosedIn: => (Member.Infix, Option[Either[FT, FT]]),
  )(implicit ft: FT, style: ScalafmtConfig, ftoks: FormatTokens): Split = {
    val noNL = !split.isNL && {
      val nextFt = ftoks.nextNonCommentSameLine(ft)
      nextFt.eq(ft) || nextFt.noBreak
    }
    if (noNL) split
    else apply(app, ft, fullInfixWithEnclosedIn).withNLIndent(split)
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

  def getSingleLineInfixPolicy(end: FT) = Policy
    .onLeft(end, prefix = "INFSLB", terminal = true) {
      case Decision(t: FT, s) if isInfixOp(t.meta.leftOwner) =>
        SplitTag.InfixChainNoNL.activateOnly(s)
    }

  private def getMidInfixToken(app: Member.Infix, isKeep: Boolean)(implicit
      ftoks: FormatTokens,
  ): FT = {
    val opToken = ftoks.getHead(app.op)
    val beforeOp = ftoks.prev(opToken)
    val lhsLast = ftoks.prevNonComment(beforeOp)
    val mid = if (isKeep || (beforeOp ne lhsLast)) lhsLast else opToken
    ftoks.nextNonCommentSameLine(mid)
  }

  @tailrec
  private def findNextInfixes(fullTree: Tree, tree: Tree)(implicit
      ftoks: FormatTokens,
      res: mutable.Buffer[Member.Infix],
  ): Unit = if (tree ne fullTree) tree.parent match {
    case Some(ia: Member.Infix) =>
      if (ia.lhs eq tree) {
        res += ia
        findNestedInfixes(ia.arg)
      }
      findNextInfixes(fullTree, ia)
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
  )(implicit ftoks: FormatTokens, res: mutable.Buffer[Member.Infix]): Unit =
    TreeOps.getBlockStat(tree) match {
      case Member.ArgClause(arg :: Nil)
          if !ftoks.isEnclosedWithinParensOrBraces(tree) =>
        findNestedInfixes(arg)
      case ia: Member.Infix if !ftoks.isEnclosedWithinParens(tree) =>
        findNestedInfixes(ia.lhs)
        res += ia
        ia.singleArg match {
          case Some(arg) => findNestedInfixes(arg)
          case _ =>
        }
      case _ =>
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
    new InfixSplits(app, ft, fullInfix, app, fullInfixEnclosedIn = None)
      .getBeforeLhsOrRhs(afterInfix, newStmtMod)
  }

  final def maybeGetInfixSplitsBeforeLhs(mod: => Option[Modification] = None)(
      nonInfixSplits: => Seq[Split],
  )(implicit style: ScalafmtConfig, ft: FT, ftoks: FormatTokens): Seq[Split] =
    asInfixApp(ft.meta.rightOwner).fold(nonInfixSplits) { ia =>
      val infixSite = style.newlines.infix.get(ia)
      if (infixSite.isNone) nonInfixSplits
      else getInfixSplitsBeforeLhs(ia, infixSite, mod)
    }

}

class InfixSplits(
    app: Member.Infix,
    ft: FT,
    fullInfix: Member.Infix,
    leftInfix: Member.Infix,
    fullInfixEnclosedIn: Option[Either[FT, FT]],
)(implicit style: ScalafmtConfig, ftoks: FormatTokens) {
  private val isLeftInfix = leftInfix eq app
  private val isAfterOp = ft.meta.leftOwner eq app.op
  private val appPrecedence = app.precedence
  private val beforeLhs = !isAfterOp && ft.left.start < app.pos.start
  private val isFirstOp = beforeLhs || isLeftInfix && isAfterOp
  private val fullExpire = ftoks
    .nextNonCommentSameLine(ftoks.getLastExceptParen(fullInfix))

  private val assignBodyExpire = {
    val prevFt = ftoks.tokenBefore(fullInfix)
    val prevOwner = prevFt.meta.leftOwner
    prevFt.left match {
      case _: T.Equals => Some(ftoks.getLast(prevOwner))
      case _: T.LeftParen | _: T.LeftBracket
          if fullInfix.parent.contains(prevOwner) &&
            !(prevOwner match {
              case po: Member.ArgClause => po.parent.exists(isInfixApp)
              case po => isInfixApp(po)
            }) && isSeqSingle(getArgsOrNil(prevOwner)) =>
        Some(ftoks.getLast(fullInfix))
      case _ => None
    }
  }

  @tailrec
  private def isOldTopLevelWithParent(tree: Tree)(p: Tree): Boolean = p match {
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
    case p: Member.Function => isBlockFunction(p)
    case p: Case => p.pat.eq(tree) || p.body.eq(tree)
    case SingleArgInBraces(_, arg, _) => tree eq arg
    case _ => false
  }
  private def isOldTopLevel(tree: Tree) = tree.parent
    .exists(isOldTopLevelWithParent(tree))
  @tailrec
  private def isAloneEnclosed(tree: Tree): Boolean = tree.parent.orNull match {
    case p: Case => p.pat eq tree
    case p: Term.If => p.cond eq tree
    case p: Term.While => p.expr eq tree
    case p: Term.Do => p.expr eq tree
    case p: Term.Block => hasSingleElement(p, tree) &&
      (p.tokens.head match {
        case head: T.LeftBrace => // check brace was not rewritten
          (ftoks.before(head).left eq head) || isAloneEnclosed(p)
        case _ => true
      })
    case p: Member.Function => isBlockFunction(p)
    case p @ Member.ArgClause(`tree` :: Nil) => ftoks.isEnclosedInMatching(p)
    case Member.Tuple(`tree` :: Nil) => true
    case _ => false
  }
  @tailrec
  private def isAloneArgOrBody(tree: Tree): Boolean = tree.parent.orNull match {
    case p: Case => p.pat.eq(tree) || p.body.eq(tree)
    case _: Term.If | _: Term.While | _: Term.Do => true
    case _: Member.ArgClause => true
    case p: Term.Block => hasSingleElement(p, tree) &&
      (p.tokens.head match {
        case head: T.LeftBrace => // check brace was not rewritten
          (ftoks.before(head).left eq head) || isAloneArgOrBody(p)
        case _ => true
      })
    case _: Init | _: Term.Super | _: Member.Tuple => true
    case p: Tree.WithBody => p.body eq tree
    case p: Term.Param => p.default.contains(tree)
    case _ => false
  }
  @tailrec
  private def getFullPat(t: Tree): Tree = t.parent match {
    case Some(p @ (_: Pat | _: Pat.ArgClause)) => getFullPat(p)
    case _ => t
  }

  private val skipInfixIndent: Boolean = {
    import IndentOperator.Exempt
    lazy val full = fullInfix match {
      case t: Pat => getFullPat(t)
      case t => t
    }
    def allowNoIndent(cfg: IndentOperator) = {
      if (cfg.exemptScope.isEmpty) Seq(Exempt.oldTopLevel) else cfg.exemptScope
    }.forall {
      case Exempt.all => true
      case Exempt.oldTopLevel => isOldTopLevel(full)
      case Exempt.aloneEnclosed => isAloneEnclosed(full)
      case Exempt.aloneArgOrBody => isAloneArgOrBody(full)
      case Exempt.notAssign => isAfterOp && !isAssignmentOp
      case Exempt.notWithinAssign => !isAssignmentOp &&
        // fullInfix itself is never an assignment
        fullInfix.parent.exists {
          case _: Member.Infix => false
          case p: Member.ArgClause => !p.parent.is[Member.Infix]
          case _ => true
        }
    } && cfg.noindent(app.op.value)
    if (beforeLhs) assignBodyExpire.isEmpty
    else app.is[Pat] || style.indent.infix.exists(allowNoIndent)
  }

  private val (fullIndentLength, fullIndentExpire) = assignBodyExpire match {
    case Some(x) if beforeLhs => (style.indent.main, x)
    case None if isFirstOp && isAssignmentOp => (style.indent.main, fullExpire)
    case _ => (style.indent.getAfterInfixSite, fullExpire)
  }

  private val fullIndent: Indent =
    Indent(fullIndentLength, fullIndentExpire, ExpiresOn.After)

  val (nlIndent, nlPolicy) = {
    def policy(triggers: T*)(implicit fl: FileLine) =
      Policy ? triggers.isEmpty || Policy.onLeft(fullExpire, prefix = "INF") {
        case Decision(FT(_: T.Ident, _, m), s) if isInfixOp(m.leftOwner) =>
          InfixSplits.switch(s, triggers: _*)
        case Decision(xft @ FT(_, _: T.Ident, m), s)
            if !AsInfixOp(m.rightOwner)
              .forall(style.newlines.infix.sourceIgnoredAt(xft)) =>
          InfixSplits.switch(s, triggers: _*)
        case Decision(xft @ FT(_, _: T.Comment, _), s) if {
              val nft = ftoks.nextNonCommentAfter(xft)
              !AsInfixOp(nft.rightOwner)
                .forall(style.newlines.infix.sourceIgnoredAt(nft))
            } => InfixSplits.switch(s, triggers: _*)
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
  private def isAssignmentOp: Boolean = appPrecedence == 0

  private def withNLIndent(split: Split): Split = split.withIndent(nlIndent)
    .andPolicy(nlPolicy)

  def getBeforeLhsOrRhs(
      afterInfix: Newlines.Infix.Site,
      newStmtMod: Option[Modification] = None,
      spaceMod: Modification = Space,
  ): Seq[Split] = {
    def getOpenClose(xft: FT): Option[(FT, FT)] = // exclude Xml.Start and similar pairs
      if (xft.right.is[T.OpenDelim]) ftoks.matchingOptRight(xft).map(xft -> _)
      else None
    val openCloseOpt = // don't need to worry about comments, they force NL
      if (!ft.right.is[T.Ident]) getOpenClose(ft)
      else if (!ft.rightOwner.parent.is[Term.ApplyUnary]) None
      else getOpenClose(ftoks.next(ft))
    val nextInfixes =
      if (openCloseOpt.isEmpty) {
        implicit val infixes = new mutable.ListBuffer[Member.Infix]
        if (isAfterOp) {
          InfixSplits.findNestedInfixes(app.arg)
          InfixSplits.findNextInfixes(fullInfix, app)
        } else InfixSplits.findNextInfixes(fullInfix, app.lhs)
        infixes.toList
      } else Nil

    val fullPrecedence = fullInfix.precedence
    val fullExpirePrecedence = fullExpire -> fullPrecedence
    val (firstExpire, expires, minPrecedence) =
      if (nextInfixes.isEmpty)
        (fullExpire, fullExpirePrecedence :: Nil, fullPrecedence)
      else {
        val isKeep = !afterInfix.style.sourceIgnored
        val anyPrecedence = isAssignmentOp ||
          isKeep && isAfterOp && ftoks.prev(ft).hasBreak
        var minPrecedence =
          if (anyPrecedence) Int.MaxValue else appPrecedence + 1
        var firstExpire: FT = null
        val out = new mutable.ListBuffer[(FT, Int)]
        def add(elem: (FT, Int)): Unit = {
          if (out.length == 3) out.remove(0)
          out += elem
        }
        @tailrec
        def iter(seq: List[Member.Infix]): Unit = seq match {
          case ia :: rest =>
            val precedence = ia.precedence
            val expire = InfixSplits.getMidInfixToken(ia, isKeep)
            if (firstExpire eq null) firstExpire = expire
            if (isKeep && (expire.hasBreak || ftoks.next(expire).hasBreak)) {
              if (minPrecedence > precedence) minPrecedence = precedence
              add(expire -> precedence)
            } else {
              if (minPrecedence > precedence) {
                minPrecedence = precedence
                add(expire -> precedence)
              }
              iter(rest)
            }
          case _ => if (minPrecedence > fullPrecedence) {
              minPrecedence = fullPrecedence
              add(fullExpirePrecedence)
            }
        }
        iter(nextInfixes)
        (firstExpire, out.toList, minPrecedence)
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

    val isFirstOrAssignOp = isFirstOp || isAssignmentOp
    val singleLineExpire = if (isFirstOrAssignOp) fullExpire else firstExpire
    def singleLineIndent =
      if (isFirstOrAssignOp) fullIndent
      else Indent(fullIndentLength, firstExpire, ExpiresOn.After)

    val singleLinePolicy = Policy ? (infixTooLong || !isFirstOrAssignOp) ||
      InfixSplits.getSingleLineInfixPolicy(fullExpire)
    val nlSinglelineSplit = Split(nlMod, 0)
      .notIf(singleLinePolicy.isEmpty || isAfterOp && !isAssignmentOp)
      .withIndent(singleLineIndent, ignore = skipInfixIndent).withSingleLine(
        singleLineExpire,
        ignorePenalty = isAssignmentOp && nextInfixes.nonEmpty,
      ).andPolicy(singleLinePolicy).andPolicy(delayedBreak)
    val spaceSingleLine = Split(spaceMod, 0).onlyIf(newStmtMod.isEmpty)
      .withSingleLine(singleLineExpire).andPolicy(singleLinePolicy)
    val singleLineSplits = Seq(
      spaceSingleLine.onlyFor(SplitTag.InfixChainNoNL),
      spaceSingleLine.notIf(singleLinePolicy.isEmpty),
      nlSinglelineSplit,
    )

    val rhsPossiblyEnclosedInfix = openCloseOpt.flatMap(delims =>
      if (beforeLhs) Some(Either.cond(delims._2.idx >= fullExpire.idx, app, app))
      else InfixSplits.getInfixRhsPossiblyEnclosed(app),
    )

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
          iaOpt.exists(_.precedence >= appPrecedence)
        case _ => false
      })

    def otherSplitsNoDelims = {
      val nlPrecedence = if (isAssignmentOp) expires.head._2 else appPrecedence
      val nlCost = if (nlMod.isNL) 1 + nlPrecedence - minPrecedence else 0
      val nlSplit = Split(nlMod, nlCost).withIndent(nlIndent)
        .withPolicy(nlPolicy & delayedBreak)
      val spaceSplits: Seq[Split] =
        if (ft.right.is[T.Comment] || mustBreakAfterNested) Seq.empty
        else {
          val nextFT = if (rightAsInfix.isDefined) ftoks.next(ft) else ft
          expires.map { case (expire, precedence) =>
            val cost = precedence - minPrecedence
            val exclude =
              if (breakMany) TokenRanges.empty
              else TokenOps.insideBracesBlock(nextFT, expire, parens = true)
            val ignore = exclude.isEmpty && singleLinePolicy.nonEmpty &&
              (expire eq fullExpire)
            Split(ignore, cost)(ModExt(newStmtMod.getOrElse(spaceMod)))
              .withSingleLine(expire, exclude, noOptimal = cost != 0)
          }
        }
      spaceSplits :+ nlSplit
    }

    def otherSplitsWithParens(openFt: FT, closeFt: FT) = {
      val nextFt = ftoks.nextNonCommentSameLineAfter(openFt)
      val bracesLike = newStmtMod.isEmpty &&
        (style.newlines.source match {
          case Newlines.fold => false
          case Newlines.unfold => !ft.hasBlankLine
          case _ => ft.noBreak && nextFt.hasBreak
        })

      val noSingleLine = newStmtMod.isDefined || breakMany ||
        isAfterOpBreakOnNested && rhsPossiblyEnclosedInfix.exists {
          case Right(ia) => ia.precedence >= appPrecedence
          case _ => false
        } || mustBreakAfterNested ||
        rightAsInfix.exists(10 < infixSequenceLength(_))
      val endOfNextOp = if (afterInfix.breakOnNested) getNextOp else None

      val slbPolicy = InfixSplits.getSingleLineInfixPolicy(closeFt)
      val nlSplit = Split(nlMod, 0, nlPolicy).withIndent(nlIndent)
        .andPolicy(slbPolicy | PolicyOps.SingleLineBlock(closeFt), !bracesLike)
        .withOptimalToken(closeFt, killOnFail = false, ignore = !bracesLike)
      val singleLineSplit = Split(noSingleLine, 0)(spaceMod)
        .withSingleLine(endOfNextOp.getOrElse(closeFt)).andPolicy(slbPolicy)
      val noSplit = Split(!bracesLike, 1)(spaceMod)
        .withPolicy(PolicyOps.decideNewlinesOnlyAfterClose(nextFt))
      Seq(singleLineSplit, nlSplit, noSplit)
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
        Split(nlMod, 0, nlPolicy).withIndent(nlIndent),
      )
    }

    val otherSplits = openCloseOpt.fold(otherSplitsNoDelims) { case (lt, rt) =>
      if (rt.left.is[T.RightBrace]) otherSplitsWithBraces(rt)
      else otherSplitsWithParens(lt, rt)
    }

    singleLineSplits ++ otherSplits
  }

}
