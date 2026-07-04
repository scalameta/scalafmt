package org.scalafmt
package internal

import org.scalafmt.config._
import org.scalafmt.util._

import org.scalameta.FileLine
import scala.meta._
import scala.meta.tokens.{Token => T}

import scala.annotation.tailrec

import Constants._
import ExpiresOn._
import FormatOps._
import Length.StateColumn
import Policy.NoPolicy
import PolicyOps._
import TokenOps._
import TreeOps._

sealed trait Splits {
  def get(implicit ft: FT, fo: FormatOps, cfg: ScalafmtConfig): Seq[Split]
}

object Splits {

  private class Multi(gen: List[Splits]) extends Splits {
    override def get(implicit
        ft: FT,
        fo: FormatOps,
        cfg: ScalafmtConfig,
    ): Seq[Split] = {
      var res = Seq.empty[Split]
      val iter = gen.iterator
      while (res.isEmpty && iter.hasNext) res = iter.next().get
      res
    }
  }

  def apply(gen: List[Splits]): Splits = gen match {
    case gen :: Nil => gen
    case _ => new Multi(gen)
  }

  def lowRankNL(ft: FT, cost: Int)(implicit fl: FileLine): Split =
    Split(Newline2x(ft), cost, rank = 1)

}

object SplitsNoSplit extends Splits {
  def get(implicit ft: FT, fo: FormatOps, cfg: ScalafmtConfig) =
    Seq(Split(NoSplit, 0))
}

object SplitsSpace extends Splits {
  def get(implicit ft: FT, fo: FormatOps, cfg: ScalafmtConfig) =
    Seq(Split(Space, 0))
}

object SplitsNewline extends Splits {
  def get(implicit ft: FT, fo: FormatOps, cfg: ScalafmtConfig) =
    Seq(Split(Newline, 0))
}

object SplitsNewline2X extends Splits {
  def get(implicit ft: FT, fo: FormatOps, cfg: ScalafmtConfig) =
    Seq(Split(Newline2x(ft), 0))
}

object SplitsBeforeStatement extends Splits { // New statement
  def get(implicit ft: FT, fo: FormatOps, cfg: ScalafmtConfig) = {
    import fo._, tokens._, ft._
    if (optimizationEntities.isStatementStart(idx + 1)) {
      val annoRight = right.is[T.At]
      val annoLeft = isSingleIdentifierAnnotation(prev(ft))

      if (
        (annoRight || annoLeft) && cfg.newlines.annotation &&
        !cfg.newlines.sourceIgnored
      ) Seq(Split(getMod(ft), 0))
      else InfixSplits.maybeGetInfixSplitsBeforeLhs(
        if (left.is[T.Comment] && noBreak) Space else Newline2x(ft),
      ) {
        val spaceCouldBeOk = annoLeft &&
          (cfg.newlines.source match {
            case Newlines.unfold => right.is[T.Comment] ||
              !cfg.newlines.annotation && annoRight
            case Newlines.fold => right.is[T.Comment] || annoRight ||
              !cfg.newlines.annotation && Reserved(right)
            case Newlines.keep => noBreak && (annoRight || Reserved(right))
            case _ => noBreak && Reserved(right)
          })
        def expire =
          (rightOwner match {
            case Tree.WithBody(body) => tokenBefore(body).nnMap { x =>
                val y = nextNonCommentSameLine(x)
                val ok = (x ne y) && y.noBreak && y.right.is[T.LeftBrace]
                if (ok) next(y) else y
              }
            case t: Mod.Annot if !cfg.newlines.keep =>
              getLast(t).nnMap(getSlbEndOnLeft)
            case _ => null
          }) ?? next(ft)
        Seq(
          // This split needs to have an optimalAt field.
          Split(Space, 0).onlyIf(spaceCouldBeOk).withSingleLine(expire),
          // For some reason, this newline cannot cost 1.
          Split(Newline2x(ft), 0),
        )
      }
    } else Nil
  }
}

object SplitsAfterInterpolationStart extends Splits {
  def get(implicit ft: FT, fo: FormatOps, cfg: ScalafmtConfig) = {
    import fo._, tokens._, ft._
    val end = matchingLeft(ft)
    val policy = {
      val penalty = BreakSingleLineInterpolatedString
      if (cfg.newlines.inInterpolation eq Newlines.InInterpolation.avoid) Policy
        .onLeft(end, "INTERP-AVOID-NL", rank = -1) { case Decision(_, ss) =>
          ss.map(s =>
            if (s.isNL) s.withPenalty(penalty)
            else if (s.optimalAt eq null) s
            else s.copy(optimalAt = null),
          )
        }
      else if (!cfg.newlines.sourceIgnored && !isTripleQuote(left.text)) Policy
        .onLeft(end, "INTERP-KEEP-NONL") {
          case Decision(x, ss) if x.noBreak => ss.penalizeNL(penalty)
        }
      else Policy ?
        (cfg.newlines.inInterpolation eq Newlines.InInterpolation.allow) &&
        Policy.onLeft(end, "INTERP-ALLOW-NL", rank = -1) {
          case Decision(_, ss) => ss.penalizeNL(1)
        }
    }
    val split = Split(NoSplit, 0, policy)
    val alignIndents =
      if (!cfg.align.stripMargin) null
      else findInterpolate(leftOwner).nnMap(ti =>
        getStripMarginChar(ti) match {
          case None => null
          case Some(pipe) =>
            val startsWithPipe = ti.parts.headOption match {
              case Some(Lit.String(x)) => x.headOption.contains(pipe)
              case _ => false
            }
            Seq(
              Indent(StateColumn, end, After),
              // -1 because of margin characters |
              Indent(if (startsWithPipe) -1 else -2, end, After),
            )
        },
      )
    val indents = alignIndents ?? Seq(Indent(cfg.indent.main, end, After))
    Seq(split.withIndents(indents))
  }
}

object SplitsAfterLeftBrace extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    if (right.is[T.RightBrace]) Seq(Split(NoSplit, 0))
    else if (existsParentOfType[ImportExportStat](leftOwner)) getImportSelectors
    else if (prev(ft).left.is[T.Interpolation.SpliceStart])
      getInterpolationSplice
    else if (isCapturingBrace(leftOwner)) {
      val close = matchingLeft(ft)
      Seq(Split(NoSplit, 0).withIndent(cfg.indent.main, close, Before))
    } else if (leftOwner.is[Type.Bounds]) {
      val close = matchingLeft(ft)
      val tb = leftOwner.asInstanceOf[Type.Bounds]
      val mod = Space(cfg.spaces.withinContextBoundBraces(tb))
      Seq(Split(mod, 0).withIndent(cfg.indent.main, close, Before))
    } else getRest
  }

  private def getImportSelectors(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    val close = matchingLeft(ft)
    val beforeClose = prev(close)
    val binPack = cfg.importSelectorsBinPack

    def dangleBraces = cfg.danglingParentheses.importSite
    val spacePolicy = binPack match {
      case ImportSelectors.keep | null if !dangleBraces && ft.noBreak =>
        NoPolicy
      case ImportSelectors.fold if !dangleBraces => NoPolicy
      case ImportSelectors.singleLine => SingleLineBlock(close, okSLC = true)
      case _ => SingleLineBlock(close)
    }
    def newlineBeforeClosingCurly(implicit fl: FileLine) =
      decideNewlinesOnlyBeforeClose(close)

    val mustDangleForTrailingCommas = getMustDangleForTrailingCommas(beforeClose)
    val mustUseNL = hasBreak && isRightCommentThenBreak(ft)
    val newlinePolicy = binPack match {
      case ImportSelectors.singleLine if mustUseNL => spacePolicy
      case ImportSelectors.singleLine if !mustDangleForTrailingCommas =>
        NoPolicy
      case ImportSelectors.unfold | null => newlineBeforeClosingCurly &
          splitOneArgOneLine(close, leftOwner)
      case _ => newlineBeforeClosingCurly
    }

    val indent = Indent(cfg.indent.main, close, Before)
    Seq(
      Split(Space(cfg.spaces.inImportCurlyBraces), 0, spacePolicy)
        .notIf(mustUseNL || mustDangleForTrailingCommas).withIndent(indent),
      Split(Newline, 1, newlinePolicy).notIf(newlinePolicy.isEmpty)
        .withIndent(indent),
    )
  }

  private def getInterpolationSplice(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    val close = matchingLeft(ft)
    val alignIndents =
      if (cfg.align.inInterpolation) Seq(
        Indent(StateColumn, close, ExpiresOn.After),
        Indent(cfg.indent.main, close, ExpiresOn.Before),
        Indent(-1, close, ExpiresOn.After),
      )
      else Nil
    def spaceSplit(implicit fileLine: FileLine) =
      Split(Space(cfg.spaces.inInterpolatedStringCurlyBraces), 0)
        .withIndents(alignIndents)
    def newlineSplit(cost: Int)(implicit fileLine: FileLine) = {
      val mainIndents =
        if (alignIndents.nonEmpty) alignIndents
        else Seq(Indent(cfg.indent.main, close, ExpiresOn.Before))
      Split(Newline, cost).withIndents(mainIndents)
        .withPolicy(decideNewlinesOnlyBeforeClose(close))
    }
    def isSimpleInterpolate = !(leftOwner match {
      case t: Pat.Interpolate => findArgAfter(left.end, t.args)
      case t: Term.Interpolate => findArgAfter(left.end, t.args)
      case t: Term.Block => getBlockSingleStat(t)
      case _ => null
    }).nnHas(_.is[Term.If])

    cfg.newlines.inInterpolation match {
      case Newlines.InInterpolation.avoid => Seq(spaceSplit)
      case _ if cfg.newlines.keepBreak(hasBreak) => Seq(newlineSplit(0))
      case Newlines.InInterpolation.allow
          if !dialect.allowSignificantIndentation || isSimpleInterpolate =>
        Seq(spaceSplit)
      case _ =>
        /* sequence of tokens:
         * - 0 RBrace
         * - 1 Interpolation.SpliceEnd (empty)
         * - 2 Interpolation.Part (next string)
         * - 3 Interpolation.End (quotes) or Interpolation.SliceStart/LBrace (${)
         */
        val afterClose = tokens(close, 3)
        val lastPart = afterClose.left.is[T.Interpolation.End]
        val slbEnd =
          getSlbEndOnLeft(if (lastPart) afterClose else next(afterClose))
        Seq(spaceSplit.withSingleLine(slbEnd), newlineSplit(1))
    }
  }

  private def getRest(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    val close = matchingLeft(ft)
    val isSelfAnnotationNL = cfg.newlines.selfAnnotation &&
      (hasBreak || cfg.newlines.sourceIgnored) &&
      (leftOwner match { // Self type: trait foo { self => ... }
        case t: Template.Body => t.selfOpt.nonEmpty
        case _ => false
      })
    val rightIsComment = right.is[T.Comment]
    val nl: Modification =
      if (hasBlankLine) Newline2x
      else if (!rightIsComment) Newline
      else if (noBreak) Space
      else Newline2x(!isSelfAnnotationNL && blankLineBeforeDocstring(ft))

    // lambdaNLOnly: None for single line only
    type LambdaInfo = (FT, Int, MaybeBool)
    def getLambdaNone: LambdaInfo = (null, 0, MaybeBool.Maybe)
    @tailrec
    def getLambdaInfo(ts: List[Tree]): LambdaInfo = ts match {
      case (t: Case) :: Nil if t.cond.isEmpty =>
        val arrow = getCaseArrow(t)
        val nlOnly = cfg.newlines.beforeCurlyLambdaParams match {
          case Newlines.BeforeCurlyLambdaParams.always => MaybeBool.True
          case Newlines.BeforeCurlyLambdaParams.never => MaybeBool.False
          case _ => MaybeBool.Maybe
        }
        (arrow, 0, nlOnly)
      case (t: Term.FunctionLike) :: Nil =>
        val arrow = lastLambda(t).nnMap(getFuncArrow) ?? getLast(t)
        val nlOnly = cfg.newlines.beforeCurlyLambdaParams match {
          case Newlines.BeforeCurlyLambdaParams.always => MaybeBool.True
          case Newlines.BeforeCurlyLambdaParams.multiline => MaybeBool.Maybe
          case _ => MaybeBool.False
        }
        (arrow, 0, nlOnly)
      case (t: Term.PartialFunction) :: Nil => getLambdaInfo(t.cases)
      case (t: Term.CasesBlock) :: Nil if t.parent.is[Term.SelectMatch] =>
        getLambdaInfo(t.cases)
      case (t: Term.Block) :: Nil if !isEnclosedInBraces(t) =>
        getLambdaInfo(t.stats)
      case _ => getLambdaNone
    }
    val (lambdaArrow, lambdaIndent, lambdaNLOnly) = leftOwner match {
      case t: Template.Body => t.selfOpt match {
          case None =>
            if (t.parent.parent.isOpt[Term.NewAnonymous]) getLambdaInfo(t.stats)
            else getLambdaNone
          case Some(owner) =>
            val anno = owner.tokens.last
            val indent = cfg.indent.main
            val annoFT = tokens(anno)
            val arrow = annoFT.left.is[T.RightArrow]
            val expire = if (arrow) annoFT else nextAfterNonComment(annoFT)
            (expire, indent, MaybeBool(isSelfAnnotationNL))
        }
      case t: Term.ArgClause if isEnclosedInBraces(t) => getLambdaInfo(t.values)
      case t: Term.Block => getLambdaInfo(t.stats)
      case t => getLambdaInfo(t :: Nil)
    }
    val noLambdaSplit = cfg.newlines.keepBreak(hasBreak) ||
      lambdaArrow == null || (lambdaNLOnly ne MaybeBool.False)
    val lambdaExpire = lambdaArrow.nnMap(nextNonCommentSameLine)

    def getClassicSingleLineDecisionOpt =
      if (noBreak) MaybeBool.True else MaybeBool.Maybe

    def getSingleLineLambdaDecisionOpt = {
      val ok = (lambdaNLOnly ne MaybeBool.True) &&
        Modification.getSpaceAndNewlineAfterCurlyLambda(newlinesBetween)._1
      if (ok) MaybeBool.True else MaybeBool.Maybe
    }

    // null if skipping
    val singleLineDecisionOpt = cfg.newlines.source match {
      case Newlines.keep if hasBreak => MaybeBool.Maybe
      case Newlines.unfold => MaybeBool.Maybe
      case Newlines.fold =>
        val isTopLevelBlock = leftOwner.parent.forall {
          case t: Template =>
            // false for
            // new A { () =>
            //   println("A")
            // }
            // but true for
            // new A {
            //   def f = x
            // }
            !t.parent.is[Term.NewAnonymous] || (t.body eq leftOwner) &&
            t.body.stats.exists(_.is[Defn])
          case _: Pkg | _: Source | _: Pkg.Body => true
          case t => t.parent.isAnyOpt[Source, Pkg.Body] // includes when parent is empty
        }

        // do not fold top-level blocks
        if (isTopLevelBlock) MaybeBool.Maybe
        else if (lambdaArrow != null) getSingleLineLambdaDecisionOpt
        else MaybeBool.False
      // old behaviour
      case _ =>
        if (lambdaArrow == null) getClassicSingleLineDecisionOpt
        else getSingleLineLambdaDecisionOpt
    }

    val noSplitMod = braceSpace(leftOwner)
    val (slbMod, slbParensExclude) =
      if (singleLineDecisionOpt eq MaybeBool.Maybe) (noSplitMod, None)
      else getBracesToParensMod(close, noSplitMod)
    val sld =
      if (slbParensExclude eq null) MaybeBool.Maybe else singleLineDecisionOpt
    val singleLineSplitOpt =
      if (sld eq MaybeBool.Maybe) null
      else {
        val ownerIfNeedBreakAfterClose = close.right match {
          case _: T.KwElse => close.rightOwner match {
              case p: Term.If if p.thenp eq leftOwner => p
              case _ => null
            }
          case _: T.KwCatch => close.rightOwner match {
              case p: Term.TryClause if p.expr eq leftOwner => p
              case _ => null
            }
          case _: T.KwFinally => close.rightOwner match {
              case p: Term.TryClause
                  if (p.expr eq leftOwner) ||
                    p.catchClause.contains(leftOwner) => p
              case _ => null
            }
          case _ => null
        }
        val expire = leftOwner.parent match {
          case Some(p: Term.ForYield)
              if !sld.asBoolean && (ownerIfNeedBreakAfterClose eq null) &&
                cfg.newlines.fold && leftOwner.is[Term.EnumeratorsBlock] =>
            getLast(p)
          case _ if cfg.newlines.isBeforeOpenParenCallSite => close
          case _ => getSlbEndOnLeft(close)
        }
        // copy logic from `( ...`, binpack=never, defining `slbSplit`
        val slbParensPolicy = Policy ? (slbMod eq noSplitMod) || {
          val beforeClose = prev(close)
          Policy.onlyFor(beforeClose, "BracesToParens")(
            _.flatMap(s => if (s.isNL) None else Some(s.withMod(NoSplit))),
          )
        }
        val exclude = slbParensExclude.getOrElse(TokenRanges.empty)
        val slbPolicy =
          if (exclude.isEmpty) slbParensPolicy
          else Policy.RelayOnSplit((s, _) => s.isNL)(slbParensPolicy)(
            Policy.onLeft(close, "BracesToParensFailed", terminal = true) {
              case _ => Nil
            },
          )
        val sldPolicy = Policy ? (ownerIfNeedBreakAfterClose eq null) || {
          if (cfg.newlines.fold) {
            val pend = getSlbEndOnLeft(getLast(ownerIfNeedBreakAfterClose))
            def pendSlb(s: Split) = s
              .withSingleLine(pend, noSyntaxNL = true, extend = true)
            Policy.onlyFor(close, s"RB-ELSE[${pend.idx}]")(ss =>
              if (ss.exists(_.isNL)) ss.map(s => if (s.isNL) s else pendSlb(s))
              else ss
                .flatMap(s => Seq(pendSlb(s), s.withMod(Newline).withPenalty(1))),
            )
          } else decideNewlinesOnlyAfterClose(close)
        }
        Split(slbMod, 0).withSingleLine(
          expire,
          exclude = exclude,
          noOptimal = cfg.newlines.fold && !exclude.isEmpty &&
            exclude.ranges.forall(_.lt.left.is[T.LeftParen]),
          noSyntaxNL = true,
        ).andPolicy(sldPolicy & slbPolicy)
      }

    val lambdaNLPolicy = leftOwner match {
      case t: Template.Body if lambdaExpire ne null =>
        t.stats match {
          case Nil | (_: Term) :: Nil => NoPolicy
          case _ => decideNewlinesOnlyAfterToken(lambdaExpire)
        }
      case _ => NoPolicy
    }
    val (nlCost, nlArrowPenalty) =
      if (!nl.isNL) (0, 0)
      else {
        if (slbMod eq noSplitMod) null
        else getLambdaPenaltiesOnLeftBraceOnLeft(ft)
      } match {
        case null => (1, 0)
        case (shared, here) => (shared + here, shared + 1)
      }
    val newlineBeforeClosingCurly =
      decideNewlinesOnlyBeforeClose(Split(Newline, 0, rank = -1))(close)
    val nlPolicy = lambdaNLPolicy ==> newlineBeforeClosingCurly
    val nlSplit = Split(nl, nlCost, nlPolicy)
      .withIndent(cfg.indent.main, close, Before)

    val singleLineSplit =
      if (
        (singleLineSplitOpt ne null) &&
        (noLambdaSplit || !slbParensExclude.forall(_.isEmpty))
      ) singleLineSplitOpt
      else Split.ignored

    // must be after nlSplit
    val lambdaSplit =
      if (noLambdaSplit) Split.ignored
      else {
        val nlPolicy = newlineBeforeClosingCurly
        var mod: Modification = null
        val policy =
          if ((singleLineSplitOpt ne null) && singleLineSplit.isIgnored) {
            val arrSplit = singleLineSplitOpt.withMod(Space)
            val fnarrDesc = s"FNARR($nlArrowPenalty;$arrSplit)"
            mod = slbMod
            Policy.onlyFor(lambdaExpire, fnarrDesc) { ss =>
              var hadNoSplit = false
              val nlSplits = ss.flatMap(s =>
                // penalize NL one extra, for closing brace
                if (s.isNL)
                  Some(s.andPolicy(nlPolicy).withPenalty(nlArrowPenalty))
                else { hadNoSplit = true; None },
              )
              if (hadNoSplit) arrSplit +: nlSplits else nlSplits
            }
          } else {
            mod = noSplitMod
            decideNewlinesOnlyAfterToken(lambdaExpire) ==> nlPolicy
          }
        Split(mod, 0, SingleLineBlock(lambdaExpire) ==> policy)
          .withOptimalToken(lambdaExpire, killOnFail = true)
          .withIndent(lambdaIndent, close, Before)
      }

    val splits = Seq(singleLineSplit, lambdaSplit, nlSplit)
    right match {
      case _: T.Xml.Start => withIndentOnXmlStart(matchingRight(ft), splits)
      case _ => splits
    }
  }

}

object SplitsAfterEquals extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = ft.leftOwner match {
    case x: Enumerator.Assign => SplitsAfterEqualsLeftArrow
        .getSplitsEnumerator(x.rhs)
    case _: Enumerator => Nil // it's WithBody
    case t: Ctor.Secondary => getDefValAssign(t.body.init)
    case t: Defn.Macro if fo.dialect.allowSignificantIndentation =>
      // scala3 compiler doesn't allow newline before `macro`
      val nlPolicy = decideNewlinesOnlyAfterClose(fo.tokens.next(ft))
      getDefValAssign(t.body)
        .map(s => if (s.isNL) s.withMod(Space).andPolicy(nlPolicy) else s)
    case t: Tree.WithBody => getDefValAssign(t.body)
    case t: Term.Param => t.default match {
        case Some(x) => getDefValAssign(x)
        case _ => Nil
      }
    case _ => Nil
  }

  private def getDefValAssign(
      rhs: Tree,
  )(implicit ft: FT, fo: FormatOps, cfg: ScalafmtConfig) = {
    import fo._, tokens._, ft._
    InfixSplits.maybeGetInfixSplitsBeforeLhs() {
      def endFt = getLast(rhs)
      getSplitsDefValEquals(rhs, endFt)(
        if (leftOwner.is[Tree.WithParamClauses]) getSplitsDefEquals(rhs, endFt)
        else getSplitsValEquals(rhs, endFt)(getSplitsValEqualsClassic(rhs, endFt)),
      )
    }
  }

  def getSplitsDefValEquals(
      body: Tree,
      endFt: => FT,
      spaceIndents: Seq[Indent] = Seq.empty,
  )(
      splits: => Seq[Split],
  )(implicit ft: FT, fo: FormatOps, cfg: ScalafmtConfig): Seq[Split] = {
    import fo._, tokens._, ft._
    if (right.is[T.LeftBrace]) // The block will take care of indenting by 2
      Seq(Split(Space, 0).withIndents(spaceIndents))
    else if (isRightCommentWithBreak(ft))
      Seq(CtrlBodySplits.withIndent(Split(Space.orNL(ft), 0), body, endFt))
    else if (cfg.newlines.neverBeforeJsNative && isJsNative(body))
      Seq(Split(Space, 0).withSingleLineNoOptimal(endFt))
    else if (
      cfg.dialect.allowSignificantIndentation &&
      (cfg.newlines.sourceIgnored || noBreak) && body.parent.exists {
        case p: Enumerator.Assign => (p.body eq body) && p.parent.exists {
            case pp: Term.EnumeratorsBlock => isEnclosedWithinParens(pp)
            case _ => false
          }
        case _ => false
      }
    ) Seq(Split(Space, 0).withIndents(spaceIndents))
    else if (cfg.newlines.forceBeforeAssign(leftOwner))
      Seq(CtrlBodySplits.withIndent(Splits.lowRankNL(ft, 0), body, endFt))
    else if (cfg.newlines.shouldForceBeforeMultilineAssign(leftOwner))
      CtrlBodySplits.slbOnly(body, spaceIndents)(x =>
        CtrlBodySplits.withIndent(Splits.lowRankNL(ft, x), body, endFt),
      )
    else splits
  }

  private def getSplitsDefEquals(body: Tree, endFt: FT)(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import ft._
    def baseSplit = Split(Space, 0)

    def newlineSplit(cost: Int)(implicit fileLine: FileLine) = fo.CtrlBodySplits
      .withIndent(Splits.lowRankNL(ft, cost), body, endFt)

    def getClassicSplits(implicit fileLine: FileLine) =
      if (hasBreak) Seq(newlineSplit(0)) else Seq(baseSplit, newlineSplit(1))

    cfg.newlines.beforeMultilineDef
      .fold(getSplitsValEquals(body, endFt)(getClassicSplits)) {
        case Newlines.classic => getClassicSplits

        case Newlines.keep if hasBreak => Seq(newlineSplit(0))

        case Newlines.unfold =>
          Seq(baseSplit.withSingleLine(endFt), newlineSplit(1))

        case x => fo.CtrlBodySplits
            .folded(body, x eq Newlines.keep)(newlineSplit)
      }
  }

  private def getSplitsValEquals(body: Tree, endFt: => FT)(
      classicSplits: => Seq[Split],
  )(implicit ft: FT, fo: FormatOps, cfg: ScalafmtConfig): Seq[Split] =
    if (cfg.newlines.getBeforeMultiline eq Newlines.classic) classicSplits
    else fo.CtrlBodySplits
      .getWithIndent(body, endFt)(null)(Splits.lowRankNL(ft, _))

  def getSplitsValEqualsClassic(rawBody: Tree, endFt: FT)(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    def wouldDangle = leftOwner.parent
      .exists(cfg.danglingParentheses.atSite(_, false))

    // rhsOptimalToken is too aggressive here
    val optimal = endFt.right match {
      case _: T.Comma => next(endFt)
      case RightParenOrBracket() if !wouldDangle => next(endFt)
      case _ => endFt
    }

    def optimalWithComment = optimal.right match {
      case _: T.Comment if optimal.noBreak => next(optimal)
      case _ => optimal
    }

    val penalty = leftOwner match {
      case _: Term.Assign if cfg.binPack.callSite != BinPack.Site.Never =>
        Constants.BinPackAssignmentPenalty
      case _: Term.Param if cfg.binPack.defnSite != BinPack.Site.Never =>
        Constants.BinPackAssignmentPenalty
      case _ => 0
    }

    val body = TreeOps.getBlockStat(rawBody)
    def noSpace: Boolean = hasBreak && leftOwner.is[Defn] ||
      !cfg.newlines.ignoreInSyntax && (tokens.getNonMultilineEnd(ft) eq null)
    Seq(
      if (isRightCommentThenBreak(ft)) Split.ignored
      else if (body.isAny[Term.If, Term.ForYield, Term.TryClause])
        Split(hasBreak, 0)(Space).withOptimalToken(optimal, killOnFail = false)
          .withPolicy {
            val exclude = insideBracesBlock(ft, endFt)
            policyWithExclude(exclude, Policy.End.OnLeft, Policy.End.OnRight)(
              PenalizeAllNewlines(endFt, Constants.ShouldBeSingleLine),
            )
          }
      else Split(noSpace, 0)(Space)
        .withOptimalToken(optimalWithComment, killOnFail = false),
      CtrlBodySplits.withIndent(Split(Newline, 1 + penalty), body, endFt),
    )
  }
}

object SplitsAfterLeftArrow extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = ft.leftOwner match {
    case x: Enumerator.Assign => SplitsAfterEqualsLeftArrow
        .getSplitsEnumerator(x.rhs)
    case _ => Nil
  }

}

object SplitsAfterEqualsLeftArrow {
  def getSplitsEnumerator(
      body: Tree,
  )(implicit ft: FT, fo: FormatOps, cfg: ScalafmtConfig): Seq[Split] = {
    import fo._, tokens._
    if (body.is[Term.Block] && isEnclosedInBraces(body)) Seq(Split(Space, 0))
    else InfixSplits.maybeGetInfixSplitsBeforeLhs() {
      val endFt = getLastNonTrivial(body)
      val noSpace = !cfg.align.arrowEnumeratorGenerator ||
        (body match {
          case t: Term.TryClause => Some(getHead(t))
          case t: Term.ForClause => Some(getHead(t))
          case t: Term.If => Some(tokenBefore(t.thenp))
          case _ => None
        }).exists(OptionalBraces.at)
      val spaceIndents =
        if (noSpace) Seq.empty else Seq(Indent(StateColumn, endFt, After))
      SplitsAfterEquals.getSplitsDefValEquals(body, endFt, spaceIndents) {
        CtrlBodySplits.get(body, spaceIndents) {
          if (spaceIndents.nonEmpty) Split(Space, 0).withIndents(spaceIndents)
          else {
            val noSlb = body match {
              case _: Term.TryClause => false
              case _: Term.ForClause => false
              case t: Term.If => ifWithoutElse(t)
              case _ => true
            }
            val noSlbEnd =
              if (!noSlb) null
              else if (cfg.newlines.ignoreInSyntax) next(ft)
              else tokens.getNonMultilineEnd(ft)
            if (noSlbEnd eq null) Split(Space, 0).withSingleLine(endFt)
            else Split(Space, 0).withOptimalToken(noSlbEnd, killOnFail = false)
          }
        }(cost => CtrlBodySplits.withIndent(Splits.lowRankNL(ft, cost), endFt))
      }
    }
  }
}

object SplitsBeforeLeftBrace extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    if (isCapturingBrace(rightOwner)) Seq(Split(NoSplit, 0))
    else if (isXmlBrace(rightOwner))
      withIndentOnXmlSpliceStart(ft, Seq(Split(NoSplit, 0)))
    else if ( // non-statement starting curly brace
      !cfg.newlines.configStyle.beforeComma && left.is[T.Comma] &&
      isArgClauseSite(leftOwner)
    ) {
      val close = matchingRight(ft)
      val binPackIsEnabled = cfg.binPack.callSiteFor(leftOwner) !=
        BinPack.Site.Never
      val useSpace = !cfg.newlines.keepBreak(hasBreak)
      val singleSplit =
        if (!binPackIsEnabled) Split(Space.orNL(useSpace), 0)
        else Split(Space, 0).onlyIf(useSpace).withSingleLine(close)
      val otherSplits = rightOwner match {
        case _: Term.PartialFunction | Term
              .Block(List(_: Member.Function | _: Term.PartialFunction)) =>
          Seq(Split(Newline, 0))
        case _ =>
          val breakAfter = getSlbEndOnLeft(nextAfterNonCommentSameLine(ft))
          val multiLine = decideNewlinesOnlyAfterToken(breakAfter) ==>
            decideNewlinesOnlyBeforeClose(close)
          Seq(
            Split(Newline, 0, penalty = -1).withSingleLine(close),
            Split(Space, 1, multiLine),
          )
      }
      val oneArgPerLineSplits =
        if (binPackIsEnabled) otherSplits
          .map(_.preActivateFor(SplitTag.OneArgPerLine))
        else otherSplits.map(_.onlyFor(SplitTag.OneArgPerLine))
      singleSplit +: oneArgPerLineSplits
    } else {
      val beforeStmt = SplitsBeforeStatement.get
      if (beforeStmt.nonEmpty) beforeStmt
      else {
        def maybeBracesToParensWithRB(rb: FT)(implicit fl: FileLine) =
          Seq(Split(getBracesToParensModOnly(rb, isWithinBraces = false), 0))
        def maybeBracesToParens()(implicit fl: FileLine) =
          maybeBracesToParensWithRB(matchingRight(ft))
        // partial initial expr
        if (!isTokenHeadOrBefore(right, rightOwner)) maybeBracesToParens()
        else rightOwner.parent match {
          case None => Nil
          case Some(_: Term.ApplyInfix) => Nil // exclude start of infix
          case Some(_: Term.ArgClause) => maybeBracesToParens()
          case Some(p) =>
            val rb = matchingRightOrNull(ft)
            if (
              (rb eq null) ||
              isTokenHeadOrBefore(right, p) &&
              isTokenLastOrAfter(rb.left, rightOwner)
            ) Nil
            else maybeBracesToParensWithRB(rb)
        }
      }
    }
  }
}

object SplitsAfterRightBraceLowPriority extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    if (isXmlBrace(leftOwner)) Seq(Split(NoSplit, 0))
    else leftOwner match {
      case t: Term.EnumeratorsBlock => t.parent match {
          case Some(p: Term.For) if !nextNonComment(ft).right.is[T.KwDo] =>
            val body = p.body
            def nlSplit(cost: Int) = Splits.lowRankNL(ft, cost)
              .withIndent(cfg.indent.main, getLast(body), After)
            CtrlBodySplits.get(body)(null)(nlSplit)
          case _ => Nil
        }
      case _ => Nil
    }
  }
}

object SplitsBeforeRightBrace extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    if (left.is[T.LeftBrace]) Seq(Split(NoSplit, 0))
    else if (existsParentOfType[ImportExportStat](rightOwner))
      Seq(Split(Space(cfg.spaces.inImportCurlyBraces), 0))
    else if (next(ft).right.is[T.Interpolation.SpliceEnd])
      Seq(Split(Space(cfg.spaces.inInterpolatedStringCurlyBraces), 0))
    else if (rightOwner.is[Type.Bounds]) { // context bounds
      val tb = rightOwner.asInstanceOf[Type.Bounds]
      Seq(Split(Space(cfg.spaces.withinContextBoundBraces(tb)), 0))
    } else {
      val mod = Newline2x.orMod(ft.hasBlankLine, braceSpace(rightOwner))
      Seq(Split(mod, 0, rank = 1))
    }
  }
}

object SplitsAfterFunctionArrow extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, ft._
    leftOwner match {
      case leftFunc: Term.FunctionLike =>
        val isBlockFunc = (!right.is[T.Comment] || ft.hasBreak) &&
          !tokens.isEmpty(leftFunc.body) && isBlockFunction(leftFunc)
        if (isBlockFunc) blockFunctionTerm(leftFunc) else functionOrSelf
      case t: Self if t.ancestor(2).is[Term.NewAnonymous] => functionOrSelf
      case _ => Nil
    }
  }

  private def functionOrSelf(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    val (endOfFunction, expiresOn) = leftOwner match {
      case t: Term.FunctionLike => functionExpire(t)
      case t => getLastNonTrivial(t) -> ExpiresOn.Before
    }

    val indent = // don't indent if the body is empty `{ x => }`
      if (isEmptyFunctionBody(leftOwner) && !right.is[T.Comment]) 0
      else if (leftOwner.is[Template]) 0 // { applied the indent
      else (leftOwner.parent match {
        case Some(ac: Term.ArgClause) => ac.parent
        case Some(x: Term) => x.parent.flatMap {
            case ac: Term.ArgClause => ac.parent
            case _ => None
          }
        case _ => None
      }) match {
        case Some(p: Term.Apply) if isFewerBraces(p) =>
          cfg.indent.getSignificant
        case _ => cfg.indent.main
      }

    def noSingleLine = {
      // for constructors with empty args lambda
      // new Foo { () =>
      //   println("wow")
      // }
      val isCurlyLambda = leftOwner.is[Template.Body] ||
        leftOwner.parent.is[Template.Body]

      def noSquash = cfg.newlines.afterCurlyLambdaParams ne
        Newlines.AfterCurlyLambdaParams.squash

      cfg.newlines.source match {
        case Newlines.fold => false
        case Newlines.unfold => isCurlyLambda && noSquash
        case Newlines.keep => hasBreak
        case Newlines.classic => isCurlyLambda && hasBreak && noSquash
      }
    }
    def newlineSplit(cost: Int)(implicit fileLine: FileLine) =
      Split(Newline, cost).withIndent(indent, endOfFunction, expiresOn)
    if (isRightCommentThenBreak(ft)) Seq(newlineSplit(if (noBreak) 0 else 1))
    else {
      // 2020-01: break after same-line comments, and any open brace
      val nonComment = nextNonCommentSameLine(ft)
      val hasBlock = nonComment.right.is[T.LeftBrace] &&
        (matchingRight(nonComment) eq endOfFunction)
      val noSplit =
        if (!hasBlock && (nonComment eq ft)) Split(noSingleLine, 0)(Space)
          .withSingleLine(endOfFunction)
        else
          // break after the brace or comment if fits, or now if doesn't
          // if brace, don't add indent, the LeftBrace rule will do that
          Split(Space, 0).withIndent(indent, endOfFunction, expiresOn, hasBlock)
            .withOptimalToken(
              nextNonCommentSameLineAfter(nonComment),
              killOnFail = false,
            )
      Seq(noSplit, newlineSplit(1 + nestedApplies(leftOwner)))
    }
  }

  private def blockFunctionTerm(
      leftFunc: Member.Function,
  )(implicit ft: FT, fo: FormatOps, cfg: ScalafmtConfig): Seq[Split] = {
    import fo._, tokens._, ft._
    def spaceSplitBase(implicit line: FileLine): Split = Split(Space, 0)
    if (canBreakAfterFuncArrow(leftFunc)) {
      val (afterCurlySpace, afterCurlyNewlines) = Modification
        .getSpaceAndNewlineAfterCurlyLambda(newlinesBetween)
      val spaceSplit = leftFunc.body match {
        case _: Member.Function => spaceSplitBase
        case Term.Block((_: Member.Function) :: Nil)
            if !nextNonComment(ft).right.is[T.LeftBrace] => spaceSplitBase
        case _ if afterCurlySpace && {
              cfg.newlines.fold || !rightOwner.is[Defn]
            } =>
          val exp = nextNonCommentSameLine(getLastNonTrivial(leftFunc.body))
          spaceSplitBase.withSingleLine(exp, noSyntaxNL = true)
        case _ => Split.ignored
      }
      val (endIndent, expiresOn) = functionExpire(leftFunc)
      Seq(
        spaceSplit,
        Split(afterCurlyNewlines, 1)
          .withIndent(cfg.indent.main, endIndent, expiresOn),
      )
    } else Seq(spaceSplitBase)
  }
}

object SplitsAfterRightArrow extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import ft._
    leftOwner match {
      case t: CaseTree if !right.isAny[T.KwCatch, T.KwFinally, T.Dot] => // Case arrow
        caseTree(t)
      case _: Type.ByNameType => Seq(Split(Space(cfg.spaces.inByNameTypes), 0))
      case _ => Nil
    }
  }

  def caseTree(
      owner: CaseTree,
  )(implicit ft: FT, fo: FormatOps, cfg: ScalafmtConfig): Seq[Split] = {
    import fo._, tokens._, ft._
    val body = owner.body
    val condIsDefined = leftOwner match {
      case c: Case => c.cond.isDefined
      case _ => false
    }
    val bodyIsEmpty = isEmptyTree(body)
    def baseSplit(implicit l: FileLine) = Split(Space, 0)
    def nlSplit(ft: FT)(cost: Int)(implicit l: FileLine) = Splits
      .lowRankNL(ft, cost)
    CtrlBodySplits.checkComment(nlSplit(ft)) { nft =>
      def withSlbSplit(implicit l: FileLine) =
        Seq(baseSplit.withSingleLine(getLastNonTrivial(body)), nlSplit(nft)(1))
      implicit val beforeMultiline = cfg.newlines.getBeforeMultiline
      def getNLOnlySplit(cost: Int)(implicit l: FileLine) =
        Seq(nlSplit(nft)(cost))
      def getFolded(isKeep: Boolean)(implicit l: FileLine) = CtrlBodySplits
        .foldedNonEmptyNonComment(body, nlSplit(nft), isKeep)
      if (
        isCaseBodyABlock(nft, owner) ||
        (getClosingIfCaseBodyEnclosedAsBlock(nft, owner) ne null)
      ) Seq(baseSplit)
      else if (nft.right.is[T.KwCase]) getNLOnlySplit(0)
      else if (hasBreak && !beforeMultiline.ignoreSourceSplit)
        if ((beforeMultiline eq Newlines.keep) && !bodyIsEmpty)
          getFolded(isKeep = true).filter(_.isNL)
        else getNLOnlySplit(1)
      else if (bodyIsEmpty)
        if (right.isAny[T.RightBrace, T.Semicolon])
          Seq(baseSplit, nlSplit(nft)(1))
        else getNLOnlySplit(1)
      else if (beforeMultiline eq Newlines.unfold)
        if (cfg.newlines.unfold) getNLOnlySplit(0) else withSlbSplit
      else if (
        condIsDefined || beforeMultiline.eq(Newlines.classic) ||
        (getSingleStatExceptEndMarker(body) eq null)
      ) withSlbSplit
      else getFolded(beforeMultiline eq Newlines.keep)
    }
  }
}

object SplitsAfterGiven extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    leftOwner match {
      case gvn: Stat.GivenLike =>
        if (cfg.newlines.unfold && gvn.paramClauseGroups.nonEmpty) {
          val nonSlbPolicy = gvn.paramClauseGroups.flatMap(pcg =>
            if (pcg.tparamClause.values.isEmpty) pcg.paramClauses
            else pcg.tparamClause +: pcg.paramClauses,
          ).foldLeft(Policy.noPolicy) { case (policy, pc) =>
            val afterpc = tokenAfter(pc)
            val pcPolicy = Policy ? afterpc.right.is[T.RightArrow] &&
              decideNewlinesOnlyAfterToken(afterpc)
            policy ==> pcPolicy
          }
          if (nonSlbPolicy.isEmpty) Seq(Split(Space, 0))
          else Seq(
            Split(Space, 0)
              .withSingleLine(getSlbEndOnLeft(getLast(gvn.paramClauseGroups.last))),
            Split(Space, 1, nonSlbPolicy),
          )
        } else Seq(Split(Space, 0))
      case _ => Nil
    }

  }
}

object SplitsBeforeRightArrow extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    // Given conditional arrow
    rightOwner match {
      case pcg: Member.ParamClauseGroup => pcg.parent match {
          case Some(gvn: Stat.GivenLike) =>
            val nlOnly = !cfg.newlines.sourceIgnored && hasBreak
            def spaceSplit(implicit fl: FileLine) = Split(nlOnly, 0)(Space)
            val nextParamClause = (pcg.tparamClause +: pcg.paramClauses)
              .findOrNull(_.endOffset > right.end) ??
              (gvn.paramClauseGroups.dropWhile(_ ne pcg) match {
                case `pcg` :: pcgNext :: _ =>
                  val tpc = pcgNext.tparamClause
                  if (tpc.nonEmpty) tpc else pcgNext.paramClauses.headOrNull
                case _ => null
              })
            nextParamClause.nnFold {
              gvn match {
                case gvn: Defn.Given => binPackParentConstructorSplits(
                    isFirstCtor = true,
                    owners = Set(gvn.templ),
                    rhs = gvn.templ.inits.headOrNull,
                    lastFt = templateDerivesOrCurlyOrLastNonTrivial(gvn.templ),
                    indentLen = cfg.indent.extendSite,
                    extendsThenWith = gvn.templ.inits.lengthCompare(1) > 0,
                  )
                case _ =>
                  val end = gvn match {
                    case gvn: Tree.WithBody => tokenBefore(gvn.body)
                    case _ => getLast(gvn)
                  }
                  val noSlb = gvn match {
                    case gvn: Tree.WithDeclTpe => gvn.decltpe match {
                        case t: Type.Tuple => t.args.lengthCompare(1) > 0
                        case _ => false
                      }
                    case _ => false
                  }
                  Seq(
                    spaceSplit
                      .withSingleLine(getSlbEndOnLeft(end), ignore = noSlb),
                    Split(Newline, 1)
                      .withIndent(cfg.indent.main, end, ExpiresOn.After),
                  )
              }
            } { npc =>
              val nextArrow = getSlbEndOnLeft(nextNonCommentSameLine(getLast(npc)))
              val noSlb = npc.values.lengthCompare(1) != 0
              Seq(
                spaceSplit.withSingleLine(nextArrow, ignore = noSlb),
                Split(Newline, 1)
                  .withIndent(cfg.indent.main, nextArrow, ExpiresOn.Before),
              )
            }

          case _ => Nil
        }
      case _ => Nil
    }
  }
}

object SplitsBeforeSemicolon extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    val forceBreak = hasBreak && {
      val ltoks = leftOwner.tokens
      val maxTokens = topSourceTree.tokens.length
      !ltoks
        .getWideOpt(ltoks.skipWideIf(_.is[T.Whitespace], ltoks.length, maxTokens))
        .contains(right) // something was removed
    }
    val policy = Policy ? forceBreak &&
      decideNewlinesOnlyAfterToken(nextNonCommentSameLineAfter(ft))
    Seq(Split(NoSplit, 0, policy))
  }
}

object SplitsAfterSemicolon extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    optimizationEntities.statementStart(idx + 1) match {
      case stmt if (stmt ne null) && !stmt.is[Term.EndMarker] =>
        val noSpace = !cfg.newlines.okSpaceForSource(newlinesBetween) ||
          cfg.dialect.allowSignificantIndentation &&
          stmt.is[Case] && stmt.parent.forall {
            case p: Tree.Block => !isEnclosedInMatching(p)
            case _ => false
          }
        Seq(
          Split(noSpace, 0)(Space).withSingleLine(getSlbEndOnLeft(getLast(stmt))),
          // For some reason, this newline cannot cost 1.
          Split(Newline2x(ft), 0),
        )
      case _ => Nil
    }
  }
}

object SplitsAfterRightParen extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    leftOwner match {
      case ParamClauseParent(_: Defn.ExtensionGroup)
          if !dialect.allowSignificantIndentation &&
            !LeftParenOrBrace(nextNonComment(ft).right) => Seq(Split(Space, 0))
      case t: Term.If
          if !nextNonComment(ft).right.is[T.KwThen] &&
            !isTokenLastOrAfter(left, t) => getIfWhileFor(t.thenp)
      case t: Term.While
          if !nextNonComment(ft).right.is[T.KwDo] &&
            !isTokenLastOrAfter(left, t) => getIfWhileFor(t.body)
      case t: Term.EnumeratorsBlock => t.parent match {
          case Some(p: Term.For) if !nextNonComment(ft).right.is[T.KwDo] =>
            getIfWhileFor(p.body)
          case Some(p: Term.ForYield) if cfg.indent.yieldKeyword =>
            getIfWhileFor(p.body)
          case _ => Nil
        }
      case _ => Nil
    }
  }

  def getIfWhileFor(
      bodyOriginal: Tree,
  )(implicit ft: FT, fo: FormatOps, cfg: ScalafmtConfig): Seq[Split] = {
    import fo._, tokens._
    val body = getBlockStat(bodyOriginal)
    val nft = nextNonCommentSameLine(ft)
    if (nft.right.is[T.LeftBrace] && body.is[Term.Block]) Seq(Split(Space, 0))
    else {
      val expire = getLast(body)
      def nlSplitFunc(cost: Int)(implicit fl: FileLine) = Splits
        .lowRankNL(ft, cost).withIndent(cfg.indent.main, expire, After)
      if (cfg.newlines.getBeforeMultiline eq Newlines.unfold) CtrlBodySplits
        .checkComment(nlSplitFunc)(nft =>
          if (nft.right.is[T.LeftBrace]) {
            val nextFt = nextNonCommentSameLineAfter(nft)
            val policy = decideNewlinesOnlyAfterToken(nextFt)
            Seq(Split(Space, 0, policy))
          } else Seq(nlSplitFunc(0)),
        )
      else CtrlBodySplits.get(body)(Split(Space, 0).withSingleLineNoOptimal(
        expire,
        insideBracesBlock(ft, expire),
        noSyntaxNL = nft.right.is[T.KwYield],
      ))(nlSplitFunc)
    }
  }
}

// Opening [ with no leading space.
// Opening ( with no leading space.
object SplitsBeforeLeftParenOrBracket extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    val prevFt = prevNonComment(ft)
    if (
      noSpaceBeforeOpeningParen(rightOwner) && {
        prevFt.left match {
          case _: T.RightParen | _: T.RightBrace =>
            prevFt.meta.leftOwner match {
              case _: Term.For | _: Term.If | _: Term.While => false
              case t: Term.EnumeratorsBlock => t.parent.is[Term.For]
              case _ => true
            }
          case _: T.RightBracket | _: T.KwSuper | _: T.KwThis | _: T.Ident |
              _: T.Underscore | _: T.Constant.Symbol => true
          case _ => false
        }
      }
    ) {
      def modification: Modification = Space {
        leftOwner match {
          case _ if left.is[T.Comment] => true
          case _: Mod => true
          // Add a space between constructor annotations and their parameter lists
          // see:
          // https://github.com/scalameta/scalafmt/pull/1516
          // https://github.com/scalameta/scalafmt/issues/1528
          case t: Init => t.parent.isOpt[Mod.Annot]
          case Term.Name(name) =>
            cfg.spaces.afterTripleEquals && name == "===" ||
            (rightOwner match {
              case _: Term.ArgClause => cfg.spaces.beforeApplyArgInParens(name)
              case _: Member.ParamClause => cfg.spaces.afterSymbolicDefs &&
                isSymbolicName(name)
              case _ => false
            })
          case _: Defn.ExtensionGroup => cfg.spaces.afterKeywordBeforeParen &&
            soft.KwExtension.matches(left)
          case _ => false
        }
      }
      def baseNoSplit(implicit fileLine: FileLine) = Split(modification, 0)
      val defn = isParamClauseSite(rightOwner)
      val defRhs = if (defn) defDefBodyParent(rightOwner) else null
      val beforeDefRhs = defRhs.nnMap(tokenJustBefore)
      def getSplitsBeforeOpenParen(
          src: Newlines.SourceHints,
          indentLen: Int,
          shouldAlignBefore: Align => Boolean,
      )(lastSyntaxClause: => Member.SyntaxValuesClause) = {
        val close = matchingRight(ft)
        val indent = Indent(indentLen, close, ExpiresOn.After)
        val isAlignFirstParen = shouldAlignBefore(cfg.align) &&
          !prevNonComment(ft).left.is[T.RightParen]
        def noSplitSplit(implicit fileLine: FileLine) =
          if (isAlignFirstParen) baseNoSplit
          else baseNoSplit.withSingleLine(close)
        def afterClose: FT = {
          val ftAfterClose = nextNonComment(close)
          val matches = ftAfterClose.right match {
            case _: T.LeftParen => true
            case _: T.Colon if defn =>
              ftAfterClose.left.is[T.Comment] ||
              cfg.newlines.sometimesBeforeColonInMethodReturnType &&
              (colonDeclType(ftAfterClose.meta.rightOwner) ne null)
            case _ => false
          }
          if (matches) next(ftAfterClose) else null
        }
        val splits = src match {
          case Newlines.unfold =>
            val rightParent = rightOwner.parent.get
            val slbEnd =
              if (!defn) getLast(getLastCall(rightParent))
              else if (beforeDefRhs eq null) getLast(rightParent)
              else prevNonComment(beforeDefRhs)
            Seq(
              baseNoSplit.withSingleLine(slbEnd),
              Split(Newline, 1).withIndent(indent).withPolicy(
                penalizeNewlineByNesting(ft, close),
                isSeqMulti(getArgs(rightOwner)),
              ).andPolicy(afterClose.nnMap(decideNewlinesOnlyBeforeClose)),
            )
          case Newlines.keep =>
            if (hasBreak) Seq(Split(Newline, 0).withIndent(indent))
            else Seq(noSplitSplit, Split(Newline, 1).withIndent(indent))
          case _ =>
            val nlColonPolicy = afterClose match {
              case x @ FT(_: T.Colon, _, _) => decideNewlinesOnlyBeforeClose(x)
              case _ => NoPolicy
            }
            Seq(
              noSplitSplit,
              Split(Newline, 1).withIndent(indent).withPolicy(nlColonPolicy),
            )
        }
        val args = if (isAlignFirstParen) lastSyntaxClause else null
        val argsLast = args.nnMap(getLast)
        if (argsLast eq null) splits
        else {
          val noSplitIndents = Seq(
            Indent(StateColumn, argsLast, ExpiresOn.Before),
            Indent(-indentLen, argsLast, ExpiresOn.Before),
          )
          splits.map(s => if (s.isNL) s else s.withIndents(noSplitIndents))
        }
      }
      val beforeOpenParenSplits =
        if (!right.is[T.LeftParen]) null
        else if (defn) cfg.newlines.getBeforeOpenParenDefnSite.nnMap { x =>
          val indent =
            if (beforeDefRhs eq null) cfg.indent.main
            else {
              val ob = OptionalBraces.at(beforeDefRhs)
              cfg.indent.extraBeforeOpenParenDefnSite +
                (if (ob) cfg.indent.getSignificant else cfg.indent.main)
            }
          getSplitsBeforeOpenParen(x, indent, _.beforeOpenParenDefnSite) {
            @tailrec
            def iter(tree: Tree): Member.ParamClause = tree match {
              case _: Member.ParamClause => tree.parent match {
                  case Some(p) => iter(p)
                  case None => null
                }
              case p: Tree.WithParamClauses => p.paramClauses.lastOrNull
              case _ => null
            }
            iter(rightOwner)
          }
        }
        else if (cfg.dialect.allowSignificantIndentation) cfg.newlines
          .getBeforeOpenParenCallSite.nnMap { x =>
            val indent = cfg.indent.getSignificant
            @tailrec
            def findLastCallArgs(t: Member.Apply): Member.ArgClause =
              t.parent match {
                case Some(p: Member.Apply) => findLastCallArgs(p)
                case _ => t.argClause
              }
            getSplitsBeforeOpenParen(x, indent, _.beforeOpenParenCallSite)(
              rightOwner.parent match {
                case Some(p: Member.Apply) => findLastCallArgs(p)
                case _ => null
              },
            )
          }
        else null
      beforeOpenParenSplits ?? Seq(baseNoSplit)
    } else Nil
  }
}

object SplitsBeforeLeftBracket extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] =
    if (!ft.rightOwner.parent.exists(_.begOffset < ft.right.start)) Nil
    else Seq(Split(Space(!ft.leftOwner.is[Mod.WithWithin]), 0))
}

object SplitsBeforeRightBracket extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = Seq(Split(Space(ft.left.is[T.Comment]), 0))
}

object SplitsBeforeRightParen extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import ft._
    def nlMod = Newline2x(ft)
    val mod = left match {
      case _: T.Comment => if (hasBreak || leftHasNewline) nlMod else Space
      case _: T.RightParen if rightOwner eq leftOwner => NoSplit
      case _ =>
        val nlOnly = cfg.newlines.keepBreak(hasBreak) &&
          cfg.binPack.siteFor(rightOwner)
            .fold(rightOwner.is[Pat.Alternative])(_._1 ne BinPack.Site.Never)
        if (nlOnly) nlMod else Space(cfg.spaces.inParentheses)
    }
    Seq(Split(mod, 0))
  }
}

//object SplitsBeforeLeftParen extends Splits {
//  override def get(implicit
//                   ft: FT,
//                   fo: FormatOps,
//                   cfg: ScalafmtConfig,
//                  ): Seq[Split] = { import fo._, tokens._, ft._ }
//}
//

object SplitsAfterTemplateKeyword extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, ft._
    leftOwner match {
      case lo: Stat.WithTemplate =>
        import lo._
        val policy = Policy ?
          (cfg.binPack.keepParentConstructors ||
            templ.begOffset == templ.endOffset) || {
            val expire = templateDerivesOrCurlyOrLastNonTrivial(templ)
            val forceNewlineBeforeExtends = Policy.beforeLeft(expire, "NLPCTOR") {
              case Decision(FT(_, soft.ExtendsOrDerives(), m), s)
                  if m.rightOwner eq templ =>
                s.filter(x => x.isNL && !x.isActiveFor(SplitTag.OnelineWithChain))
            }
            delayedBreakPolicyBefore(expire)(forceNewlineBeforeExtends)
          }
        Seq(Split(Space, 0).withPolicy(policy))
      case _ => Nil
    }
  }
}

object SplitsAfterLeftParenOrBracket {
  def get1(implicit ft: FT, fo: FormatOps, cfg: ScalafmtConfig): Seq[Split] = {
    import fo._, tokens._, ft._
    if (
      meta.formatOff && leftOwner.isAny[Member.SyntaxValuesClause, Member.Tuple]
    ) {
      val close = matchingLeft(ft)
      def splits(xft: FT, policy: Policy)(implicit l: FileLine) =
        Seq(Split(Provided(xft), 0, policy))
      val policy = Policy.onLeft(close, "(FMT:OFF)", rank = Int.MaxValue) {
        case Decision(xft, _) => splits(xft, NoPolicy)
      }
      splits(ft, policy)
    }
    // Parameter opening for one parameter group. This format works
    // on the WHOLE defnSite (via policies)
    else if (cfg.verticalMultiline.atDefnSite && isParamClauseSite(leftOwner))
      verticalMultiline()
    else null
  }

  def get2(
      isBracket: Boolean,
  )(implicit ft: FT, fo: FormatOps, cfg: ScalafmtConfig): Seq[Split] = {
    import ft._
    if (isArgClauseSite(leftOwner)) {
      val binPack = cfg.binPack.callSiteFor(left)
      if (binPack == BinPack.Site.Never) getNoBinPack(isBracket)
      else getArgSiteBinPack(isBracket, binPack)
    } else if (isParamClauseSite(leftOwner)) {
      val binPack = cfg.binPack.defnSiteFor(left)
      if (binPack == BinPack.Site.Never) getNoBinPack(isBracket)
      else getParamSiteBinPack(isBracket, binPack)
    } else null
  }

  def getArgSiteBinPack(isBracket: Boolean, binPack: BinPack.Site)(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._

    val close = matchingLeft(ft)
    val beforeClose = prev(close)
    val bracketPenalty = if (isBracket) Constants.BracketPenalty else 1

    val args = getArgs(leftOwner)
    val isSingleArg = isSeqSingle(args)
    val firstArg = if (args.isEmpty) null else args.head
    val singleArgAsInfix = if (isSingleArg) asInfixApp(firstArg) else null

    implicit val clauseSiteFlags: ClauseSiteFlags = ClauseSiteFlags
      .atCallSite(leftOwner)
    val flags = getBinpackCallSiteFlags(ft, beforeClose)
    val (nlOpen, nlCloseOnOpen) = flags.nlOpenClose()
    val singleLineOnly = cfg.binPack.literalsSingleLine && flags.literalArgList
    val nlOnly = nlOpen && !singleLineOnly

    val oneline = binPack.isOneline
    val afterFirstArgOneline =
      if (oneline && (firstArg ne null)) tokenAfter(firstArg) else null

    val (indentLen, bpIndentLen) = cfg.indent.getBinPackCallSites

    val exclude =
      if (!isBracket) insideBracesBlock(ft, close)
      else insideBlock[T.LeftBracket](ft, close)
    val sjsOneline = !isBracket && binPack == BinPack.Site.OnelineSjs
    val sjsExclude = exclude.getIf(sjsOneline)

    val newlinesPenalty = 3 + indentLen * bracketPenalty
    val penalizeNewlinesPolicy =
      policyWithExclude(exclude, Policy.End.BeforeLeft, Policy.End.OnLeft)(
        new PenalizeAllNewlines(newlinesPenalty) <=
          findToken(beforeClose, prev)(x => !RightParenOrBracket(x.left)),
      )

    val (onelineCurryToken, onelinePolicy) =
      if (afterFirstArgOneline eq null) (null, NoPolicy)
      else BinPackOneline.getPolicy(true, sjsExclude)(afterFirstArgOneline)

    def baseNoSplit(implicit fileLine: FileLine) =
      Split(Space(cfg.spaces.inParentheses), 0)
    val noNLPolicy = flags.noNLPolicy

    val noSplit =
      if (nlOnly) Split.ignored
      else if (singleLineOnly || noNLPolicy == null) baseNoSplit.withSingleLine(
        close,
        exclude = sjsExclude,
        noSyntaxNL = true,
        killOnFail = MaybeBool(!dangleCloseDelim || sjsExclude.isEmpty),
      )
      else {
        def noSingleArgIndents = oneline || (singleArgAsInfix ne null) ||
          !cfg.binPack.indentCallSiteSingleArg ||
          !isBracket && (getAssignAtSingleArgCallSite(args) ne null)
        val indent = Indent(bpIndentLen, close, Before)
        val noSplitIndents =
          if (isSingleArg && noSingleArgIndents) Nil
          else if (cfg.binPack.indentCallSiteOnce) {
            @tailrec
            def iter(tree: Tree): T = tree.parent match {
              case Some(p) =>
                if (isArgClauseSite(p)) getIndentTrigger(p)
                else if (p.isAny[Tree.Block, Tree.WithBody]) null
                else iter(p)
              case _ => null
            }
            val trigger = iter(leftOwner)
            Seq(if (trigger eq null) indent else Indent.before(indent, trigger))
          } else if (alignOpenDelim) getOpenParenAlignIndents(close)
          else Seq(indent)

        val nextComma =
          if (firstArg eq null) null
          else if (!oneline) tokens.findTokenEx(xft =>
            xft.right match {
              case close.left | _: T.RightBrace | _: T.RightArrow => null
              case _: T.Comma => Right(next(xft))
              case _: T.LeftBrace => Left(matchingRight(xft))
              case _ => Left(next(xft))
            },
          )(ft)
          else if (isSingleArg || (afterFirstArgOneline eq null)) null
          else next(afterFirstArgOneline)
        val opt = if (nextComma ne null) nextComma else getSlbEndOnLeft(close)

        val slbArg = oneline && !noSplitIndents.exists(_.hasStateColumn)
        val slbPolicy: Policy = Policy ? (slbArg && (nextComma ne null)) &&
          SingleLineBlock(nextComma, noSyntaxNL = true, exclude = sjsExclude)
        val noSplitPolicy = slbPolicy ==> onelinePolicy & penalizeNewlinesPolicy
        val indentPolicy = Policy ? noSplitIndents.isEmpty || {
          def unindentBraces = insideBlock(getEndOfBlock(_) {
            case FT(_: T.LeftBrace, _, _) => MaybeBool.True
            case FT(_: T.LeftParen, _, m) // don't unindent someone else's blocks
                if isArgClauseSite(m.leftOwner) &&
                  getArgs(m.leftOwner).lengthCompare(1) > 0 => MaybeBool.False
            case _ => MaybeBool.Maybe
          })(ft, close)
          def unindentPolicy = Policy ? (isSingleArg || sjsOneline) &&
            unindentAtExclude(bpIndentLen)(
              if (isBracket) exclude else unindentBraces,
            )
          def indentOncePolicy = Policy ? cfg.binPack.indentCallSiteOnce && {
            val trigger = getIndentTrigger(leftOwner)
            Policy.onLeft(close, prefix = "IND1") {
              case Decision(FT(LeftParenOrBracket(), _, m), s)
                  if isArgClauseSite(m.leftOwner) =>
                s.map(x => if (x.isNL) x else x.switch(trigger, false))
            }
          }
          unindentPolicy & indentOncePolicy
        }
        val optLite = cfg.newlines.keep && preferConfigStyle && !isSingleArg
        val kof = cfg.newlines.keep && preferConfigStyle && isSingleArg &&
          !dangleCloseDelim && (singleArgAsInfix eq null) && !flags.scalaJsStyle
        baseNoSplit
          .withOptimalToken(opt, recurseOnly = optLite, killOnFail = kof)
          .withPolicy(noSplitPolicy & indentPolicy & noNLPolicy())
          .withIndents(noSplitIndents)
      }

    def avoidNlClosedOnOpenNo = (onelineCurryToken ne null) && {
      if (onelineCurryToken.right.is[T.Dot]) onelinePolicy.isEmpty
      else !flags.scalaJsStyle
    }
    val nlClosedOnOpenEffective = nlCloseOnOpen match {
      case NlClosedOnOpen.No if avoidNlClosedOnOpenNo =>
        val forceCfg = preferConfigStyle && !flags.literalArgList
        if (forceCfg) NlClosedOnOpen.Cfg else NlClosedOnOpen.Yes
      case x => x
    }

    val nlPolicy: Policy = {
      def newlineBeforeClose(implicit fileLine: FileLine) =
        decideNewlinesOnlyBeforeClose(close)
      nlClosedOnOpenEffective match {
        case NlClosedOnOpen.No => onelinePolicy
        case NlClosedOnOpen.Cfg => splitOneArgOneLine(close, leftOwner) ==>
            newlineBeforeClose
        case _ => newlineBeforeClose & onelinePolicy
      }
    }

    val nlIndentLen =
      if (nlClosedOnOpenEffective eq NlClosedOnOpen.Cfg) indentLen
      else bpIndentLen
    val nlMod = {
      if (nlOnly && noBreak && right.is[T.Comment]) Space.toExt
      else Newline.withAltIf(singleLineOnly)(NoSplit)
    }.withIndent(nlIndentLen, close, Before)
    val nlSplit = Split(nlMod, bracketPenalty * (if (oneline) 4 else 2))
      .withSingleLineNoOptimal(close, ignore = !singleLineOnly)
      .andPolicy(Policy ? singleLineOnly || nlPolicy & penalizeNewlinesPolicy)
      .andPolicy(singleArgAsInfix &&& InfixSplits(singleArgAsInfix, ft).nlPolicy)
    Seq(noSplit, nlSplit)

  }

  def getParamSiteBinPack(isBracket: Boolean, binPack: BinPack.Site)(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._

    val close = matchingLeft(ft)
    val noSplitMod = Space(cfg.spaces.inParentheses)
    if (close.left eq right) Seq(Split(noSplitMod, 0))
    else {
      implicit val clauseSiteFlags: ClauseSiteFlags = ClauseSiteFlags
        .atDefnSite(leftOwner)

      val bracketPenalty = if (isBracket) Constants.BracketPenalty else 1
      val penalizeBrackets =
        if (isBracket) PenalizeAllNewlines(close, bracketPenalty + 3)
        else Policy.NoPolicy
      val firstArg = optimizationEntities.argument
      val nextComma = firstArg &&& {
        val ok = isSeqMulti(getArgs(leftOwner))
        if (ok) findFirstOnRight[T.Comma](getLast(firstArg), close) else null
      }
      val nextCommaOneline = if (binPack.isOneline) nextComma else null

      val flags = getBinpackDefnSiteFlags(ft, prev(close))
      val (nlOnly, nlCloseOnOpen) = flags.nlOpenClose()
      val noNLPolicy = flags.noNLPolicy
      val slbOrNL = nlOnly || noNLPolicy == null

      val rightIsComment = right.is[T.Comment]
      def getNoSplit(slbEnd: FT)(implicit fileLine: FileLine) = {
        val mod = if (rightIsComment) Space.orNL(noBreak) else noSplitMod
        if (slbEnd eq null) Split(mod, 0)
        else Split(mod, 0).withOptimalToken(slbEnd, killOnFail = true)
          .withPolicy(SingleLineBlock(slbEnd, okSLC = true, noSyntaxNL = true))
      }

      val noSplit =
        if (nlOnly) Split.ignored
        else if (slbOrNL) getNoSplit(close)
        else {
          val opensPolicy =
            if (isBracket) Policy.beforeLeft(close, "PENBP[") {
              case Decision(ftd @ FT(o: T.LeftBracket, _, m), s)
                  if isParamClauseSite(m.leftOwner) && styleMap.at(o)
                    .binPack.bracketDefnSite.exists(_ != BinPack.Site.Never) =>
                if (isRightCommentThenBreak(ftd)) s
                else s.penalizeNL(bracketPenalty)
            }
            else Policy.NoPolicy
          getNoSplit(nextComma.nnMap(getSlbEndOnLeft))
            .andPolicy((opensPolicy | penalizeBrackets) & noNLPolicy())
        }

      def nlCost = bracketPenalty
      val nlMod = Space.orNL(rightIsComment && nlOnly && noBreak)
      def getDanglePolicy(implicit fileLine: FileLine) =
        decideNewlinesOnlyBeforeClose(close)
      val nlPolicy = nlCloseOnOpen match {
        case NlClosedOnOpen.Cfg => splitOneArgOneLine(close, leftOwner) ==>
            getDanglePolicy
        case NlClosedOnOpen.Yes => getDanglePolicy
        case NlClosedOnOpen.No => NoPolicy
      }
      val nlOnelinePolicy =
        if (nextCommaOneline eq null) Policy.NoPolicy
        else splitOneArgPerLineAfterCommaOnBreak(next(nextCommaOneline))

      val (indentLen, bpIndentLen) = cfg.indent.getBinPackDefnSites(leftOwner)
      val nlIndentLen =
        if (nlCloseOnOpen eq NlClosedOnOpen.Cfg) indentLen else bpIndentLen

      def noSplitIndents =
        if (alignOpenDelim) getOpenParenAlignIndents(close)
        else Seq(Indent(bpIndentLen, close, Before))

      Seq(
        noSplit.withIndents(noSplitIndents),
        Split(nlMod, if (slbOrNL) 0 else nlCost)
          .withIndent(nlIndentLen, close, Before)
          .withPolicy(nlPolicy & penalizeBrackets & nlOnelinePolicy),
      )
    }
  }

  def getNoBinPack(
      isBracket: Boolean,
  )(implicit ft: FT, fo: FormatOps, cfg: ScalafmtConfig): Seq[Split] = {
    import fo._, tokens._, ft._
    val rightIsComment = right.is[T.Comment]
    val nft = if (rightIsComment) nextNonCommentSameLine(ft) else ft
    val rightIsCommentWithBreak = rightIsComment && ((nft eq ft) || nft.hasBreak)
    val afterOpen = nextNonCommentSameLineAfter(nft)
    val close = matchingLeft(ft)
    val beforeClose = prev(close)
    val tupleSite = leftOwner.is[Member.Tuple]
    val anyDefnSite = isParamClauseSite(leftOwner)
    val defnSite = !tupleSite && anyDefnSite

    val args = getArgs(leftOwner)

    // In long sequence of select/apply, we penalize splitting on
    // parens furthest to the right.
    def leftOwnerIsEnclosed = leftOwner.is[Member.Function]

    val lhsPenalty = leftOwner match {
      case t: Member.SyntaxValuesClause => t.parent match {
          case Some(p: Init) => treeDepth(p.tpe)
          case Some(p: Member.Apply) => treeDepth(p.fun)
          case Some(_: Member.Function) => maxTreeDepth(t.values)
          case _ => 0
        }
      case t => treeDepth(t)
    }

    // XXX: sometimes we have zero args, so multipleArgs != !singleArgument
    var onlyArgument: Tree = null
    var enclosedOnlyArgumentHead: FT = null
    var multipleArgs = false
    var notTooManyArgs = false
    args match {
      case arg :: Nil =>
        onlyArgument = TreeOps.getBlockStat(arg)
        enclosedOnlyArgumentHead = getHeadIfEnclosed(onlyArgument)
      case _ :: rest =>
        multipleArgs = true
        notTooManyArgs = rest.lengthCompare(100) < 0
      case _ =>
    }

    val bracketCoef = if (isBracket) Constants.BracketPenalty else 1
    val mustDangleForTrailingCommas = getMustDangleForTrailingCommas(beforeClose)

    implicit val clauseSiteFlags: ClauseSiteFlags =
      ClauseSiteFlags(leftOwner, defnSite)
    implicit val configStyleFlags: Newlines.ConfigStyleElement =
      clauseSiteFlags.configStyle
    val closeBreak = beforeClose.hasBreak
    val forceConfigStyle = mustForceConfigStyle(ft)
    val onlyConfigStyle = forceConfigStyle ||
      preserveConfigStyle(ft, mustDangleForTrailingCommas || closeBreak)
    val configStyleFlag = configStyleFlags.prefer

    val sourceIgnored = cfg.newlines.sourceIgnored
    val nestedPenalty = 1 + nestedApplies(leftOwner) + lhsPenalty

    val indentLen =
      if (anyDefnSite) cfg.indent.getDefnSite(leftOwner)
      else cfg.indent.callSite
    val indent = Indent(indentLen, close, ExpiresOn.Before)

    val isBeforeOpenParen =
      if (defnSite) cfg.newlines.isBeforeOpenParenDefnSite
      else cfg.newlines.isBeforeOpenParenCallSite
    val optimalOpt =
      if (isBeforeOpenParen || !defnSite || isBracket) null
      else defnSiteOptimalToken(leftOwner)
    val optimal: FT = getSlbEndOnLeft(optimalOpt ?? close)

    val wouldDangle = onlyConfigStyle || mustDangleForTrailingCommas ||
      dangleCloseDelim || closeBreak && beforeClose.left.is[T.Comment]
    val optimalIsComment = optimal.left.is[T.Comment]

    val newlinePolicy: Policy = Policy ? (wouldDangle || optimalIsComment) &&
      decideNewlinesOnlyBeforeClose(Split(Newline, 0, rank = -1))(close)

    // covers using as well
    val handleImplicit = !(tupleSite || rightIsComment && nft.hasBreak) &&
      (if (onlyConfigStyle) opensConfigStyleImplicitParamList(ft)
       else hasImplicitParamList(nft.rightOwner))

    val align = !rightIsComment && alignOpenDelim &&
      (!handleImplicit || cfg.newlines.forceAfterImplicitParamListModifier)
    val alignTuple = align && tupleSite && !onlyConfigStyle

    val noSplitForNL = !onlyConfigStyle && right.is[T.LeftBrace]
    val skipNoSplit = rightIsCommentWithBreak ||
      !noSplitForNL && !alignTuple &&
      (cfg.newlines.keepBreak(hasBreak) || {
        if (!handleImplicit) onlyConfigStyle
        else cfg.newlines.forceBeforeImplicitParamListModifier
      })
    val noSplitMod =
      if (skipNoSplit) null
      else getNoSplitAfterOpening(ft, commentNL = null, spaceOk = !isBracket)

    val noSplitsForAssign = rightIsCommentWithBreak || defnSite || isBracket ||
      !sourceIgnored && configStyleFlag && hasBreak
    val splitsForAssign =
      if (noSplitsForAssign) Seq.empty
      else {
        val assign = getAssignAtSingleArgCallSite(args)
        if (assign eq null) Seq.empty
        else {
          val breakToken = nextNonCommentSameLine(assign.rhs match {
            case b: Term.Block if isEnclosedInBraces(b) => getHead(b)
            case x => tokenBefore(x)
          })
          val newlineAfterAssignDecision = Policy ? newlinePolicy.isEmpty ||
            decideNewlinesOnlyAfterToken(breakToken)
          Seq(
            Split(Newline, nestedPenalty + Constants.ExceedColumnPenalty)
              .withPolicy(newlinePolicy).withIndent(indent),
            Split(NoSplit, nestedPenalty).withSingleLineNoOptimal(breakToken)
              .andPolicy(newlinePolicy & newlineAfterAssignDecision),
          )
        }
      }

    val excludeBlocks = cfg.newlines.source match {
      case _ if isBracket =>
        val excludeBeg = if (align) getHead(args.last) else ft
        insideBlock[T.LeftBracket](excludeBeg, close)
      case _ if multipleArgs => TokenRanges.empty
      case Newlines.unfold
          if leftOwnerIsEnclosed || (enclosedOnlyArgumentHead eq null) ||
            (enclosedOnlyArgumentHead eq ft) => TokenRanges.empty
      case Newlines.fold if (onlyArgument match {
            case _: Tree.WithCond | _: Term.TryClause | _: Term.ForClause =>
              enclosedOnlyArgumentHead eq null
            case _ => false
          }) => TokenRanges.empty
      case Newlines.fold if (onlyArgument ne null) && {
            (enclosedOnlyArgumentHead ne null) ||
            isTreeEndingInArgumentClause(onlyArgument)
          } =>
        if (onlyArgument eq leftOwner) TokenRanges(TokenRange(ft, close))
        else parensTuple(onlyArgument)
      case _ => insideBracesBlock(ft, close)
    }

    def singleLine(newlinePenalty: Int)(implicit fileLine: FileLine): Policy =
      if (multipleArgs && (isBracket || excludeBlocks.isEmpty))
        SingleLineBlock(close, noSyntaxNL = true)
      else if (isBracket)
        PenalizeAllNewlines(close, newlinePenalty, penalizeLambdas = false)
      else PenalizeAllNewlines(
        close,
        penalty =
          if (!multipleArgs) newlinePenalty else Constants.ShouldBeNewline,
        penalizeLambdas = multipleArgs,
        noSyntaxNL = multipleArgs,
        exclude = excludeBlocks,
      )

    val preferNoSplit = !skipNoSplit && (onlyArgument ne null) &&
      cfg.newlines.keepBreak(noBreak)
    val oneArgOneLine = newlinePolicy &
      (leftOwner match {
        case t @ (_: Member.SyntaxValuesClause | _: Member.Tuple) =>
          splitOneArgOneLine(close, t)
        case _ => Policy.NoPolicy
      })
    val extraOneArgPerLineIndent =
      if (multipleArgs && cfg.newlines.configStyle.beforeComma)
        Indent(2, afterOpen, After)
      else Indent.Empty
    val (implicitPenalty, implicitPolicy) =
      if (!handleImplicit) (2, Policy.NoPolicy)
      else (0, decideNewlinesOnlyAfterToken(afterOpen))

    val splitsNoNL =
      if (noSplitMod == null) Nil
      else if (onlyConfigStyle) Seq(
        Split(noSplitMod, 0).withPolicy(oneArgOneLine & implicitPolicy)
          .withOptimalToken(afterOpen, killOnFail = true)
          .withIndents(extraOneArgPerLineIndent, indent),
      )
      else {
        val useOneArgPerLineSplit = notTooManyArgs && align ||
          handleImplicit &&
          (cfg.newlines.notBeforeImplicitParamListModifier ||
            afterOpen.hasBreak && // prefer before, but must break after
            (afterOpen.left.is[T.Comment] || afterOpen.right.is[T.Comment]))
        val slbSplit =
          if (mustDangleForTrailingCommas) Split.ignored
          else {
            def getSlb(implicit l: FileLine) = SingleLineBlock(
              close,
              exclude = excludeBlocks.excludeCloseDelim,
              noSyntaxNL = multipleArgs,
            )

            val needDifferentFromOneArgPerLine = newlinePolicy.nonEmpty &&
              handleImplicit && useOneArgPerLineSplit
            val noSplitPolicy =
              if (needDifferentFromOneArgPerLine) getSlb
              else if (preferNoSplit && splitsForAssign.isEmpty) singleLine(2)
              else if (
                wouldDangle || optimalIsComment && isBracket ||
                sourceIgnored && configStyleFlag &&
                (enclosedOnlyArgumentHead eq null)
              ) getSlb
              else if (splitsForAssign.nonEmpty) singleLine(3)
              else singleLine(10)
            val kof = (cfg.newlines.keep || excludeBlocks.isEmpty) &&
              needDifferentFromOneArgPerLine
            val noOptimal = cfg.newlines.keep && !useOneArgPerLineSplit ||
              (onlyArgument ne null) && !excludeBlocks.isEmpty &&
              excludeBlocks.ranges.forall(_.lt.left.is[T.LeftParen])
            val okIndent = rightIsComment || handleImplicit
            Split(noSplitMod, 0, noSplitPolicy)
              .withOptimalToken(optimal, ignore = noOptimal, killOnFail = kof)
              .withIndent(indent, ignore = !okIndent)
          }
        val oneArgPerLineSplit =
          if (useOneArgPerLineSplit)
            Split(noSplitMod, (implicitPenalty + lhsPenalty) * bracketCoef)
              .withPolicy(oneArgOneLine & implicitPolicy).withIndents(
                if (align) getOpenParenAlignIndents(close) else Seq(indent),
              )
          else Split.ignored
        Seq(slbSplit, oneArgPerLineSplit)
      }

    val splitsNL =
      if (
        splitsNoNL.nonEmpty &&
        (alignTuple ||
          !(onlyConfigStyle || multipleArgs || splitsForAssign.isEmpty))
      ) Nil
      else {
        val cost =
          if (forceConfigStyle || rightIsCommentWithBreak)
            if (splitsNoNL.isEmpty) 0 else 1
          else (if (preferNoSplit) Constants.ExceedColumnPenalty else 0) +
            bracketCoef * (nestedPenalty + (if (multipleArgs) 2 else 0))
        val split =
          if (multipleArgs) Split(Newline, cost, oneArgOneLine)
            .withIndent(extraOneArgPerLineIndent)
          else {
            val noConfigStyle = noSplitForNL || newlinePolicy.isEmpty ||
              !configStyleFlag
            val policy =
              if (!noSplitForNL) newlinePolicy
              else decideNewlinesOnlyBeforeToken(matchingRight(ft))
            val infix = asInfixApp(onlyArgument)
            Split(NoSplit.orNL(noSplitForNL), cost, policy)
              .andPolicy(Policy ? noConfigStyle && singleLine(4))
              .andPolicy(infix &&& InfixSplits(infix, ft).nlPolicy)
          }
        Seq(split.withIndent(indent, ignore = !split.isNL))
      }

    splitsNoNL ++ splitsNL ++ splitsForAssign
  }
}

object SplitsAfterLeftParen extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    var splits = SplitsAfterLeftParenOrBracket.get1
    if (splits ne null) return splits
    splits = getLambdaAsSingleArg
    if (splits ne null) return splits
    splits = get1
    if (splits ne null) return splits
    splits = SplitsAfterLeftParenOrBracket.get2(isBracket = false)
    if (splits ne null) return splits
    get2
  }

  private def get1(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    right match {
      case _: T.RightParen =>
        val noNL = cfg.newlines.sourceIgnored || noBreak
        Seq(Split(NoSplit.orNL(noNL), 0))
      case _: T.Comment if (leftOwner match {
            case _: Lit.Unit => true
            case t: Member.SyntaxValuesClause => t.values.isEmpty
            case _ => false
          }) && !hasBreakBeforeNonComment(ft) => Seq(Split(Space, 0))
      case _ => null
    }
  }

  private def get2(implicit ft: FT, fo: FormatOps, cfg: ScalafmtConfig) =
    getIfForWhile ??
      (ft.right match {
        case _: T.LeftBrace => Seq(Split(NoSplit, 0))
        case _ => getRest
      })

  private def getIfForWhile(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ) = {
    import fo._, tokens._, ft._
    def impl(enclosedInBraces: => Boolean) = {
      val close = matchingLeft(ft)
      val indentLen = cfg.indent.ctrlSite.getOrElse(cfg.indent.callSite)
      val indent = Indent(indentLen, close, ExpiresOn.Before)
      val noAlign = !cfg.align.openParenCtrlSite
      def alignIndents = getOpenParenAlignIndents(close)
      val penalizeNewlines = penalizeNewlineByNesting(ft, close)
      def baseNoSplit(policy: Policy, commentNL: Modification = null)(implicit
          fileLine: FileLine,
      ) = Split.opt(getNoSplitAfterOpening(ft, commentNL = commentNL), 0, policy)
      if (cfg.danglingParentheses.ctrlSite) {
        def noSplitPolicy = penalizeNewlines &
          decideNewlinesOnlyBeforeCloseOnBreak(close)
        val noSplit =
          if (noAlign) baseNoSplit(Policy.NoPolicy)
            .withSingleLine(close, ignore = enclosedInBraces)
          else baseNoSplit(noSplitPolicy).withIndents(alignIndents)
        val nlPolicy = penalizeNewlines & decideNewlinesOnlyBeforeClose(close)
        Seq(noSplit, Split(Newline, 1, nlPolicy).withIndent(indent))
      } else Seq(baseNoSplit(penalizeNewlines, Newline).withIndents(
        if (noAlign) if (enclosedInBraces) Seq.empty else Seq(indent)
        else alignIndents,
      ))
    }
    leftOwner match { // If/For/While/For with (
      case t: Term.EnumeratorsBlock if getHead(t) eq ft => impl(false)
      case t: Tree.WithCond if !isTokenHeadOrBefore(left, t) =>
        impl(isEnclosedInBraces(t.cond))
      case _ => null
    }
  }

  private def getRest(implicit ft: FT, fo: FormatOps, cfg: ScalafmtConfig) = {
    import fo._, tokens._, ft._
    val close = matchingLeft(ft)
    val beforeClose = prev(close)
    implicit val clauseSiteFlags: ClauseSiteFlags = ClauseSiteFlags
      .atCallSite(leftOwner)
    val isConfig = couldPreserveConfigStyle(ft, beforeClose.hasBreak)

    val enclosed = findEnclosedBetweenParens(left, close.left, leftOwner)
    def spaceSplitWithoutPolicy(implicit fileLine: FileLine) = {
      val indent: Length = right match {
        case _: T.KwIf => StateColumn
        case _: T.KwFor if !cfg.indent.yieldKeyword => StateColumn
        case _ =>
          val needIndent =
            (enclosed match {
              case _: Term.ApplyInfix | _: Term.NewAnonymous => false
              case Term.ArgClause((_: Term.ApplyInfix) :: Nil, _) => false
              case _ => true
            }) && {
              val pft = prevNonCommentSameLine(beforeClose)
              (pft eq beforeClose) && beforeClose.left.is[T.Comment]
            }
          if (needIndent) cfg.indent.main else 0
      }
      Split.opt(getNoSplitAfterOpening(ft, commentNL = null), 0)
        .withIndent(indent, close, Before)
    }
    def spaceSplit(implicit fileLine: FileLine) = spaceSplitWithoutPolicy
      .withPolicy(PenalizeAllNewlines(close, 1))
    def newlineSplit(cost: Int, forceDangle: Boolean)(implicit
        fileLine: FileLine,
    ) = {
      val shouldDangle = forceDangle || dangleCloseDelim
      Split(Newline, cost)
        .withPolicy(decideNewlinesOnlyBeforeClose(close), !shouldDangle)
        .withIndent(cfg.indent.callSite, close, Before)
    }
    if (isRightCommentThenBreak(ft)) Seq(newlineSplit(0, isConfig))
    else cfg.newlines.source match {
      case Newlines.classic =>
        Seq(if (isConfig) newlineSplit(0, true) else spaceSplit)
      case Newlines.keep =>
        Seq(if (hasBreak) newlineSplit(0, isConfig) else spaceSplit)
      case _ =>
        val singleLine = (enclosed eq null) || cfg.newlines.unfold
        Seq(
          if (!singleLine) spaceSplit
          else spaceSplitWithoutPolicy.withSingleLine(close).andPolicy(
            if ((enclosed eq null) || !isInfixApp(enclosed)) null
            else InfixSplits.getSingleLineInfixPolicy(close),
          ),
          newlineSplit(10, forceDangle = true),
        )
    }
  }

  @tailrec
  private def getSingleFunctionArg(
      values: List[Tree],
  )(implicit ftoks: FormatTokens): Term.FunctionLike = values match {
    case (t: Term.FunctionLike) :: Nil => t
    case (t: Term.Block) :: Nil if !ftoks.isEnclosedInBraces(t) =>
      getSingleFunctionArg(t.stats)
    case _ => null
  }

  private def getLambdaAsSingleArg(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    val lambda = leftOwner match {
      case Term.ArgClause(v, None) => getSingleFunctionArg(v)
      case _ => null
    }
    if (lambda eq null) return null

    val close = matchingLeft(ft)
    val beforeClose = prev(close)
    val dangle = cfg.danglingParentheses.callSite
    val newlinePolicy = Policy ? dangle && decideNewlinesOnlyBeforeClose(close)
    val beforeParenLambdaParams = cfg.newlines.getBeforeParenLambdaParams
    val noSplitMod =
      if (
        (beforeParenLambdaParams eq Newlines.BeforeCurlyLambdaParams.always) ||
        getMustDangleForTrailingCommas(beforeClose)
      ) null
      else getNoSplitAfterOpening(ft, commentNL = null)

    def multilineSpaceSplit(implicit fileLine: FileLine): Split = {
      val lambdaLeft: FT = functionExpire(lambda)._1 match {
        case x @ FT(_: T.RightBrace, _, _) => matchingLeftOrNull(x)
        case _ => null
      }

      val arrowFt = getFuncArrow(lambda)
      val lambdaIsABlock = lambdaLeft.nnHas(_.left eq arrowFt.right)
      val lambdaToken =
        nextNonCommentSameLine(if (lambdaIsABlock) next(arrowFt) else arrowFt)

      val spacePolicy = SingleLineBlock(lambdaToken) ==> {
        def before = Policy.onlyFor(beforeClose, "NODANGLE") { _ =>
          val bc = beforeClose.left
          if (bc.is[T.Comment])
            if (bc.text.startsWith("//")) Nil else Seq(Split(Space, 0))
          else Seq(Split(Space(cfg.spaces.inParentheses), 0))
        }
        Policy ? lambdaIsABlock || penalizeOneNewline(lambdaToken, 1) & {
          val exp = Policy.End <= lambdaLeft ?? close
          Policy.RelayOnSplit.by(exp)((s, _) => s.isNL)(before)(newlinePolicy)
        }
      }
      Split(noSplitMod, 0, spacePolicy)
        .withOptimalToken(lambdaToken, killOnFail = true)
    }

    val indentLen =
      if (dangle || cfg.binPack.callSite == BinPack.Site.Never)
        cfg.indent.callSite
      else cfg.indent.getBinPackCallSite

    if (noSplitMod == null)
      Seq(Split(Newline, 0, newlinePolicy).withIndent(indentLen, close, Before))
    else {
      val newlinePenalty = 3 + nestedApplies(leftOwner)
      val noMultiline = beforeParenLambdaParams eq
        Newlines.BeforeCurlyLambdaParams.multiline
      Seq(
        if (noMultiline) Split(noSplitMod, 0).withSingleLine(close)
        else multilineSpaceSplit,
        Split(Newline, newlinePenalty, newlinePolicy)
          .withIndent(indentLen, close, Before),
      )
    }
  }
}

object SplitsAfterLeftBracket extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    var splits = SplitsAfterLeftParenOrBracket.get1
    if (splits ne null) return splits
    splits = SplitsAfterLeftParenOrBracket.get2(isBracket = true)
    if (splits ne null) return splits
    Seq(Split(NoSplit, 0))
  }
}

object SplitsAfterMatch extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    if (!leftOwner.is[Term.Name]) { // exclude end marker
      val indentLen = cfg.indent.matchSite.fold(0)(_ - cfg.indent.main)
      def expire = getLastNonTrivial(leftOwner) // should be rbrace
      Seq(Split(Space, 0).withIndent(indentLen, expire, ExpiresOn.Before))
    } else Nil
  }
}

object SplitsBeforeMatch extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    // do not split `.match`
    val noSplit = ft.left.is[T.Dot] && fo.dialect.allowMatchAsOperator
    Seq(Split(Space(!noSplit), 0))
  }
}

object SplitsAfterColon extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    leftOwner match {
      case ColonDeclType(returnType) if cfg.newlines.avoidInResultType =>
        val expire = getLastNonTrivial(returnType)
        val policy = PenalizeAllNewlines(expire, Constants.ShouldBeNewline)
        Seq(
          Split(Space, 0).withPolicy(policy)
            .withOptimalToken(expire, killOnFail = false),
        )
      case x: Pat.Typed if (cfg.spaces.afterColonInMatchPattern match {
            case Spaces.AfterColonInMatchPattern.Never => true
            case Spaces.AfterColonInMatchPattern.Always => false
            case Spaces.AfterColonInMatchPattern.NoAlternatives => x.parent
                .is[Pat.Alternative]
          }) => Seq(Split(NoSplit, 0))
      case _ => Seq(Split(Space, 0))
    }
  }
}

object SplitsBeforeColon extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    rightOwner match {
      case tp: Type.Bounds =>
        def noNLMod = Space(cfg.spaces.beforeContextBoundColon(tp))
        getSplitsForTypeBounds(noNLMod, tp, tp.context)
      case ColonDeclType(returnType) // Closing def site ): ReturnType
          if cfg.newlines.sometimesBeforeColonInMethodReturnType ||
            left.is[T.Comment] && hasBreak =>
        val expire = getLastNonTrivial(returnType)
        val sameLineSplit = Space(endsWithSymbolIdent(left))
        cfg.newlines.getBeforeOpenParenDefnSite.nnMap { x =>
          val ob = OptionalBraces.at(nextAfterNonComment(expire))
          def extraIfBody = cfg.indent.extraBeforeOpenParenDefnSite
          val indent =
            if (ob) cfg.indent.getSignificant + extraIfBody
            else cfg.indent.main +
              (if (defDefBody(rightOwner) eq null) 0 else extraIfBody)
          Seq(
            Split(sameLineSplit, 0).onlyIf(noBreak || x.ne(Newlines.keep))
              .withSingleLine(expire),
            Split(Newline, 1).withIndent(indent, expire, After),
          )
        } ?? {
          def penalizeNewlines(extra: Int)(implicit fileLine: FileLine) =
            PenalizeAllNewlines(expire, Constants.BracketPenalty + extra)
          val indent = cfg.indent.getDefnSite(leftOwner)
          if (cfg.newlines.keepBreak(hasBreak)) Seq(
            Split(Newline, 1).withIndent(indent, expire, After)
              .withPolicy(penalizeNewlines(1)),
          )
          else {
            val nlPenalty = 2 + treeDepth(returnType)
            Seq(
              Split(sameLineSplit, 0).withPolicy(penalizeNewlines(0)),
              // Spark style guide allows this:
              // https://github.com/databricks/scala-style-guide#indent
              Split(Newline, Constants.SparkColonNewline + nlPenalty)
                .withIndent(indent, expire, After)
                .withPolicy(penalizeNewlines(nlPenalty)),
            )
          }
        }
      case _ =>
        val mod = left match {
          case ident: T.Ident => identModification(ident)
          case _ => NoSplit
        }
        Seq(Split(mod, 0))
    }
  }
}

object SplitsAfterReturnLowPriority extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import ft._
    val mod =
      if (hasBlankLine) Newline2x
      else leftOwner match {
        case Term.Return(unit: Lit.Unit) if unit.tokens.isEmpty =>
          if (right.is[T.RightParen]) Space(cfg.spaces.inParentheses)
          else Newline //  force blank line for Unit "return".
        case _ => Space
      }
    Seq(Split(mod, 0))
  }
}

object SplitsAfterComma extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    if (right.is[T.LeftBrace]) SplitsBeforeLeftBrace.get
    else if (right.is[T.Comment]) {
      val nextFt = next(ft)
      if (hasBlankLine) Seq(Split(Newline2x, 0))
      else if (nextFt.hasBreak || meta.right.hasNL)
        Seq(Split(Space.orNL(newlinesBetween), 0))
      else if (noBreak) {
        val endFt = nextNonCommentSameLine(nextFt)
        val useSpaceOnly = endFt.hasBreak ||
          rightIsCloseDelimToAddTrailingComma(left, endFt)
        Seq(Split(Space, 0), Split(useSpaceOnly, 1)(Newline))
      } else if (
        !cfg.comments.willWrap &&
        rightIsCloseDelimToAddTrailingComma(left, nextNonComment(nextFt))
      ) Seq(Split(Space, 0), Split(Newline, 1))
      else Seq(Split(Newline, 0))
    } else if (leftOwner.is[Template]) { // trait A extends B, C, D, E
      val templ = leftOwner.asInstanceOf[Template]
      typeTemplateSplits(templ, cfg.indent.commaSiteRelativeToExtends)
    } else {
      val nlMod = Newline2x(ft)

      def forBinPack(binPack: BinPack.Site, callSite: Boolean): Seq[Split] = {
        if (binPack eq BinPack.Site.Never) null
        else optimizationEntities.argument
      }.nnMap { nextArg =>
        val lastFT = getLast(nextArg)
        val oneline = binPack.isOneline
        val afterNextArg = nextNonComment(lastFT)
        val nextCommaOrParen = afterNextArg.right match {
          case _: T.Comma | _: T.RightParen | _: T.RightBracket => afterNextArg
          case _ => null
        }
        def getEndOfResultType(cp: FT) =
          if (
            cp.right.is[T.Comma] || !cfg.newlines.avoidInResultType ||
            cfg.newlines.sometimesBeforeColonInMethodReturnType
          ) null
          else {
            val nft = findToken(next(cp), next)(x =>
              x.right match {
                case _: T.Comment => x.hasBreak
                case _: T.RightParen | _: T.RightBracket => false
                case _ => true
              },
            )
            if (!nft.right.is[T.Colon]) null // could be empty tree
            else colonDeclType(nft.meta.rightOwner).nnMap(getLastNonTrivial)
          }

        val noSpace = nlMod.isDouble || cfg.newlines.keepBreak(hasBreak)
        val onelineWithNextCommaOrParen = oneline && (nextCommaOrParen ne null)
        val noSjsExclude = (binPack ne BinPack.Site.OnelineSjs) ||
          noSpace && !onelineWithNextCommaOrParen
        val sjsExclude =
          if (noSjsExclude) TokenRanges.empty else insideBracesBlock(ft, lastFT)
        val onelinePolicy = Policy ? onelineWithNextCommaOrParen &&
          BinPackOneline.getPolicy(callSite, sjsExclude)(nextCommaOrParen)._2

        val indentOncePolicy = Policy ?
          (callSite && cfg.binPack.indentCallSiteOnce) && {
            val trigger = getIndentTrigger(leftOwner)
            Policy.onLeft(lastFT, prefix = "IND1") {
              case Decision(FT(LeftParenOrBracket(), _, m), s)
                  if isArgClauseSite(m.leftOwner) =>
                s.map(x => if (x.isNL) x else x.switch(trigger, true))
            }
          }
        val nlSplit = Split(nlMod, 1, onelinePolicy & indentOncePolicy)
        val noSplit =
          if (noSpace) Split.ignored
          else {
            val end = getSlbEndOnLeft(
              nextCommaOrParen.nnMap(getEndOfResultType) ?? lastFT,
            )
            val slbPolicy =
              SingleLineBlock(end, exclude = sjsExclude, noSyntaxNL = true)
            Split(Space, 0, slbPolicy ==> onelinePolicy)
              .withOptimalToken(end, killOnFail = sjsExclude.isEmpty)
          }
        Seq(noSplit, nlSplit)
      }

      def defaultSplits(indent: Length, allowKeepNL: Boolean = true)(implicit
          fileLine: FileLine,
      ) = {
        val noSpace = nlMod.isDouble ||
          cfg.newlines.keepBreak(allowKeepNL && hasBreak)
        Seq(
          Split(noSpace, 0)(Space),
          Split(nlMod, 1, rank = 1).withIndent(indent, next(ft), ExpiresOn.After),
        )
      }

      def altSplits = leftOwner match {
        case _: Defn.Val | _: Defn.Var =>
          defaultSplits(cfg.indent.getDefnSite(leftOwner))
        case _: Defn.RepeatedEnumCase
            if cfg.newlines.unfold || !cfg.newlines.fold && hasBreak =>
          Seq(Split(Newline, 0))
        case _: ImportExportStat => defaultSplits(cfg.indent.main)
        case _: Importer =>
          defaultSplits(0, cfg.importSelectorsBinPack eq ImportSelectors.keep)
        case _ => defaultSplits(0)
      }

      cfg.binPack.siteFor(leftOwner).fold(altSplits) { case (bp, isCallSite) =>
        forBinPack(bp, isCallSite) ?? defaultSplits(0)
      }
    }
  }
}

object SplitsBeforeComma extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import ft._
    Seq(Split(NoSplit.orNL(!left.is[T.Comment] || noBreak), 0))
  }
}

object SplitsBeforeViewbound extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, ft._
    rightOwner match {
      case tb: Type.Bounds => getSplitsForTypeBounds(Space, tb, tb.view)
      case _ => Nil
    }
  }
}

/* Type bounds in type definitions and declarations such as:
 * type `Tuple <: Alpha & Beta = Another` or `Tuple <: Alpha & Beta`
 */

object SplitsBeforeSubtype extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    rightOwner match {
      case tb: Type.Bounds =>
        val boundEnd = tb.hi match {
          case Some(t) => getLastNonTrivial(t)
          case None => null
        }
        getSplitsForTypeBounds(Space, tb, boundEnd)
      case _ => Nil
    }
  }
}

object SplitsBeforeSupertype extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    rightOwner match {
      case tb: Type.Bounds =>
        val boundEnd = tb.lo match {
          case Some(t) => getLastNonTrivial(t)
          case None => null
        }
        getSplitsForTypeBounds(Space, tb, boundEnd)
      case _ => Nil
    }
  }
}

object SplitsBeforeDot extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = Select.onRightOrNull(ft)(fo.tokens) match {
    case x
        if (x ne null) &&
          (cfg.newlines.keep || !x.tree.is[Term.Select] ||
            (findTreeWithParent(x.tree) {
              case _: Term.ArgClause => MaybeBool.Maybe
              case _: Type.Select | _: Importer | _: Pkg => MaybeBool.True
              case _: Term.Select | _: Member.Apply => MaybeBool.Maybe
              case _ => MaybeBool.False
            } eq null)) => getSelect(x)
    case _ => Seq(Split(NoSplit, 0))
  }

  private def getSelect(
      thisSelect: Select,
  )(implicit ft: FT, fo: FormatOps, cfg: ScalafmtConfig): Seq[Split] = {
    import fo._, tokens._, ft._
    val enclosed = cfg.newlines.encloseSelectChains
    val (expireTree, nextSelect) =
      findLastApplyAndNextSelect(rightOwner, enclosed)
    val (prevSelect, prevApply) = Select.prevAndApply(thisSelect.qual, enclosed)
    val afterComment = left.is[T.Comment]

    // includes other optional-braces-like trees
    def checkFewerBraces(tree: Tree) =
      (tree match {
        case p: Term.Apply => isFewerBraces(p)
        case p: Term.MatchLike => getHead(p.casesBlock).leftOwner ne
            p.casesBlock
        case p: Term.NewAnonymous => getHead(p.templ.body)
            .nnHas(_.left.is[T.Colon])
        case _: Term.ForClause | _: Term.ApplyInfix | _: Term.SelectPostfix =>
          true
        case _ => false
      }) && !isEnclosedInMatching(tree)
    val fewerBracesLike = checkFewerBraces(thisSelect.qual)
    val indentFewerBraces = cfg.getFewerBraces()
    val noIndentFewerBraces =
      if (nextSelect eq null) indentFewerBraces != Indents.FewerBraces.always
      else indentFewerBraces == Indents.FewerBraces.never

    val nlOnly = fewerBracesLike ||
      cfg.newlines.sourceIgnored && afterComment && hasBreak ||
      (prevApply ne null) && getHead(prevApply.argClause).left.is[T.Colon]
    val expire = getLastExceptParen(expireTree)
    val indentLen = cfg.indent.main

    val nextDotOpt = nextSelect &&& tokenBefore(nextSelect.nameFt)
    val beforeNextDotOpt = nextDotOpt.nnMap(prev)
    val nextFewerBraces = nextSelect match {
      case null => if (checkFewerBraces(expireTree)) expire else null
      case ns => if (checkFewerBraces(ns.qual)) nextDotOpt else null
    }

    def forcedBreakOnNextDotPolicy(implicit fileLine: FileLine) = Policy ?
      (beforeNextDotOpt eq null) ||
      decideNewlinesOnlyAfterToken(beforeNextDotOpt)
    def getClassicNonFirstBreakOnDot(dot: FT): Policy = Policy
      .onlyFor(dot, "NEXTSEL2NL") { s =>
        val filtered = s.flatMap { x =>
          val y = x.activateFor(SplitTag.SelectChainSecondNL)
          if (y.isActive) Some(y) else None
        }
        if (filtered.isEmpty) Nil
        else {
          val minCost = math.max(0, filtered.map(_.costWithPenalty).min - 1)
          filtered.map { x =>
            val p = x.policy.filter(!_.isInstanceOf[PenalizeAllNewlines])
            implicit val fileLine = x.fileLineStack.fileLineHead
            x.copy(penalty = x.costWithPenalty - minCost, policy = p)
          }
        }
      }
    def classicNonFirstBreakOnNextDot: Policy = Policy ?
      (beforeNextDotOpt ne null) &&
      getClassicNonFirstBreakOnDot(beforeNextDotOpt)

    def getSlbEnd() = {
      val nft = nextNonCommentSameLineAfter(ft)
      val eft = if (nft.noBreak) nextNonCommentSameLineAfter(nft) else nft
      getSlbEndOnLeft(eft)
    }

    val ftAfterRight = tokens(ft, 2)
    def modSpace = Space(afterComment)
    var spaceIsDelayedNL = false
    val baseSplits = cfg.newlines.getSelectChains match {
      case Newlines.classic =>
        def getNlMod = {
          val endSelect = nextFewerBraces match {
            case null =>
              if (nextSelect eq null) expire
              else getLastNonTrivial(nextSelect.qual)
            case x => if (noIndentFewerBraces) x else null
          }
          val altIndent =
            if (endSelect eq null) Indent.empty
            else Indent(-indentLen, endSelect, After)
          Newline.withAlt(modSpace.withIndent(altIndent))
        }

        val prevChain = inSelectChain(prevSelect, thisSelect, expireTree)
        def splitSecondNL(modNoBreaks: ModExt)(implicit fileLine: FileLine) =
          Split(!prevChain, 1)(
            if (!cfg.newlines.selectChains.classicKeepAfterFirstBreak)
              modNoBreaks
            else Newline.orMod(hasBreak, modSpace),
          ).onlyFor(SplitTag.SelectChainSecondNL)
        if (canStartSelectChain(thisSelect, nextSelect, expireTree)) {
          val chainExpire =
            if (nextSelect eq null) thisSelect.nameFt else expire
          val nestedPenalty = nestedSelect(rightOwner) + nestedApplies(leftOwner)
          // This policy will apply to both the space and newline splits, otherwise
          // the newline is too cheap even it doesn't actually prevent other newlines.
          val penalizeBreaks = PenalizeAllNewlines(chainExpire, 2)
          val newlinePolicy = classicNonFirstBreakOnNextDot & penalizeBreaks
          val ignoreNoSplit = nlOnly ||
            hasBreak &&
            (afterComment || cfg.newlines.selectChains.classicKeepFirst)
          val chainLengthPenalty =
            if (
              cfg.newlines.penalizeSingleSelectMultiArgList &&
              (nextSelect eq null)
            )
              // penalize by the number of arguments in the rhs open apply.
              // I know, it's a bit arbitrary, but my manual experiments seem
              // to show that it produces OK output. The key insight is that
              // many arguments on the same line can be hard to read. By not
              // putting a newline before the dot, we force the argument list
              // to break into multiple lines.
              ftAfterRight.meta.rightOwner match {
                case Member.ArgClause(v) => math.max(0, v.length - 1)
                case _ => 0
              }
            else 0
          // when the flag is on, penalize break, to avoid idempotence issues;
          // otherwise, after the break is chosen, the flag prohibits nosplit
          val nlBaseCost =
            if (cfg.newlines.selectChains.classicKeepFirst) 3 else 2
          val nlCost = nlBaseCost + nestedPenalty + chainLengthPenalty
          val nlMod = getNlMod
          // must come first, for backwards compat
          val legacySplit = splitSecondNL(nlMod).withPolicy(newlinePolicy)
          val slbSplit =
            if (ignoreNoSplit) Split.ignored
            else {
              val noSplit = Split(modSpace, 0)
              if (prevChain) noSplit
              else chainExpire.left match { // allow newlines in final {} block
                case _: T.RightBrace =>
                  val lb = matchingLeft(chainExpire)
                  noSplit.withSingleLine(lb, noSyntaxNL = true)
                case _ => noSplit
                    .withSingleLineNoOptimal(chainExpire, noSyntaxNL = true)
              }
            }.andPolicy(penalizeBreaks)
          val nlSplit = Split(Newline.orMod(ignoreNoSplit, nlMod), nlCost)
            .withPolicy(newlinePolicy)
          Seq(legacySplit, slbSplit, nlSplit)
        } else {
          val doBreak = nlOnly || afterComment && hasBreak
          Seq(
            splitSecondNL(Newline.orMod(doBreak, getNlMod))
              .withPolicy(classicNonFirstBreakOnNextDot),
            Split(Newline.orMod(doBreak, modSpace), 0),
          )
        }

      case Newlines.keep =>
        if (hasBreak) Seq(Split(Newline, 0))
        else if (hasBreakAfterRightBeforeNonComment(ft)) {
          spaceIsDelayedNL = true
          val nft = next(ft)
          Seq(Split(modSpace, 0).withPolicy(
            decideNewlinesOnlyAfterClose(nft),
            ignore = nft.right.is[T.Comment],
          ))
        } else Seq(
          Split(modSpace, 0),
          Split(Newline, 1).onlyFor(SplitTag.SelectChainBinPackNL),
        )

      case Newlines.unfold =>
        val nlMod = Newline.withAlt(modSpace)
        def splits(
            nlPolicy: Policy = NoPolicy,
        )(slbEnd: FT, noSyntaxNL: Boolean = false) =
          if (nlOnly) Seq(Split(nlMod, 0, nlPolicy))
          else Seq(
            if (isRightCommentWithBreak(next(ft))) {
              spaceIsDelayedNL = true
              Split(modSpace, 0, nlPolicy)
            } else Split(modSpace, 0)
              .withSingleLine(slbEnd, noSyntaxNL = noSyntaxNL),
            Split(nlMod, 1, nlPolicy),
          )
        if ((prevSelect eq null) && (nextSelect eq null)) splits()(getSlbEnd())
        else splits(forcedBreakOnNextDotPolicy)(expire, noSyntaxNL = true)

      case Newlines.fold =>
        def nlSplitBase(cost: Int, policy: Policy = NoPolicy)(implicit
            fileLine: FileLine,
        ) = Split(Newline.withAlt(modSpace, noAltIndent = true), cost, policy)
        if ((nextFewerBraces ne null) && (nextFewerBraces ne expire)) {
          val policy: Policy = forcedBreakOnNextDotPolicy
          if (nlOnly) Seq(nlSplitBase(0).withPolicy(policy))
          else {
            val noSplit = Split(modSpace, 0, policy)
            Seq(noSplit, nlSplitBase(1, policy))
          }
        } else if (!nlOnly) {
          val end =
            if (nextSelect eq null) expire
            else getLastNonTrivial(nextSelect.qual)
          val bracketsToo = nextSelect.orHas(_.qual.is[Term.ApplyType]) &&
            (cfg.binPack.callSiteFor(isBracket = true) ne BinPack.Site.Never)
          val exclude =
            insideBracesBlock(ft, end, parens = true, brackets = bracketsToo)
              .excludeCloseDelim
          def arrowOwner(tree: Tree): Boolean = tree.is[Member.Function] &&
            (findTreeWithParent(tree) {
              case _: Member.Apply => MaybeBool.True
              case p: Term.ArgClause if !isSeqSingle(p.values) =>
                MaybeBool.False
              case p: Tree.Block if !isSeqSingle(p.stats) => MaybeBool.False
              case _ => MaybeBool.Maybe
            } ne null)
          val arrowPolicy = exclude.ranges
            .foldLeft(Policy.noPolicy) { case (res, tr) =>
              val policy = Policy.onRight(tr.rt, "PNL+DOTARR") {
                case Decision(FT(_: T.FunctionArrow, r, m), ss)
                    if !r.is[T.Comment] && arrowOwner(m.leftOwner) =>
                  ss.penalizeNL(1)
              }
              Policy.End <= tr.lt ==> policy ==> res
            }
          // include paren as it may have been a brace earlier (i.e. idempotence)
          val bracesToParens = ftAfterRight.right.is[T.OpenDelim] && {
            implicit val ft: FT = next(ftAfterRight)
            val rb = matchingRight(ftAfterRight)
            getBracesToParensModOnly(rb) ne Space
          }
          val noSplit = Split(modSpace, 0).withSingleLine(end, exclude = exclude)
          val nlCost = if (bracesToParens) 0 else 1
          val nlSplit = nlSplitBase(nlCost, arrowPolicy)
          Seq(noSplit, nlSplit)
        } else Seq(nlSplitBase(0))
    }

    val delayedBreakPolicyOpt = Policy ? (nextSelect ne null) && {
      val tree = nextSelect.tree
      Policy.beforeLeft(nextSelect.nameFt, "NEXTSEL1NL") {
        case Decision(FT(_, _: T.Dot, m), s) if m.rightOwner eq tree =>
          SplitTag.SelectChainFirstNL.activateOnly(s)
        case Decision(FT(_, _: T.Comment, m), s)
            if m.rightOwner.eq(tree) && (nextFewerBraces eq null) &&
              s.exists(_.isNeededFor(SplitTag.SelectChainFirstNL)) =>
          SplitTag.SelectChainFirstNL.activateOnly(s)
      }
    }

    // trigger indent only on the first newline
    val noIndent = fewerBracesLike &&
      indentFewerBraces == Indents.FewerBraces.never
    val nlIndent =
      if (noIndent) Indent.Empty else Indent(indentLen, expire, After)
    val spcPolicy: Policy = delayedBreakPolicyOpt
    val nlPolicy = spcPolicy ? noIndent
    val splits =
      if (
        spaceIsDelayedNL ||
        nextNonCommentSameLine(ftAfterRight).right.is[T.Comment]
      ) // will break
        baseSplits.map(_.withIndent(nlIndent).andFirstPolicy(nlPolicy))
      else {
        val spcIndent =
          if (nextFewerBraces eq null) Indent.empty
          else if (noIndentFewerBraces) Indent.Empty
          else if (nextSelect eq null) nlIndent
          else Indent(indentLen, nextFewerBraces, Before)
        baseSplits.map(s =>
          if (s.isNL) s.withIndent(nlIndent).andFirstPolicy(nlPolicy)
          else s.withIndent(spcIndent).andFirstPolicy(spcPolicy),
        )
      }

    if (prevSelect eq null) splits
    else baseSplits ++ splits.map(_.onlyFor(SplitTag.SelectChainFirstNL))
  }
}

object SplitsAfterAt extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import ft._
    if (leftOwner.is[Pat.Bind]) Seq(Split(Space, 0))
    else right match {
      case _: T.Symbolic => Seq(Split(NoSplit, 0))
      // Add space if right starts with a symbol
      case r: T.Ident => Seq(Split(identModification(r), 0))
      case _ => Nil
    }
  }
}

object SplitsBeforeAt extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = // For annotations, see #183 for discussion on this.
    if (ft.rightOwner.is[Pat.Bind]) Seq(Split(Space, 0)) else Nil
}

object SplitsAfterIdent extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    def getIdent = left.asInstanceOf[T.Ident].value
    leftOwner match {
      case t: Lit if right.is[T.Literal] && (t eq rightOwner) =>
        Seq(Split(NoSplit, 0))
      case t: Type.Name if right.is[T.Underscore] && (t eq rightOwner) && {
            val ident = getIdent
            ident == "+" || ident == "-"
          } => Seq(Split(NoSplit, 0)) // Kind projector type lambda
      case t: Name => t.parent match {
          case Some(p: Member.Infix) if p.op eq t =>
            if (right.is[T.Colon]) Seq(Split(NoSplit, 0))
            else insideInfixSplit(p)
          case Some(p: Lit.WithUnary)
              if (p.op eq t) && (t.tokens.head eq left) => Seq(Split(NoSplit, 0))
          case Some(p: Term.ApplyUnary)
              if (p.op eq t) && (t.tokens.head eq left) =>
            val useSpace = right match {
              case _: T.LeftBrace => true
              case r: T.Ident => isSymbolicName(r.value)
              case _ => false
            }
            Seq(Split(Space(useSpace), 0))
          case _ => Nil
        }
      case t: Pat.Alternative if getIdent == "|" => // Pattern alternatives
        if (cfg.newlines.keep) Seq(Split(Space.orNL(noBreak), 0))
        else {
          val end = getLast(t.rhs match {
            case t: Pat.Alternative => t.lhs
            case t => t
          })
          val nft = nextAfterNonComment(end)
          val opt = nft.left match {
            case _: T.KwIf => end
            case _ => nft
          }
          Seq(
            Split(Space, 0).withOptimalToken(opt, killOnFail = false),
            Split(Newline, 1),
          )
        }
      case _: Mod.Variant if right.isAny[T.Ident, T.Underscore] =>
        Seq(Split(Space(isSymbolicIdent(right)), 0)) // Type variance
      case _: Type.ByNameType if soft.KwPureFunctionArrow.matches(left) =>
        Seq(Split(Space(cfg.spaces.inByNameTypes && !right.is[T.LeftBrace]), 0))
      case _: Mod.ParamsType => SplitsAfterImplicit.get
      case _: Term.SplicedMacroLike => SplitsNoSplit.get
      case _ => Nil
    }
  }
}

object SplitsBeforeIdent extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, ft._
    def getIdent = right.asInstanceOf[T.Ident].value
    rightOwner match {
      case t: Template if soft.KwDerives(right) =>
        SplitsBeforeExtends.getTemplate(t)
      case _: Pat.Alternative if getIdent == "|" =>
        val noNL = !cfg.newlines.keepBreak(hasBreak)
        Seq(Split(Space.orNL(noNL), 0))
      case _: Tree.Repeated | _: Pat.SeqWildcard if getIdent == "*" =>
        Seq(Split(NoSplit, 0))
      case _: Type.Capturing | _: Type.CapSetName if getIdent == "^" =>
        Seq(Split(NoSplit, 0))
      case _: Type.Name if leftOwner.is[Type.Project] =>
        Seq(Split(Space(isSymbolicName(getIdent)), 0))
      case t: Name => t.parent match {
          case Some(p: Member.Infix) if p.op eq t => insideInfixSplit(p)
//          case _ if left.is[T.Dot] => Seq(Split(NoSplit, 0))
          case _ => Nil
        }
      case _ => Nil
    }
  }
}

object SplitsBeforeExtends extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = ft.rightOwner match {
    case t: Defn.EnumCase => getEnumCase(t)
    case t: Template => getTemplate(t)
    case _ => Nil
  }

  def getEnumCase(
      enumCase: Defn.EnumCase,
  )(implicit ft: FT, fo: FormatOps, cfg: ScalafmtConfig): Seq[Split] = {
    import fo._, tokens._, ft._
    binPackParentConstructorSplits(
      true,
      Set(rightOwner),
      enumCase.inits.headOrNull,
      getLast(rightOwner),
      cfg.indent.extendSite,
      enumCase.inits.lengthCompare(1) > 0,
    )
  }

  def getTemplate(
      template: Template,
  )(implicit ft: FT, fo: FormatOps, cfg: ScalafmtConfig): Seq[Split] = {
    import fo._
    def lastToken = templateDerivesOrCurlyOrLastNonTrivial(template)
    binPackParentConstructorSplits(
      true,
      Set(template),
      findTemplateGroupOnRight(_.superType)(template),
      lastToken,
      cfg.indent.extendSite,
      if (template.earlyClause.nonEmpty) template.inits.nonEmpty
      else template.inits.lengthCompare(1) > 0,
    )
  }
}

object SplitsBeforeWith extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    rightOwner match {
      // something like new A with B with C
      case t: Template
          if t.parent.isAny[Term.New, Term.NewAnonymous, Defn.Given] =>
        val (isFirstCtor, extendsThenWith) = t.inits match {
          case _ :: x :: _ => (x.begOffset > ft.right.start, true)
          case _ => (false, false)
        }
        binPackParentConstructorSplits(
          isFirstCtor,
          Set(t),
          findTemplateGroupOnRight(_.superType)(t),
          templateCurlyOrLastNonTrivial(t),
          cfg.indent.main,
          extendsThenWith,
        )
      // trait A extends B with C with D with E
      case t: Template =>
        typeTemplateSplits(t, cfg.indent.withSiteRelativeToExtends)
      case t @ WithChain(top) => binPackParentConstructorSplits(
          !t.lhs.is[Type.With],
          withChain(top).toSet,
          t.rhs,
          getLast(top),
          cfg.indent.main,
        )
      case enumCase: Defn.EnumCase =>
        val indent = cfg.indent.withSiteRelativeToExtends
        val expire = getLast(enumCase)
        Seq(
          Split(Space, 0).withIndent(indent, expire, ExpiresOn.After),
          Split(Newline, 1).withIndent(indent, expire, ExpiresOn.After),
        )
      case _ => Seq(Split(Space, 0))
    }
  }
}

object SplitsAfterIf extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    leftOwner match {
      case t: Term.If =>
        val expire = getLast(t)
        val isKeep = cfg.newlines.keep
        val mod =
          if (isKeep && hasBreak) Newline
          else Space(cfg.spaces.isSpaceAfterKeyword(right))
        val slb = Split(mod.isNL, 0)(mod).withSingleLine(expire)
        val mlSplitBase = Split(mod, if (slb.isIgnored) 0 else 1).withPolicy(
          if (isKeep) getBreakBeforeElsePolicy(t)
          else getBreaksBeforeElseChainPolicy(t),
        )
        val mlSplitOpt = OptionalBraces
          .indentAndBreakBeforeCtrl[T.KwThen](t.cond, mlSplitBase)
        Seq(slb, mlSplitOpt ?? mlSplitBase)

      case _: Case if cfg.newlines.keepBreak(nextNonCommentSameLine(ft)) =>
        Seq(Split(Newline, 0))

      case t: Term.Name if t.parent.is[Term.EndMarker] => Nil

      case _ => Seq(Split(Space, 0))
    }
  }
}

object SplitsBeforeIf extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    rightOwner match {
      case t: Enumerator.Guard =>
        val isFirst = t.parent match {
          case Some(p: Term.EnumeratorsBlock) =>
            @tailrec
            def iter(enums: List[Enumerator], prev: Tree): Boolean = enums match {
              case head :: tail =>
                if (head eq t) !prev.is[Enumerator.Guard] else iter(tail, head)
              case _ => false
            }
            iter(p.enums, p)
          case _ => false
        }
        if (isFirst) cfg.newlines.source match {
          case Newlines.fold =>
            val endOfGuard = getLast(rightOwner)
            val exclude = insideBracesBlock(ft, endOfGuard, parens = true)
            Seq(
              Split(Space, 0).withSingleLine(endOfGuard, exclude = exclude),
              Split(Newline, 1),
            )
          case Newlines.unfold => Seq(Split(Newline, 0))
          case _ => Seq(Split(hasBreak, 0)(Space), Split(Newline, 1))
        }
        else Seq(Split(Newline, 0))

      case t: Case =>
        val arrow = getCaseArrow(t)
        val noSplit =
          if (cfg.newlines.keepBreak(hasBreak)) Split.ignored
          else {
            val afterIf = nextNonCommentSameLineAfter(ft)
            if (cfg.newlines.keepBreak(afterIf)) {
              val indent = Indent(cfg.indent.main, arrow, ExpiresOn.Before)
              Split(Space, 0).withSingleLine(afterIf).withIndent(indent)
            } else {
              val exclude = insideBracesBlock(ft, arrow)
              Split(Space, 0).withSingleLine(
                arrow,
                exclude = exclude,
                recurseOnly = !exclude.isEmpty,
              )
            }
          }
        val nlSplit = Split(Newline, 1)
          .withPolicy(penalizeNewlineByNesting(next(ft), arrow))
        Seq(noSplit, nlSplit)

      case _ => Nil
    }
  }
}

object SplitsAfterForWhile extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, ft._
    if (leftOwner.isAny[Term.While, Term.ForClause]) { // exclude end marker
      def spaceMod = Space(cfg.spaces.isSpaceAfterKeyword(right))
      def splitBase(implicit fileLine: FileLine) = {
        val onlyNL = cfg.newlines.keepBreak(hasBreak)
        Split(Newline.orMod(onlyNL, spaceMod), 0)
      }
      val split = leftOwner match {
        // block expr case is handled in OptionalBraces.WhileImpl
        case t: Term.While => OptionalBraces
            .indentAndBreakBeforeCtrl[T.KwDo](t.expr, splitBase)
        // below, multi-enum cases are handled in OptionalBraces.ForImpl
        case t: Term.For => getSingleElement(t.enumsBlock).nnMap(x =>
            OptionalBraces.indentAndBreakBeforeCtrl[T.KwDo](x, splitBase),
          )
        case t: Term.ForYield => getSingleElement(t.enumsBlock).nnMap(x =>
            OptionalBraces.indentAndBreakBeforeCtrl[T.KwYield](x, splitBase),
          )
        case _ => null
      }
      Seq(split ?? Split(spaceMod, 0))
    } else Nil
  }
}

object SplitsBeforeWhile extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import ft._
    if (!rightOwner.is[Term.Do]) Nil
    else if (hasBlankLine) Seq(Split(Newline2x, 0))
    else {
      val nlOnly = left.is[T.RightBrace] &&
        cfg.newlines.alwaysBeforeElseAfterCurlyIf &&
        isCurlyWithNextKeyword(ft, ft) ||
        !cfg.newlines.sourceIgnored && hasBreak
      val noSplit = Split(nlOnly, 0)(Space)
      val nlSplit = Split(Newline, 1)
      Seq(noSplit, nlSplit)
    }
  }
}

object SplitsAfterDo extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    leftOwner match {
      case t: Term.Do => getWithBody(t) { eft =>
          val indent = Indent(cfg.indent.main, eft, ExpiresOn.After)
          val kwWhile = nextAfterNonComment(eft)
          val noSplit =
            if (noBreak && isRightCommentThenBreak(ft)) Split(Space, 0)
              .withIndents(indent)
            else {
              val exclude = insideBracesBlock(ft, eft, parens = true)
              Split(Space, 0)
                .withSingleLine(getSlbEndOnLeft(eft), exclude = exclude)
            }
          val nlSplit = Split(Newline, 1, decideNewlinesOnlyBeforeToken(kwWhile))
            .withIndents(indent)
          Seq(noSplit, nlSplit)
        }
      case t: Tree.WithBody if cfg.newlines.keepBreak =>
        getWithBody(t)(eft =>
          Seq(Split(Newline, 1).withIndent(cfg.indent.main, eft, ExpiresOn.After)),
        )
      case _ => Nil
    }
  }

  private def getWithBody(t: Tree.WithBody)(
      ifNotBlock: FT => Seq[Split],
  )(implicit ft: FT, fo: FormatOps): Seq[Split] = {
    import fo._, tokens._, ft._
    val eft = getLast(t.body)
    if (
      t.body.is[Tree.Block] && right.is[T.LeftBrace] &&
      { val m = matchingRightOrNull(ft); (m ne null) && m.idx >= eft.idx }
    ) Seq(Split(Space, 0))
    else ifNotBlock(eft)
  }
}

object SplitsBeforeThenDo {
  def get(implicit ft: FT, fo: FormatOps, cfg: ScalafmtConfig): Seq[Split] = {
    import fo._, tokens._, ft._
    val noSplit = Split(!cfg.newlines.sourceIgnored && hasBreak, 0)(Space)
      .withOptimalToken(nextNonCommentSameLineAfter(ft), killOnFail = false)
    Seq(noSplit, Split(Newline, 1))
  }
}

object SplitsBeforeThen extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = SplitsBeforeThenDo.get
}

object SplitsBeforeDo extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = if (ft.rightOwner.is[Term.Do]) Nil else SplitsBeforeThenDo.get
}

object SplitsAfterElse extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    leftOwner match {
      case t: Term.If =>
        val space = t.elsep match {
          case _: Term.If => true
          case b: Term.Block => isEnclosedInBraces(b) ||
            (b.stats match {
              case (_: Term.If) :: Nil => true
              case _ => false
            })
          case _ => false
        }
        if (space) Seq(Split(Space, 0))
        else {
          val expire = tokens.getLast(t.elsep)
          def nlSplitFunc(cost: Int) = Splits.lowRankNL(ft, cost)
            .withIndent(cfg.indent.main, expire, After)
          if (cfg.newlines.getBeforeMultiline eq Newlines.unfold)
            Seq(nlSplitFunc(0))
          else CtrlBodySplits.get(t.elsep)(
            Split(Space, 0).withSingleLineNoOptimal(expire),
          )(nlSplitFunc)
        }
      case _ => Nil
    }
  }
}

object SplitsBeforeElseYield extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    if (ft.hasBlankLine) Seq(Split(Newline2x, 0))
    else if (left.is[T.RightBrace]) Seq(Split(Space, 0))
    else {
      val noSplit =
        if (cfg.newlines.okSpaceForSource(newlinesBetween)) {
          val expire = getLast(rightOwner)
          Split(Space, 0).withSingleLineNoOptimal(
            expire,
            exclude = insideBracesBlock(ft, expire),
            noSyntaxNL = right.is[T.KwYield],
          )
        } else Split.ignored
      Seq(noSplit, Split(Newline, 1))
    }
  }
}

object SplitsAfterCase extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    leftOwner match {
      case t: Defn.RepeatedEnumCase =>
        val indent = Indent(cfg.indent.main, getLast(t), ExpiresOn.After)
        Seq(Split(Space, 0).withIndent(indent))

      case c: Case => getCaseTree(getCaseArrow(c), c)
      case tc: TypeCase => getCaseTree(getCaseArrow(tc), tc)

      case _ => Nil
    }
  }

  private def getCaseTree(arrow: FT, owner: CaseTree)(implicit
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._
    val postArrow = nextNonCommentSameLine(arrow)
    val ownerEnd = getLast(owner)
    val expire = {
      val nft = nextNonCommentSameLine(ownerEnd)
      val commentOwner = owner.parent.orNull
      def notNextCase(xft: FT) = !xft.rightOwner.parent.contains(commentOwner)
      cfg.comments.indentTrailingInCaseBody match {
        case Comments.IndentTrailingInCaseBody.none => nft
        case Comments.IndentTrailingInCaseBody.more => // find last blank or non-comment
          var bft: FT = null
          findTokenEx { xft =>
            if (xft.hasBlankLine) bft = xft
            if (xft.right.is[T.Comment] && (commentOwner eq xft.rightOwner))
              Left(next(xft))
            else Right(if ((bft eq null) || notNextCase(xft)) xft else bft)
          }(nft) ?? nft
        case Comments.IndentTrailingInCaseBody.less => // find first blank or non-comment
          findToken(nft, next)(xft =>
            !xft.right.is[T.Comment] || (commentOwner ne xft.rightOwner) ||
              xft.hasBlankLine,
          )
      }
    }

    val bodyBlock = isCaseBodyABlock(arrow, owner)
    def defaultPolicy = decideNewlinesOnlyAfterToken(postArrow)
    val postArrowPolicy =
      if (bodyBlock || (arrow ne postArrow) && postArrow.hasBreak) NoPolicy
      else {
        // postArrowFt points to non-comment after arrowFt
        // possibly on next line without intervening comments
        implicit val beforeMultiline = cfg.newlines.getBeforeMultiline
        val rparen = getClosingIfCaseBodyEnclosedAsBlock(postArrow, owner)
        if (rparen eq null)
          Policy ? beforeMultiline.in(Newlines.fold, Newlines.keep) ||
          defaultPolicy
        else {
          val lparen = next(postArrow)
          val postParen = nextNonCommentSameLine(lparen)
          val indent = cfg.indent.main
          val lindents = Seq(
            Indent(indent, next(rparen), Before),
            Indent(-indent, expire, After),
          )
          val lmod = NewlineT(noIndent = rhsIsCommentedOut(postParen))
          val lsplit = Seq(Split(lmod, 0).withIndents(lindents))
          Policy.onRight(postParen, prefix = "CASE[(]") {
            case Decision(`postParen`, _) => lsplit
            // fires only if there's a comment between lparentFt and postParentFt
            case Decision(`lparen`, _) => Seq(Split(Space, 0))
          } ==>
            Policy.onlyFor(rparen, prefix = "CASE[)]")(_ => Seq(Split(Newline, 0)))
        }
      }

    val bodyIndent = if (bodyBlock) 0 else cfg.indent.main
    val arrowIndent = cfg.indent.caseSite - bodyIndent
    val indents =
      List(Indent(bodyIndent, expire, After), Indent(arrowIndent, arrow, After))
    val mod = ModExt(Space, indents)
    val slbExpireOpt = prevNotTrailingComment(ownerEnd).toOption
    val policy = slbExpireOpt.fold(postArrowPolicy) { slbExpire =>
      val onArrowPolicy = Policy.End <= arrow ==>
        Policy.onRight(postArrow, "CASESLB>ARROW") { case Decision(_, ss) =>
          ss.flatMap { s =>
            val split = s.andPolicy(postArrowPolicy)
            if (s.isNL) Seq(split)
            else Seq(
              s.withSingleLine(slbExpire, extend = true, noOptimal = !s.noCost),
              split,
            )
          }
        }
      Policy.RelayOnSplit((s, _) => s.isNL)(onArrowPolicy)(postArrowPolicy)
    }
    Seq(Split(mod, 0, policy))
  }
}

object SplitsAfterTryCatchFinally {
  def getWithBodyFunc(
      f: Term.TryClause => Tree,
  )(implicit ft: FT, fo: FormatOps, cfg: ScalafmtConfig): Seq[Split] =
    ft.leftOwner match {
      case _: Term.Name => Nil // exclude end marker
      case t: Term.TryClause => getWithBody(f(t))
      case t => getWithBody(t)
    }

  private def getWithBody(
      body: Tree,
  )(implicit ft: FT, fo: FormatOps, cfg: ScalafmtConfig): Seq[Split] = {
    import fo._, tokens._
    val end = getLast(body)
    val nft = nextNonComment(ft)
    val inBraces = body.is[Tree.Block] && nft.right.is[T.LeftBrace] && {
      val m = matchingRightOrNull(nft); (m ne null) && m.idx >= end.idx
    }
    if (inBraces) Seq(Split(Space, 0))
    else {
      val indent = Indent(cfg.indent.main, end, ExpiresOn.After)
      CtrlBodySplits.get(body, Seq(indent))(
        Split(Space, 0).withSingleLineNoOptimal(end),
      )(Splits.lowRankNL(ft, _).withIndent(indent))
    }
  }
}

object SplitsAfterTry extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = SplitsAfterTryCatchFinally.getWithBodyFunc(_.expr)
}

object SplitsBeforeCatchFinally extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import ft._
    val ok = cfg.newlines.alwaysBeforeElseAfterCurlyIf ||
      !(left.is[T.RightBrace] && isCurlyWithNextKeyword(ft, ft))
    if (ok) Seq(Split(Newline2x(hasBlankLine), 0)) else Nil
  }
}

object SplitsAfterCatch extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = SplitsAfterTryCatchFinally
    .getWithBodyFunc(t => t.catchClause.getOrElse(t))
}

object SplitsAfterFinally extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = SplitsAfterTryCatchFinally
    .getWithBodyFunc(t => t.finallyp.getOrElse(t))
}

object SplitsAfterYield extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    (leftOwner match {
      case t: Term.ForYield => TreeOps.getBlockStat(t.body)
      case _ => null
    }) match {
      case null => Nil
      case b: Term.PartialFunction
          if dialect.allowSignificantIndentation &&
            nextNonComment(ft).right.is[T.KwCase] =>
        val split = Split(Newline, 0)
          .withIndent(cfg.indent.getSignificant, getLast(b), ExpiresOn.After)
        Seq(split)
      case b: Tree.Block if right.is[T.LeftBrace] && {
            val m = matchingRightOrNull(ft);
            (m ne null) && m.idx >= getLast(b).idx
          } => Seq(Split(Space, 0))
      case b =>
        val lastToken = getLast(b)
        val indent = Indent(cfg.indent.main, lastToken, ExpiresOn.After)
        val avoidAfterYield = cfg.newlines.avoidAfterYield && ! {
          b.isAny[Term.If, Term.ForClause, Term.TryClause] && // unless in parens
          !nextNonComment(ft).right.is[T.LeftParen]
        }
        if (avoidAfterYield) {
          val noIndent = !isRightCommentWithBreak(ft)
          Seq(Split(Space, 0).withIndent(indent, noIndent))
        } else Seq(
          // Either everything fits in one line or break on =>
          Split(cfg.newlines.keepBreak, 0)(Space).withSingleLine(lastToken),
          Split(Newline, 1).withIndent(indent),
        )
    }
  }
}

object SplitsAfterXmlStart extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._
    val splits = Seq(Split(NoSplit, 0))
    if (prev(ft).left.is[T.LeftBrace]) splits
    else withIndentOnXmlStart(matchingLeft(ft), splits)
  }
}

object SplitsAfterXmlSpliceStart extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] =
    if (cfg.xmlLiterals.assumeFormatted) fo
      .withIndentOnXmlSpliceStart(ft, Seq(Split(NoSplit, 0)))
    else Nil
}

object SplitsAfterCommentLowPriority extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = Seq(Split(getMod(ft), 0))
}

object SplitsBeforeCommentLowPriority extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    val forceBlankLine = hasBreak && blankLineBeforeDocstring(ft)
    val mod = if (forceBlankLine) Newline2x else getMod(ft)
    val nft = nextNonCommentAfter(ft)
    def baseSplit(implicit fileLine: FileLine) = Split(mod, 0)

    val nearbyDot =
      if (nft.right.is[T.Dot]) nft
      else {
        val pft = prevBeforeNonComment(ft)
        if (pft.right.is[T.Dot]) pft else null
      }
    val selectLike = nearbyDot.nnMap(Select.onRightOrNull)

    selectLike match {
      case null =>
        val infixSplits = nft.rightOwner match {
          case t: Name if nft.right.is[T.Ident] =>
            t.parent match {
              case Some(p: Member.Infix) if p.op eq t => insideInfixSplit(p)
              case _ => Nil
            }
          case _ => Nil
        }
        if (infixSplits.isEmpty) Seq(baseSplit) else infixSplits
      case t =>
        val noIndent = (nearbyDot ne null) && nearbyDot.idx <= ft.idx
        def split(implicit fileLine: FileLine) = baseSplit
          .withIndent(cfg.indent.main, nft, ExpiresOn.After, ignore = noIndent)
        if (Select.prev(t, cfg.newlines.encloseSelectChains) eq null) Seq(split)
        else Seq(baseSplit, split.onlyFor(SplitTag.SelectChainFirstNL))
    }
  }
}

object SplitsBeforeHash extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = Seq(Split(Space(endsWithSymbolIdent(ft.left)), 0))
}

object SplitsAfterImplicit extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo._, tokens._, ft._
    val ok = cfg.binPack.defnSite == BinPack.Site.Never &&
      !cfg.verticalMultiline.atDefnSite && !right.is[T.Comment]
    val params = if (ok) TreeOps.getImplicitParamList(leftOwner) else null
    if (params eq null) Nil
    else {
      val spaceSplit =
        if (cfg.newlines.forceAfterImplicitParamListModifier) Split.ignored
        else Split(Space, 0).withPolicy(
          SingleLineBlock(getLast(params)),
          cfg.newlines.notPreferAfterImplicitParamListModifier,
        )
      Seq(spaceSplit, Split(Newline, if (spaceSplit.isActive) 1 else 0))
    }
  }
}

object SplitsAfterDot extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] =
    if (existsParentOfType[ImportExportStat](ft.rightOwner))
      Seq(Split(NoSplit, 0))
    else Nil
}

object SplitsAfterDotLowPriority extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] =
    if (!ft.right.isAny[T.Ident, T.KwType, T.KwThis, T.KwSuper]) Nil
    else Seq(Split(NoSplit, 0))
}

object SplitsWithOptionalNL extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import ft._
    if (fo.optimizationEntities.optionalNL) {
      @tailrec
      def noAnnoLeftFor(tree: Tree): Boolean = tree.parent match {
        case Some(_: Mod.Annot) => false
        case Some(p: Init) => noAnnoLeftFor(p)
        case _ => true
      }
      def noAnnoLeft = leftOwner.is[Mod] || noAnnoLeftFor(leftOwner)
      def newlineOrBoth(implicit fileLine: FileLine) =
        if (cfg.newlines.annotation) Seq(Split(Newline, 0))
        else Seq(Split(Space, 0), Split(Newline, 1))
      cfg.newlines.source match {
        case _ if hasBlankLine => Seq(Split(Newline2x, 0))
        case Newlines.unfold =>
          if (right.is[T.At]) newlineOrBoth
          else Seq(Split(Space.orNL(noAnnoLeft), 0))
        case Newlines.fold =>
          if (right.is[T.At]) Seq(Split(Space, 0), Split(Newline, 1))
          else if (noAnnoLeft) Seq(Split(Space, 0))
          else newlineOrBoth
        case _ =>
          val noNL = !cfg.newlines.annotation || noBreak
          Seq(Split(Space.orNL(noNL), 0))
      }
    } else Nil
  }
}

object SplitsAfterBOF extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = Seq(Split(NoSplit.orNL(!ft.right.is[T.EOF] || ft.idx > 0), 0))
}

object SplitsAfterEOF extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = Seq(Split(NoSplit.orNL(fo.tokens.prev(ft).left.is[T.BOF]), 0))
}

object SplitsAfterOptionalBracesKeyword extends Splits {
  override def get(implicit
      ft: FT,
      fo: FormatOps,
      cfg: ScalafmtConfig,
  ): Seq[Split] = {
    import fo.tokens
    val ob = OptionalBraces.get(ft)
    if (ob eq null) Nil else ob.splits
  }

}
