package org.scalafmt
package rewrite

import org.scalafmt.config._
import org.scalafmt.internal._
import org.scalafmt.util.TreeOps._

import scala.meta._
import scala.meta.tokens.{Token => T, _}

import scala.annotation.tailrec
import scala.reflect.ClassTag

object RemoveScala3OptionalBraces extends FormatTokensRewrite.RuleFactory {

  private def settings(implicit
      style: ScalafmtConfig,
  ): RewriteScala3Settings.RemoveOptionalBraces =
    style.rewrite.scala3.optionalBraces

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    style.dialect.allowSignificantIndentation && settings.enabled

  override def create(implicit ftoks: FormatTokens): FormatTokensRewrite.Rule =
    new RemoveScala3OptionalBraces

}

private class RemoveScala3OptionalBraces(implicit val ftoks: FormatTokens)
    extends FormatTokensRewrite.RuleWithAppend {

  import FormatTokensRewrite._
  import RemoveScala3OptionalBraces.settings

  private def allowOldSyntax(implicit style: ScalafmtConfig): Boolean =
    ConvertToNewScala3Syntax.enabled || settings.oldSyntaxToo

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    RemoveScala3OptionalBraces.enabled

  override def onToken(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Replacement = ft.right match {
    case x: T.LeftBrace => ft.meta.rightOwner match {
        case _ if ftoks.nextNonCommentAfter(ft).right.is[T.RightBrace] => null
        case t: Term.Block if t.stats.nonEmpty =>
          val pft = ftoks.prevNonComment(ft)
          val removeRepl =
            if (settings.isRemoveEnabled) onLeftForBlock(t, pft) else null
          if (removeRepl ne null) removeRepl
          else if (
            isSingleStatBlock(t) && settings.isInsertEnabled &&
            (session.rule[RedundantBraces] ne null) && {
              val triple = getOptionalBraces(pft)
              (triple ne null) && triple._3
            }
          ) Replacement(this, ft, ReplacementType.Replace, style)
          else null
        case _ if !settings.isRemoveEnabled => null
        case t: Template.Body if !t.isEmpty =>
          if (t.parent.parent.is[Defn.Given]) removeToken
          else replaceToken(":")(new T.Colon(x.input, x.dialect, x.start))
        case t: Term.ArgClause => onLeftForArgClause(t)
        case t: Term.PartialFunction => t.parent match {
            case Some(p: Term.ArgClause) if (headTokenOrNull(p) match {
                  case px: T.LeftBrace => px eq x
                  case px: T.LeftParen =>
                    shouldRewriteArgClauseWithLeftParen[RedundantBraces](px)
                  case _ => false
                }) => onLeftForArgClause(p)
            case _ => null
          }
        case t: Term.EnumeratorsBlock
            if allowOldSyntax || !t.parent.is[Term.For] || {
              val rbFt = ftoks.matchingRight(ft)
              ftoks.nextNonComment(rbFt).right.is[T.KwDo]
            } => removeToken
        case _: Tree.CasesBlock => removeToken
        case _: Ctor.Block if ftoks.prevNonComment(ft).left.is[T.Equals] =>
          removeToken
        case _ => null
      }
    case _ if !settings.isRemoveEnabled => null
    case _: T.LeftParen
        if !ftoks.nextNonCommentAfter(ft).right.is[T.RightParen] =>
      ft.meta.rightOwner match {
        case t: Term.ArgClause => onLeftForArgClause(t)
        case _ => null
      }
    case _: T.Colon if settings.isInsertEnabled => insertOnToken(isColon = true)
    case _ => null
  }

  override def appendOnToken(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Replacement =
    if (!settings.isInsertEnabled || ft.right.is[T.Colon]) null
    else insertOnToken(isColon = false)

  private[rewrite] def getOptionalBraces(ft: FT)(implicit
      session: Session,
      style: ScalafmtConfig,
  ): (OptionalBraces, FT, Boolean) = OptionalBraces.getWithBrace {
    val r = session.claimedRule(ft.idx - 1)
    val left =
      if ((r ne null) && (r.how eq ReplacementType.Replace)) r.ft.left else null
    if (left eq null) ft else ft.copy(left = left)
  }

  private def insertOnToken(isColon: Boolean)(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Replacement = getOptionalBraces(ftoks.next(ft))
    .nnMap { case (ob, _, skip) =>
      val block = ob.block
      if (skip) null
      else if (block.parent.is[Case]) if (isColon) removeToken else null
      else {
        val rb = ftoks.getLast(block)
        if (rb eq null) null
        else {
          val rt = ft.right
          val lb = new T.LeftBrace(rt.input, rt.dialect, rt.start)
          if (isColon) replaceToken("{", afterRight = rb.idx)(lb)
          else {
            val meta = ft.meta
              .copy(right = ft.meta.right.copy(text = "{", owner = block))
            val how = appendTokensType(ft.copy(right = lb, meta = meta))
            Replacement(this, ft, how, style, afterRight = rb.idx)
          }
        }
      }
    }

  override def onRight(left: Replacement, hasFormatOff: Boolean)(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): (Replacement, Replacement) = {
    val toBraces = left.how match {
      case ReplacementType.Replace => left.ft.right.is[T.LeftBrace]
      case rt: ReplacementType.AppendAfter => rt.ft.right.is[T.LeftBrace]
      case _ => false
    }
    if (toBraces) onRightToBraces(left, hasFormatOff)
    else onRightFromBraces(left, hasFormatOff)
  }

  private def onRightFromBraces(left: Replacement, hasFormatOff: Boolean)(
      implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): (Replacement, Replacement) = {
    val nextFt = ftoks.nextNonCommentAfter(ft)
    val notOkToRewrite = hasFormatOff || { // can't force significant indentation
      val cfg = settings
      !cfg.remove.forall { x =>
        import RewriteScala3Settings.Between
        val checks = Iterator[(Between, Between => Boolean)](
          (x.span, _.satisfied(session.getSpan(left))),
          (x.blankGaps, _.satisfied(session.getBlankGaps(left))),
        ).flatMap { case (bw, f) => if (bw.enabled) Some(f(bw)) else None }
        if (!cfg.preferInsert) checks.contains(true)
        else checks.hasNext && !checks.contains(false)
      }
    } ||
      (nextFt.meta.rightOwner match {
        case t: Term.Name => t.parent.exists {
            case p: Term.SelectPostfix => p.name eq t // select without `.`
            case p: Term.ApplyInfix if p.op eq t =>
              !style.dialect.allowInfixOperatorAfterNL || {
                val head = headTokenOrNull(t)
                (head ne null) && !head.isSymbolicInfixOperator
              }
            case _ => false
          }
        case _: Term.Select => nextFt.noBreak &&
          (style.newlines.getSelectChains eq Newlines.keep)
        case _ => false
      }) ||
      (left.ft.right match {
        case _: T.Colon =>
          !shouldRewriteColonOnRight(left) || !skipRightToBraces(left)
        case _ => false
      })
    ft.right match {
      case _ if notOkToRewrite => null
      case _: T.RightParen if RewriteTrailingCommas.checkIfPrevious =>
        (left, removeToken)
      case x: T.RightBrace =>
        val replacement = ft.meta.rightOwner match {
          case t: Term.EnumeratorsBlock
              if allowOldSyntax && t.parent.is[Term.For] &&
                !nextFt.right.is[T.KwDo] =>
            replaceToken("do", t.parentOrNull)(
              new T.KwDo(x.input, x.dialect, x.start),
            )
          case _ => removeToken
        }
        (left, replacement)
      case _ => null
    }
  }

  private[rewrite] def skipRightToBraces(
      left: Replacement,
  )(implicit session: Session, style: ScalafmtConfig): Boolean = {
    // left must be a LeftBrace
    val cfg = settings
    !cfg.insert.exists { ib =>
      val isSingleStatBlock = isTreeSingleExpr(left.how match {
        case x: ReplacementType.AppendAfter => x.ft.rightOwner
        case _ => left.ft.rightOwner
      })
      def getSpan = session.getSpan(left)
      def getBlankGaps = session.getBlankGaps(left)
      val checkSpan = (x: RewriteScala3Settings.Between) =>
        if (isSingleStatBlock) {
          val range = getSpan
          range >= style.maxColumn && x.satisfied(range)
        } else x.max == 0 || x.satisfied(getSpan)
      val checkBlankGaps = (x: RewriteScala3Settings.Between) =>
        if (isSingleStatBlock) {
          val range = getBlankGaps
          range >= 1 && x.satisfied(range)
        } else x.max == 0 || x.satisfied(getBlankGaps)
      val checks = Iterator((ib.span, checkSpan), (ib.blankGaps, checkBlankGaps))
        .flatMap { case (bw, f) => if (bw.enabled) Some(f(bw)) else None }
      if (cfg.preferInsert) checks.contains(true)
      else checks.hasNext && !checks.contains(false)
    }
  }

  private def onRightToBraces(left: Replacement, hasFormatOff: Boolean)(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): (Replacement, Replacement) = {
    val notOkToRewrite = skipRightToBraces(left)
    // had the braces already but RedundantBraces is on
    val maybeKeep =
      (left.how eq ReplacementType.Replace) && (ftoks(left.idx) eq left.ft)
    if (maybeKeep)
      if (notOkToRewrite) session.rule[RedundantBraces].nnMap(r =>
        {
          implicit val ft: FT = left.ft
          implicit val style: ScalafmtConfig = left.style
          r.onLeftBrace(ft.rightOwner)
        }.nnMap(r.onRight(_, hasFormatOff)),
      )
      else null
    else if (notOkToRewrite) null
    else {
      val rt = ft.right
      val rbt = new T.RightBrace(rt.input, rt.dialect, rt.end)
      val lb = left.how match {
        case how: ReplacementType.AppendAfter => how.ft
        case _ => left.ft
      }
      val rbmeta = lb.meta.copy(right = lb.meta.right.copy(text = "}"))
      val replType = appendTokensType(FT(rt, rbt, rbmeta))
      (left, Replacement(this, ft, replType, style))
    }
  }

  @tailrec
  private def onLeftForBlock(tree: Term.Block, pft: FT)(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Replacement = tree.parentOrNull match {
    case null => null
    case t: Term.Block
        if (pft.meta.leftOwner eq t) && pft.left.is[T.LeftBrace] &&
          hasSingleElement(t, tree) && session.isRemovedOnLeft(pft, true) =>
      onLeftForBlock(t, ftoks.prevNonCommentBefore(pft))
    case t: Term.If =>
      val ok = pft.left match {
        case _: T.KwIf => true
        case _: T.KwThen => true
        case _: T.KwElse => allowOldSyntax || isTreeSingleExpr(t.elsep) ||
          ftoks.tokenAfter(t.cond).right.is[T.KwThen]
        case _: T.RightParen => allowOldSyntax
        case _ => false
      }
      if (ok) removeToken else null
    case _: Term.While =>
      val ok = pft.left match {
        case _: T.KwDo => true
        case _: T.RightParen => allowOldSyntax
        case _ => false
      }
      if (ok) removeToken else null
    case _: Term.For =>
      val ok = pft.left match {
        case _: T.KwDo => true
        case _: T.RightParen | _: T.RightBrace => allowOldSyntax
        case _ => false
      }
      if (ok) removeToken else null
    case _: Term.ForYield => removeToken
    case _: Term.Try => removeToken
    case _: Term.Throw => removeToken
    case _: Term.Return => removeToken
    case _: Defn.ExtensionGroup => removeToken
    case t: Defn.Def =>
      if (tree ne t.body) null
      else if (pft.left.is[T.Equals]) removeToken
      else null
    case p: Tree.WithBody => if (p.body eq tree) removeToken else null
    case p: Term.ArgClause => headTokenOrNull(p) match {
        case _: T.LeftBrace => onLeftForArgClause(p)
        case px: T.LeftParen
            if shouldRewriteArgClauseWithLeftParen[RedundantParens](px) =>
          onLeftForArgClause(p)
        case _ => null
      }
    case _ => null
  }

  private def shouldRewriteArgClauseWithLeftParen[A <: Rule](
      lp: T,
  )(implicit ft: FT, session: Session, tag: ClassTag[A]): Boolean = {
    val prevFt = ftoks.prevNonComment(ft)
    prevFt.left.eq(lp) && session.claimedRule(prevFt.meta.idx - 1)
      .nnHas(x => tag.runtimeClass.isInstance(x.rule))
  }

  private[rewrite] def onLeftForArgClause(
      tree: Term.ArgClause,
  )(implicit ft: FT, style: ScalafmtConfig): Replacement = {
    val cfg = settings
    val ok = style.dialect.allowFewerBraces && cfg.fewerBraces.span.max > 0 &&
      (ft.right.is[T.LeftBrace] ||
        cfg.fewerBraces.parensToo &&
        (style.dialect.allowInfixOperatorAfterNL ||
          style.newlines.infix.sourceIgnoredAt(ft)(tree))) &&
      isSeqSingle(tree.values)
    if (!ok) return null

    tree.parent match {
      case Some(p: Term.Apply) if (p.parent match {
            case Some(pp: Term.Apply) => pp.fun ne p
            case _ => true
          }) =>
        val x = ft.right // `{` or `(`
        replaceToken(":")(new T.Colon(x.input, x.dialect, x.start))
      case _ => null
    }
  }

  private[rewrite] def onLeftForArgClause(lft: FT, left: Replacement)(
      tree: Term.ArgClause,
  ): Replacement = {
    implicit val ft: FT = lft
    implicit val style: ScalafmtConfig = left.style
    val repl = onLeftForArgClause(tree)
    if (repl ne null) {
      repl.copySpanFrom(left)
      repl.advanceSpanRange(left.idx + 1, lft.idx, ftoks.arr)
    }
    repl
  }

  private def shouldRewriteColonOnRight(
      left: Replacement,
  )(implicit session: Session, style: ScalafmtConfig): Boolean = {
    def shouldRewriteArgClause(ac: Term.ArgClause): Boolean =
      0 == ac.values.lengthCompare(1) &&
        settings.fewerBraces.span.satisfied(session.getSpan(left))
    val lft = left.ft
    lft.meta.rightOwner match {
      case t: Term.ArgClause => shouldRewriteArgClause(t)
      case t @ (_: Term.Block | _: Term.PartialFunction) => t.parent match {
          case Some(p: Term.ArgClause) => shouldRewriteArgClause(p)
          case _ => false
        }
      case _ => true // template etc
    }
  }

}
