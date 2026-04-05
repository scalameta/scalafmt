package org.scalafmt.rewrite

import org.scalafmt.config._
import org.scalafmt.internal._
import org.scalafmt.util.TreeOps._

import scala.meta._
import scala.meta.tokens.{Token => T}

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
  ): Option[Replacement] = ft.right match {
    case x: T.LeftBrace => Option {
        ft.meta.rightOwner match {
          case _ if ftoks.nextNonCommentAfter(ft).right.is[T.RightBrace] => null
          case t: Term.Block if t.stats.nonEmpty =>
            val pft = ftoks.prevNonComment(ft)
            val removeRepl =
              if (settings.isRemoveEnabled) onLeftForBlock(t, pft) else null
            if (removeRepl ne null) removeRepl
            else if (
              isSingleStatBlock(t) && settings.isInsertEnabled &&
              session.rule[RedundantBraces].nonEmpty &&
              getOptionalBraces(pft).exists(_._3)
            ) Replacement(this, ft, ReplacementType.Replace, style)
            else null
          case _ if !settings.isRemoveEnabled => null
          case t: Template.Body if !t.isEmpty =>
            if (t.parent.parent.is[Defn.Given]) removeToken
            else replaceToken(":")(new T.Colon(x.input, x.dialect, x.start))
          case t: Term.ArgClause => onLeftForArgClause(t)
          case t: Term.PartialFunction => t.parent match {
              case Some(p: Term.ArgClause) if (p.tokens.head match {
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
      }
    case _ if !settings.isRemoveEnabled => None
    case _: T.LeftParen
        if !ftoks.nextNonCommentAfter(ft).right.is[T.RightParen] =>
      ft.meta.rightOwner match {
        case t: Term.ArgClause => Option(onLeftForArgClause(t))
        case _ => None
      }
    case _: T.Colon if settings.isInsertEnabled => insertOnToken(isColon = true)
    case _ => None
  }

  override def appendOnToken(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Option[Replacement] =
    if (!settings.isInsertEnabled || ft.right.is[T.Colon]) None
    else insertOnToken(isColon = false)

  private[rewrite] def getOptionalBraces(ft: FT)(implicit
      session: Session,
      style: ScalafmtConfig,
  ): Option[(OptionalBraces, FT, Boolean)] = OptionalBraces.getWithBrace(
    session.claimedRule(ft.idx - 1).flatMap(r =>
      if (r.how eq ReplacementType.Replace) Some(r.ft.right) else None,
    ).fold(ft)(x => ft.copy(left = x)),
  )

  private def insertOnToken(isColon: Boolean)(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Option[Replacement] = getOptionalBraces(ftoks.next(ft))
    .flatMap { case (ob, _, skip) =>
      val block = ob.block
      if (skip) None
      else if (block.parent.is[Case]) if (isColon) Some(removeToken) else None
      else ftoks.getLastOpt(block).map { rb =>
        val rt = ft.right
        val lb = new T.LeftBrace(rt.input, rt.dialect, rt.start)
        if (isColon) replaceToken("{", afterRight = Some(rb.idx))(lb)
        else {
          val meta = ft.meta
            .copy(right = ft.meta.right.copy(text = "{", owner = block))
          val how = appendTokensType(ft.copy(right = lb, meta = meta))
          Replacement(this, ft, how, style, afterRight = Some(rb.idx))
        }
      }
    }

  override def onRight(left: Replacement, hasFormatOff: Boolean)(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Option[(Replacement, Replacement)] = {
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
  ): Option[(Replacement, Replacement)] = {
    val nextFt = ftoks.nextNonCommentAfter(ft)
    val notOkToRewrite = hasFormatOff || { // can't force significant indentation
      val cfg = settings
      !cfg.remove.forall { x =>
        val checks = Iterator(
          (x.maxSpan, (max: Int) => session.getSpan(left) <= max),
          (x.maxBlankGaps, (max: Int) => session.getBlankGaps(left) <= max),
        ).flatMap { case (max, f) => if (max < 0) None else Some(f(max)) }
        if (!cfg.preferInsert) checks.contains(true)
        else checks.hasNext && !checks.contains(false)
      }
    } ||
      (nextFt.meta.rightOwner match {
        case t: Term.Name => t.parent.exists {
            case p: Term.SelectPostfix => p.name eq t // select without `.`
            case p: Term.ApplyInfix if p.op eq t =>
              !style.dialect.allowInfixOperatorAfterNL ||
              !t.tokens.head.isSymbolicInfixOperator
            case _ => false
          }
        case _: Term.Select => nextFt.noBreak &&
          (style.newlines.getSelectChains eq Newlines.keep)
        case _ => false
      }) ||
      (left.ft.right match {
        case _: T.Colon => !shouldRewriteColonOnRight(left)
        case _ => false
      })
    ft.right match {
      case _ if notOkToRewrite => None
      case _: T.RightParen if RewriteTrailingCommas.checkIfPrevious =>
        Some((left, removeToken))
      case x: T.RightBrace =>
        val replacement = ft.meta.rightOwner match {
          case t: Term.EnumeratorsBlock
              if allowOldSyntax && t.parent.is[Term.For] &&
                !nextFt.right.is[T.KwDo] =>
            replaceToken("do", t.parent)(new T.KwDo(x.input, x.dialect, x.start))
          case _ => removeToken
        }
        Some((left, replacement))
      case _ => None
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

      val checkSpan = (x: Int) =>
        if (isSingleStatBlock) session.getSpan(left) >= x.max(style.maxColumn)
        else x == 0 || session.getSpan(left) >= x
      val checkBlankGaps = (x: Int) =>
        if (isSingleStatBlock) session.getBlankGaps(left) >= x.max(1)
        else x == 0 || session.getBlankGaps(left) >= x
      val checks =
        Iterator((ib.minSpan, checkSpan), (ib.minBlankGaps, checkBlankGaps))
          .flatMap { case (min, f) => if (min < 0) None else Some(f(min)) }
      if (cfg.preferInsert) checks.contains(true)
      else checks.hasNext && !checks.contains(false)
    }
  }

  private def onRightToBraces(left: Replacement, hasFormatOff: Boolean)(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Option[(Replacement, Replacement)] = {
    val notOkToRewrite = skipRightToBraces(left)
    // had the braces already but RedundantBraces is on
    val maybeKeep =
      (left.how eq ReplacementType.Replace) && (ftoks(left.idx) eq left.ft)
    if (maybeKeep)
      if (notOkToRewrite) session.rule[RedundantBraces].flatMap { r =>
        val rbRepl = {
          implicit val ft: FT = left.ft
          implicit val style: ScalafmtConfig = left.style
          r.onLeftBrace(ft.rightOwner)
        }
        if (rbRepl eq null) None else r.onRight(rbRepl, hasFormatOff)
      }
      else None
    else if (notOkToRewrite) None
    else {
      val rt = ft.right
      val rbt = new T.RightBrace(rt.input, rt.dialect, rt.end)
      val lb = left.how match {
        case how: ReplacementType.AppendAfter => how.ft
        case _ => left.ft
      }
      val rbmeta = lb.meta.copy(right = lb.meta.right.copy(text = "}"))
      val replType = appendTokensType(FT(rt, rbt, rbmeta))
      Some((left, Replacement(this, ft, replType, style)))
    }
  }

  @tailrec
  private def onLeftForBlock(tree: Term.Block, pft: FT)(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Replacement = tree.parent.orNull match {
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
    case p: Term.ArgClause => p.tokens.head match {
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
  )(implicit ft: FT, session: Session, tag: ClassTag[A]) = {
    val prevFt = ftoks.prevNonComment(ft)
    prevFt.left.eq(lp) && session.claimedRule(prevFt.meta.idx - 1)
      .exists(x => tag.runtimeClass.isInstance(x.rule))
  }

  private[rewrite] def onLeftForArgClause(
      tree: Term.ArgClause,
  )(implicit ft: FT, style: ScalafmtConfig): Replacement = {
    val cfg = settings
    val ok = style.dialect.allowFewerBraces && cfg.fewerBraces.maxSpan > 0 &&
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
      0 == ac.values.lengthCompare(1) && {
        val rob = settings
        val span = session.getSpan(left)
        span >= rob.fewerBraces.minSpan && span <= rob.fewerBraces.maxSpan
      }
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
