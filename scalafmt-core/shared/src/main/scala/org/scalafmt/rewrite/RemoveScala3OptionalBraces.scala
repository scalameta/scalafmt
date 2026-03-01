package org.scalafmt.rewrite

import org.scalafmt.config._
import org.scalafmt.internal._
import org.scalafmt.util.TreeOps._

import scala.meta._
import scala.meta.tokens.{Token => T}

import scala.annotation.tailrec
import scala.reflect.ClassTag

object RemoveScala3OptionalBraces extends FormatTokensRewrite.RuleFactory {

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    style.dialect.allowSignificantIndentation &&
      style.rewrite.scala3.removeOptionalBraces.enabled

  override def create(implicit ftoks: FormatTokens): FormatTokensRewrite.Rule =
    new RemoveScala3OptionalBraces

}

private class RemoveScala3OptionalBraces(implicit val ftoks: FormatTokens)
    extends FormatTokensRewrite.Rule {

  import FormatTokensRewrite._

  private def allowOldSyntax(implicit style: ScalafmtConfig): Boolean =
    ConvertToNewScala3Syntax.enabled ||
      style.rewrite.scala3.removeOptionalBraces.oldSyntaxToo

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    RemoveScala3OptionalBraces.enabled

  override def onToken(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Option[Replacement] = ft.right match {
    case x: T.LeftBrace // skip empty brace pairs
        if !ftoks.nextNonCommentAfter(ft).right.is[T.RightBrace] =>
      Option {
        ft.meta.rightOwner match {
          case t: Term.Block if t.stats.nonEmpty =>
            onLeftForBlock(t, ftoks.prevNonComment(ft))
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
    case _: T.LeftParen
        if !ftoks.nextNonCommentAfter(ft).right.is[T.RightParen] =>
      ft.meta.rightOwner match {
        case t: Term.ArgClause => Option(onLeftForArgClause(t))
        case _ => None
      }
    case _ => None
  }

  override def onRight(left: Replacement, hasFormatOff: Boolean)(implicit
      ft: FT,
      session: Session,
      style: ScalafmtConfig,
  ): Option[(Replacement, Replacement)] = {
    val nextFt = ftoks.nextNonCommentAfter(ft)
    val notOkToRewrite = hasFormatOff || // can't force significant indentation
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
    def okLeftDelim = ft.right.is[T.LeftBrace] ||
      style.rewrite.scala3.removeOptionalBraces.fewerBracesParensToo &&
      (style.dialect.allowInfixOperatorAfterNL ||
        style.newlines.infix.sourceIgnoredAt(ft)(tree))
    val ok = style.dialect.allowFewerBraces && okLeftDelim &&
      style.rewrite.scala3.removeOptionalBraces.fewerBracesMaxSpan > 0 &&
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
    onLeftForArgClause(tree)
  }

  private def shouldRewriteColonOnRight(
      left: Replacement,
  )(implicit ft: FT, session: Session, style: ScalafmtConfig): Boolean = {
    val lft = left.ft
    lft.meta.rightOwner match {
      case t: Term.ArgClause => shouldRewriteArgClauseColonOnRight(t, lft)
      case t @ (_: Term.Block | _: Term.PartialFunction) => t.parent match {
          case Some(p: Term.ArgClause) =>
            shouldRewriteArgClauseColonOnRight(p, lft)
          case _ => false
        }
      case _ => true // template etc
    }
  }

  private def shouldRewriteArgClauseColonOnRight(
      ac: Term.ArgClause,
      lft: FT,
  )(implicit ft: FT, session: Session, style: ScalafmtConfig): Boolean =
    ac.values match {
      case arg :: Nil =>
        val begIdx = math.max(ftoks.getHead(arg).meta.idx - 1, lft.meta.idx + 1)
        val endIdx = math.min(ftoks.getLast(arg).meta.idx, ft.meta.idx)
        var span = 0
        val rob = style.rewrite.scala3.removeOptionalBraces
        val maxStats = rob.fewerBracesMaxSpan
        (begIdx until endIdx).foreach { idx =>
          val tokOpt = session.claimedRule(idx) match {
            case Some(x) if x.ft.meta.idx == idx =>
              if (x.how == ReplacementType.Remove) None else Some(x.ft.right)
            case _ =>
              val tok = ftoks(idx).right
              if (tok.is[T.Whitespace]) None else Some(tok)
          }
          tokOpt.foreach { tok =>
            span += tok.len
            if (span > maxStats) return false // RETURNING!!!
          }
        }
        span >= rob.fewerBracesMinSpan
      case _ => false
    }

}
