package org.scalafmt.rewrite

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.FormatToken
import org.scalafmt.internal.FormatTokens
import org.scalafmt.util.TreeOps._

import scala.meta._
import scala.meta.tokens.Token

import scala.reflect.ClassTag

object RemoveScala3OptionalBraces extends FormatTokensRewrite.RuleFactory {

  override def enabled(implicit style: ScalafmtConfig): Boolean =
    style.dialect.allowSignificantIndentation &&
      style.rewrite.scala3.removeOptionalBraces.enabled

  override def create(implicit ftoks: FormatTokens): FormatTokensRewrite.Rule =
    new RemoveScala3OptionalBraces

  override def priority: Int = 1

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
      ft: FormatToken,
      session: Session,
      style: ScalafmtConfig,
  ): Option[Replacement] = Option {
    ft.right match {
      case x: Token.LeftBrace // skip empty brace pairs
          if !ftoks.nextNonComment(ftoks.next(ft)).right.is[Token.RightBrace] =>
        ft.meta.rightOwner match {
          case t: Term.Block if t.stats.nonEmpty => onLeftForBlock(t)
          case t: Template if t.stats.nonEmpty || t.self.tokens.nonEmpty =>
            if (t.parent.exists(_.is[Defn.Given])) removeToken
            else replaceToken(":")(new Token.Colon(x.input, x.dialect, x.start))
          case t: Term.ArgClause => onLeftForArgClause(t)
          case t: Term.PartialFunction => t.parent match {
              case Some(p: Term.ArgClause) if (p.tokens.head match {
                    case px: Token.LeftBrace => px eq x
                    case px: Token.LeftParen =>
                      shouldRewriteArgClauseWithLeftParen[RedundantBraces](px)
                    case _ => false
                  }) => onLeftForArgClause(p)
              case _ => null
            }
          case _: Term.For if allowOldSyntax || {
                val rbFt = ftoks(ftoks.matching(ft.right))
                ftoks.nextNonComment(rbFt).right.is[Token.KwDo]
              } => removeToken
          case _: Term.ForYield => removeToken
          case _: Term.Match => removeToken
          case _: Type.Match => removeToken
          case _: Term.Try => removeToken
          case _: Ctor.Secondary
              if ftoks.prevNonComment(ft).left.is[Token.Equals] => removeToken
          case _ => null
        }
      case _ => null
    }
  }

  override def onRight(left: Replacement, hasFormatOff: Boolean)(implicit
      ft: FormatToken,
      session: Session,
      style: ScalafmtConfig,
  ): Option[(Replacement, Replacement)] = {
    val nextFt = ftoks.nextNonComment(ftoks.next(ft))
    val notOkToRewrite = hasFormatOff || // can't force significant indentation
      (nextFt.meta.rightOwner match {
        case t: Term.Name => t.parent.exists {
            case p: Term.Select => p.name eq t // select without `.`
            case p: Term.ApplyInfix if p.op eq t =>
              !style.dialect.allowInfixOperatorAfterNL ||
              !t.tokens.head.isSymbolicInfixOperator
            case _ => false
          }
        case _ => false
      }) ||
      (left.ft.right match {
        case _: Token.Colon => !shouldRewriteColonOnRight(left)
        case _ => false
      })
    ft.right match {
      case _ if notOkToRewrite => None
      case _: Token.RightParen if RewriteTrailingCommas.checkIfPrevious =>
        Some((left, removeToken))
      case x: Token.RightBrace =>
        val replacement = ft.meta.rightOwner match {
          case _: Term.For if allowOldSyntax && !nextFt.right.is[Token.KwDo] =>
            replaceToken("do")(new Token.KwDo(x.input, x.dialect, x.start))
          case _ => removeToken
        }
        Some((left, replacement))
      case _ => None
    }
  }

  private def onLeftForBlock(tree: Term.Block)(implicit
      ft: FormatToken,
      session: Session,
      style: ScalafmtConfig,
  ): Replacement = tree.parent.fold(null: Replacement) {
    case t: Term.If =>
      val ok = ftoks.prevNonComment(ft).left match {
        case _: Token.KwIf => true
        case _: Token.KwThen => true
        case _: Token.KwElse => !isTreeMultiStatBlock(t.elsep) ||
          allowOldSyntax || ftoks.tokenAfter(t.cond).right.is[Token.KwThen]
        case _: Token.RightParen => allowOldSyntax
        case _ => false
      }
      if (ok) removeToken else null
    case _: Term.While =>
      val ok = ftoks.prevNonComment(ft).left match {
        case _: Token.KwDo => true
        case _: Token.RightParen => allowOldSyntax
        case _ => false
      }
      if (ok) removeToken else null
    case _: Term.For =>
      val ok = ftoks.prevNonComment(ft).left match {
        case _: Token.KwDo => true
        case _: Token.RightParen | _: Token.RightBrace => allowOldSyntax
        case _ => false
      }
      if (ok) removeToken else null
    case _: Term.ForYield => removeToken
    case _: Term.Try => removeToken
    case _: Term.Throw => removeToken
    case _: Term.Return => removeToken
    case _: Defn.ExtensionGroup => removeToken
    case _: Term.FunctionTerm => removeToken
    case t: Defn.Def =>
      if (tree ne t.body) null
      else if (ftoks.prevNonComment(ft).left.is[Token.Equals]) removeToken
      else null
    case p: Tree.WithBody => if (p.body eq tree) removeToken else null
    case p: Term.ArgClause => p.tokens.head match {
        case _: Token.LeftBrace => onLeftForArgClause(p)
        case px: Token.LeftParen
            if shouldRewriteArgClauseWithLeftParen[RedundantParens](px) =>
          onLeftForArgClause(p)
        case _ => null
      }
    case _ => null
  }

  private def shouldRewriteArgClauseWithLeftParen[A <: Rule](
      lp: Token,
  )(implicit ft: FormatToken, session: Session, tag: ClassTag[A]) = {
    val prevFt = ftoks.prevNonComment(ft)
    prevFt.left.eq(lp) && session.claimedRule(prevFt.meta.idx - 1)
      .exists(x => tag.runtimeClass.isInstance(x.rule))
  }

  private[rewrite] def onLeftForArgClause(
      tree: Term.ArgClause,
  )(implicit ft: FormatToken, style: ScalafmtConfig): Replacement = {
    val ok = style.dialect.allowFewerBraces &&
      style.rewrite.scala3.removeOptionalBraces.fewerBracesMaxSpan > 0 &&
      isSeqSingle(tree.values)
    if (!ok) return null

    tree.parent match {
      case Some(p: Term.Apply) if (p.parent match {
            case Some(pp: Term.Apply) => pp.fun ne p
            case _ => true
          }) =>
        val x = ft.right // `{` or `(`
        replaceToken(":")(new Token.Colon(x.input, x.dialect, x.start))
      case _ => null
    }
  }

  private def shouldRewriteColonOnRight(left: Replacement)(implicit
      ft: FormatToken,
      session: Session,
      style: ScalafmtConfig,
  ): Boolean = {
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
      lft: FormatToken,
  )(implicit
      ft: FormatToken,
      session: Session,
      style: ScalafmtConfig,
  ): Boolean = ac.values match {
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
            if (tok.is[Token.Whitespace]) None else Some(tok)
        }
        tokOpt.foreach { tok =>
          span += tok.end - tok.start
          if (span > maxStats) return false // RETURNING!!!
        }
      }
      span >= rob.fewerBracesMinSpan
    case _ => false
  }

}
