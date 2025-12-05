package org.scalafmt.internal

import org.scalafmt.util.TreeOps

import scala.meta.tokens.{Token => T}
import scala.meta.{Ctor, Defn, Term, Tree}

object MissingBraces {

  type Ranges = Seq[(Tree, Tree)]
  type Result = Option[(Tree, Ranges)]

  private trait Factory {
    def getBlocks(ft: FT, nft: FT, all: Boolean)(implicit
        ftoks: FormatTokens,
    ): Result
  }

  def getBlocks(ft: FT, all: Boolean)(implicit ftoks: FormatTokens): Result = {
    val nft = ftoks.nextNonComment(ft)
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

  private def seq(all: Boolean, t: Tree): Ranges = if (all) Seq(t -> t) else Nil

  private def seq(all: Boolean, t: Option[Tree]): Ranges = t.map(seq(all, _))
    .getOrElse(Nil)

  private def seq(all: Boolean, t: Seq[Tree]): Ranges =
    if (all && t.nonEmpty) Seq(t.head -> t.last) else Nil

  private object BlockImpl extends Factory {
    def getBlocks(ft: FT, nft: FT, all: Boolean)(implicit
        ftoks: FormatTokens,
    ): Result = {
      def ok(stat: Tree): Boolean = ftoks.isJustBeforeTree(nft)(stat)
      val leftOwner = ft.meta.leftOwner
      TreeOps.findTreeWithParentSimple(nft.rightOwner)(_ eq leftOwner) match {
        case Some(t: Term.Block) =>
          if (t.stats.headOption.exists(ok)) Some((t, Nil)) else None
        case x => x.filter(ok).map((_, Nil))
      }
    }
  }

  private object RightArrowImpl extends Factory {
    def getBlocks(ft: FT, nft: FT, all: Boolean)(implicit
        ftoks: FormatTokens,
    ): Result = ft.meta.leftOwner match {
      case t: Term.FunctionLike =>
        val skip = t.parent.exists(TreeOps.isExprWithParentInBraces(t))
        if (skip) None else Some((t.body, seq(all, t.paramClause.values)))
      case _ => None
    }
  }

  private object RightParenImpl extends Factory {
    def getBlocks(ft: FT, nft: FT, all: Boolean)(implicit
        ftoks: FormatTokens,
    ): Result = ft.meta.leftOwner match {
      case x: Term.If if !nft.right.is[T.KwThen] =>
        val hasElse = all && !TreeOps.ifWithoutElse(x)
        Some((x.thenp, seq(hasElse, x.elsep) ++ seq(all, x.cond)))
      case t: Term.EnumeratorsBlock
          if !nft.right.is[T.KwDo] && ftoks.getLastOpt(t).contains(ft) =>
        t.parent match {
          case Some(p: Term.For) => Some((p.body, seq(all, t)))
          case _ => None
        }
      case t: Term.While if !nft.right.is[T.KwDo] =>
        Some((t.body, seq(all, t.cond)))
      case _ => None
    }
  }

  private object RightBraceImpl extends Factory {
    def getBlocks(ft: FT, nft: FT, all: Boolean)(implicit
        ftoks: FormatTokens,
    ): Result = ft.meta.leftOwner match {
      case t: Term.EnumeratorsBlock
          if !nft.right.is[T.KwDo] && ftoks.getLastOpt(t).contains(ft) =>
        t.parent match {
          case Some(p: Term.For) => Some((p.body, seq(all, t)))
          case _ => None
        }
      case _ => None
    }
  }

  private object DoImpl extends Factory {
    def getBlocks(ft: FT, nft: FT, all: Boolean)(implicit
        ftoks: FormatTokens,
    ): Result = ft.meta.leftOwner match {
      case t: Term.Do => Some((t.body, seq(all, t.expr)))
      case _ => None
    }
  }

  private object EqualsImpl extends Factory {
    def getBlocks(ft: FT, nft: FT, all: Boolean)(implicit
        ftoks: FormatTokens,
    ): Result = ft.meta.leftOwner match {
      case _: Defn.Type => None
      case t: Ctor.Secondary => Some((t, seq(all, t.body)))
      case t: Tree.WithBody => Some((t.body, Nil))
      case _ => BlockImpl.getBlocks(ft, nft, all)
    }
  }

  private object TryImpl extends Factory {
    def getBlocks(ft: FT, nft: FT, all: Boolean)(implicit
        ftoks: FormatTokens,
    ): Result = ft.meta.leftOwner match {
      case t: Term.TryClause =>
        Some((t.expr, seq(all, t.catchClause) ++ seq(all, t.finallyp)))
      case _ => None
    }
  }

  private object CatchImpl extends Factory {
    def getBlocks(ft: FT, nft: FT, all: Boolean)(implicit
        ftoks: FormatTokens,
    ): Result = ft.meta.leftOwner match {
      case t: Term.TryClause => t.catchClause
          .map(x => (x, seq(all, t.expr) ++ seq(all, t.finallyp)))
      case _ => None
    }
  }

  private object FinallyImpl extends Factory {
    def getBlocks(ft: FT, nft: FT, all: Boolean)(implicit
        ftoks: FormatTokens,
    ): Result = ft.meta.leftOwner match {
      case t: Term.TryClause => t.finallyp
          .map(x => (x, seq(all, t.expr) ++ seq(all, t.catchClause)))
      case _ => None
    }
  }

  private object ElseImpl extends Factory {
    def getBlocks(ft: FT, nft: FT, all: Boolean)(implicit
        ftoks: FormatTokens,
    ): Result = ft.meta.leftOwner match {
      case x: Term.If if !x.elsep.is[Term.If] =>
        Some((x.elsep, seq(all, x.thenp) ++ seq(all, x.cond)))
      case _ => None
    }
  }

  private object YieldImpl extends Factory {
    def getBlocks(ft: FT, nft: FT, all: Boolean)(implicit
        ftoks: FormatTokens,
    ): Result = ft.meta.leftOwner match {
      case t: Term.ForYield => Some((t.body, seq(all, t.enumsBlock)))
      case _ => None
    }
  }

}
