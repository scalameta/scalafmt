package org.scalafmt.internal

import org.scalafmt.config.RewriteSettings.InsertBraces
import org.scalafmt.util.TreeOps

import scala.meta.tokens.{Token => T}
import scala.meta.{Ctor, Defn, Term, Tree}

object MissingBraces {

  type AllRange = (Tree, Tree)
  type AllRanges = Seq[AllRange]

  case class Result(tree: Tree, all: AllRanges = Nil, non: AllRanges = Nil)
  type ResultOpt = Option[Result]

  private trait Factory {
    def getBlocks(ft: FT, nft: FT)(implicit
        ftoks: FormatTokens,
        ib: InsertBraces,
    ): ResultOpt
  }

  def getBlocks(
      ft: FT,
  )(implicit ftoks: FormatTokens, ib: InsertBraces): ResultOpt = {
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
        case _: T.KwThen => ThenImpl
        case _: T.KwElse => ElseImpl
        case _: T.KwYield => YieldImpl
        case _ => null
      }
      Option(impl).flatMap(_.getBlocks(ft, nft))
    }
  }

  private def rng(t: Tree): AllRange = t -> t
  private def rng(t: Seq[Tree]): Option[AllRange] =
    if (t.isEmpty) None else Some(t.head -> t.last)

  private def seq(ok: Boolean, t: Tree): AllRanges =
    if (ok) Seq(rng(t)) else Nil

  private def seqopt(ok: Boolean, t: Option[Tree]*): AllRanges =
    if (ok) t.flatMap(_.map(rng)) else Nil

  private def seqseq(ok: Boolean, t: => Seq[Tree]): AllRanges =
    if (ok) rng(t).toSeq else Nil

  private object BlockImpl extends Factory {
    def getBlocks(ft: FT, nft: FT)(implicit
        ftoks: FormatTokens,
        ib: InsertBraces,
    ): ResultOpt = {
      def ok(stat: Tree): Boolean = ftoks.isJustBeforeTree(nft)(stat)
      val leftOwner = ft.meta.leftOwner
      TreeOps.findTreeWithParentSimple(nft.rightOwner)(_ eq leftOwner) match {
        case Some(t: Term.Block) =>
          if (t.stats.headOption.exists(ok)) Some(Result(t)) else None
        case x => x.filter(ok).map(Result(_))
      }
    }
  }

  private object RightArrowImpl extends Factory {
    def getBlocks(ft: FT, nft: FT)(implicit
        ftoks: FormatTokens,
        ib: InsertBraces,
    ): ResultOpt = ft.meta.leftOwner match {
      case t: Term.FunctionLike =>
        val skip = t.parent.exists(TreeOps.isExprWithParentInBraces(t))
        if (skip) None
        else Some(Result(t.body, non = seqseq(ib.nonBlocks, t.paramClause.values)))
      case _ => None
    }
  }

  private object RightParenImpl extends Factory {
    def getBlocks(ft: FT, nft: FT)(implicit
        ftoks: FormatTokens,
        ib: InsertBraces,
    ): ResultOpt = ft.meta.leftOwner match {
      case x: Term.If if !nft.right.is[T.KwThen] => ThenImpl.getTermIf(x)
      case t: Term.EnumeratorsBlock
          if !nft.right.is[T.KwDo] && ftoks.getLastOpt(t).contains(ft) =>
        t.parent match {
          case Some(p: Term.For) =>
            Some(Result(p.body, all = seq(ib.allBlocks, t)))
          case _ => None
        }
      case t: Term.While if !nft.right.is[T.KwDo] =>
        Some(Result(t.body, non = seq(ib.nonBlocks, t.cond)))
      case _ => None
    }
  }

  private object RightBraceImpl extends Factory {
    def getBlocks(ft: FT, nft: FT)(implicit
        ftoks: FormatTokens,
        ib: InsertBraces,
    ): ResultOpt = ft.meta.leftOwner match {
      case t: Term.EnumeratorsBlock
          if !nft.right.is[T.KwDo] && ftoks.getLastOpt(t).contains(ft) =>
        t.parent match {
          case Some(p: Term.For) =>
            Some(Result(p.body, all = seq(ib.allBlocks, t)))
          case _ => None
        }
      case _ => None
    }
  }

  private object DoImpl extends Factory {
    def getBlocks(ft: FT, nft: FT)(implicit
        ftoks: FormatTokens,
        ib: InsertBraces,
    ): ResultOpt = ft.meta.leftOwner match {
      case t: Term.Do => Some(Result(t.body, non = seq(ib.nonBlocks, t.expr)))
      case t: Term.For =>
        Some(Result(t.body, all = seq(ib.allBlocks, t.enumsBlock)))
      case _ => None
    }
  }

  private object EqualsImpl extends Factory {
    def getBlocks(ft: FT, nft: FT)(implicit
        ftoks: FormatTokens,
        ib: InsertBraces,
    ): ResultOpt = ft.meta.leftOwner match {
      case _: Defn.Type => None
      case t: Ctor.Secondary =>
        if (t.body.stats.nonEmpty) None // must have braces
        else Some(Result(t.body.init, non = seqseq(ib.nonBlocks, t.paramClauses)))
      case t: Tree.WithBody =>
        val nonBlocks = t match {
          case _ if !ib.nonBlocks => Nil
          case t: Defn.Def if !TreeOps.isJsNative(t.body) =>
            rng(t.paramClauseGroups).toSeq
          case t: Defn.Val => rng(t.pats).toSeq
          case t: Defn.Var => rng(t.pats).toSeq
          case _ => Nil
        }
        Some(Result(t.body, non = nonBlocks))
      case _ => BlockImpl.getBlocks(ft, nft)
    }
  }

  private object TryImpl extends Factory {
    def getBlocks(ft: FT, nft: FT)(implicit
        ftoks: FormatTokens,
        ib: InsertBraces,
    ): ResultOpt = ft.meta.leftOwner match {
      case t: Term.TryClause =>
        Some(Result(t.expr, all = seqopt(ib.allBlocks, t.catchClause, t.finallyp)))
      case _ => None
    }
  }

  private object CatchImpl extends Factory {
    def getBlocks(ft: FT, nft: FT)(implicit
        ftoks: FormatTokens,
        ib: InsertBraces,
    ): ResultOpt = ft.meta.leftOwner match {
      case t: Term.TryClause => t.catchClause
          .map(Result(_, all = seqopt(ib.allBlocks, Some(t.expr), t.finallyp)))
      case _ => None
    }
  }

  private object FinallyImpl extends Factory {
    def getBlocks(ft: FT, nft: FT)(implicit
        ftoks: FormatTokens,
        ib: InsertBraces,
    ): ResultOpt = ft.meta.leftOwner match {
      case t: Term.TryClause => t.finallyp
          .map(Result(_, all = seqopt(ib.allBlocks, Some(t.expr), t.catchClause)))
      case _ => None
    }
  }

  private object ThenImpl extends Factory {
    def getBlocks(ft: FT, nft: FT)(implicit
        ftoks: FormatTokens,
        ib: InsertBraces,
    ): ResultOpt = ft.meta.leftOwner match {
      case x: Term.If => getTermIf(x)
      case _ => None
    }

    def getTermIf(owner: Term.If)(implicit ib: InsertBraces): ResultOpt = Some(
      Result(
        owner.thenp,
        all = seq(ib.allBlocks && !TreeOps.ifWithoutElse(owner), owner.elsep),
        non = seq(ib.nonBlocks, owner.cond),
      ),
    )
  }

  private object ElseImpl extends Factory {
    def getBlocks(ft: FT, nft: FT)(implicit
        ftoks: FormatTokens,
        ib: InsertBraces,
    ): ResultOpt = ft.meta.leftOwner match {
      case x: Term.If if !x.elsep.is[Term.If] =>
        Some(Result(
          x.elsep,
          all = seq(ib.allBlocks, x.thenp),
          non = seq(ib.nonBlocks, x.cond),
        ))
      case _ => None
    }
  }

  private object YieldImpl extends Factory {
    def getBlocks(ft: FT, nft: FT)(implicit
        ftoks: FormatTokens,
        ib: InsertBraces,
    ): ResultOpt = ft.meta.leftOwner match {
      case t: Term.ForYield =>
        Some(Result(t.body, all = seq(ib.allBlocks, t.enumsBlock)))
      case _ => None
    }
  }

}
