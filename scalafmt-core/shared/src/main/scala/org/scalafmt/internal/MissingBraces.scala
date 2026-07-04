package org.scalafmt
package internal

import org.scalafmt.config.RewriteSettings.InsertBraces
import org.scalafmt.util.TreeOps

import scala.meta._
import scala.meta.tokens.{Token => T}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object MissingBraces {

  type AllRange = (Tree, Tree)
  type AllRanges = Seq[AllRange]

  case class Result(tree: Tree, all: AllRanges = Nil, non: AllRanges = Nil)

  private trait Factory {
    def getBlocks(ft: FT, nft: FT)(implicit
        ftoks: FormatTokens,
        ib: InsertBraces,
    ): Result
  }

  def getBlocks(
      ft: FT,
  )(implicit ftoks: FormatTokens, ib: InsertBraces): Result = {
    // ascribed to the supertype: Scala 3 otherwise infers a union of the *Impl
    // singleton types (+ Null), on which `.nnMap` won't resolve
    val impl: Factory = ft.left match {
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
    impl &&& {
      val nft = ftoks.nextNonComment(ft)
      impl.getBlocks(ft, nft)
        .nnIf(r => (r.tree ne nft.rightOwner) || !nft.right.is[T.LeftBrace])
    }
  }

  private def rng(t: Tree): AllRange = t -> t
  private def rngs(t: Iterable[Tree]*): AllRanges = {
    val outer = t.iterator.map(_.iterator).filter(_.hasNext)
    if (outer.hasNext) {
      var inner = outer.next()
      val head: Tree = inner.next()
      var last = head
      while (outer.hasNext) inner = outer.next()
      while (inner.hasNext) last = inner.next()
      Seq(head -> last)
    } else Nil
  }

  private def seq(ok: Boolean, t: Tree): AllRanges =
    if (ok) Seq(rng(t)) else Nil

  private def seqopt(ok: Boolean, t: Option[Tree]*): AllRanges =
    if (ok) t.flatMap(_.map(rng)) else Nil

  private def seqseq(ok: Boolean, t: => Seq[Tree]): AllRanges =
    if (ok) rngs(t) else Nil

  private object BlockImpl extends Factory {
    def getBlocks(ft: FT, nft: FT)(implicit
        ftoks: FormatTokens,
        ib: InsertBraces,
    ): Result = {
      def ok(stat: Tree): Boolean = ftoks.isJustBeforeTree(nft)(stat)
      val leftOwner = ft.meta.leftOwner
      TreeOps.findTreeWithParentSimple(nft.rightOwner)(_ eq leftOwner) match {
        case t: Term.Block =>
          if (t.stats.headOption.exists(ok)) Result(t) else null
        case null => null
        case t => if (ok(t)) Result(t) else null
      }
    }
  }

  private object RightArrowImpl extends Factory {
    def getBlocks(ft: FT, nft: FT)(implicit
        ftoks: FormatTokens,
        ib: InsertBraces,
    ): Result = ft.meta.leftOwner match {
      case t: Term.FunctionLike
          if !t.parent.exists(TreeOps.isExprWithParentInBraces(t)) =>
        Result(t.body, non = seqseq(ib.nonBlocks, t.paramClause.values))
      case _ => null
    }
  }

  private object RightParenImpl extends Factory {
    def getBlocks(ft: FT, nft: FT)(implicit
        ftoks: FormatTokens,
        ib: InsertBraces,
    ): Result = ft.meta.leftOwner match {
      case x: Term.If if !nft.right.is[T.KwThen] => ThenImpl.getTermIf(x)
      case t: Term.EnumeratorsBlock
          if !nft.right.is[T.KwDo] && (ftoks.getLast(t) eq ft) =>
        t.parent match {
          case Some(p: Term.For) => Result(p.body, all = seq(ib.allBlocks, t))
          case _ => null
        }
      case t: Term.While if !nft.right.is[T.KwDo] =>
        Result(t.body, non = seq(ib.nonBlocks, t.cond))
      case _ => null
    }
  }

  private object RightBraceImpl extends Factory {
    def getBlocks(ft: FT, nft: FT)(implicit
        ftoks: FormatTokens,
        ib: InsertBraces,
    ): Result = ft.meta.leftOwner match {
      case t: Term.EnumeratorsBlock
          if !nft.right.is[T.KwDo] && (ftoks.getLast(t) eq ft) =>
        t.parent match {
          case Some(p: Term.For) => Result(p.body, all = seq(ib.allBlocks, t))
          case _ => null
        }
      case _ => null
    }
  }

  private object DoImpl extends Factory {
    def getBlocks(ft: FT, nft: FT)(implicit
        ftoks: FormatTokens,
        ib: InsertBraces,
    ): Result = ft.meta.leftOwner match {
      case t: Term.Do => Result(t.body, non = seq(ib.nonBlocks, t.expr))
      case t: Term.For => Result(t.body, all = seq(ib.allBlocks, t.enumsBlock))
      case _ => null
    }
  }

  private object EqualsImpl extends Factory {
    def getBlocks(ft: FT, nft: FT)(implicit
        ftoks: FormatTokens,
        ib: InsertBraces,
    ): Result = ft.meta.leftOwner match {
      case _: Defn.Type => null
      case t: Ctor.Secondary =>
        if (t.body.stats.nonEmpty) null // must have braces
        else Result(t.body.init, non = seqseq(ib.nonBlocks, t.paramClauses))
      case t: Tree.WithBody =>
        val nonBlocks = t match {
          case _ if !ib.nonBlocks => Nil
          case t: Defn.Def if !TreeOps.isJsNative(t.body) =>
            rngs(t.paramClauseGroups, t.decltpe)
          case t: Defn.Val => rngs(t.pats, t.decltpe)
          case t: Defn.Var => rngs(t.pats, t.decltpe)
          case _ => Nil
        }
        Result(t.body, non = nonBlocks)
      case _ => BlockImpl.getBlocks(ft, nft)
    }
  }

  private object TryImpl extends Factory {
    def getBlocks(ft: FT, nft: FT)(implicit
        ftoks: FormatTokens,
        ib: InsertBraces,
    ): Result = ft.meta.leftOwner match {
      case t: Term.TryClause =>
        Result(t.expr, all = seqopt(ib.allBlocks, t.catchClause, t.finallyp))
      case _ => null
    }
  }

  private object CatchImpl extends Factory {
    def getBlocks(ft: FT, nft: FT)(implicit
        ftoks: FormatTokens,
        ib: InsertBraces,
    ): Result = ft.meta.leftOwner match {
      case t: Term.TryClause => t.catchClause match {
          case Some(c) =>
            Result(c, all = seqopt(ib.allBlocks, Some(t.expr), t.finallyp))
          case None => null
        }
      case _ => null
    }
  }

  private object FinallyImpl extends Factory {
    def getBlocks(ft: FT, nft: FT)(implicit
        ftoks: FormatTokens,
        ib: InsertBraces,
    ): Result = ft.meta.leftOwner match {
      case t: Term.TryClause => t.finallyp match {
          case Some(f) =>
            Result(f, all = seqopt(ib.allBlocks, Some(t.expr), t.catchClause))
          case None => null
        }
      case _ => null
    }
  }

  private object ThenImpl extends Factory {
    def getBlocks(ft: FT, nft: FT)(implicit
        ftoks: FormatTokens,
        ib: InsertBraces,
    ): Result = ft.meta.leftOwner match {
      case x: Term.If => getTermIf(x)
      case _ => null
    }

    def getTermIf(
        owner: Term.If,
    )(implicit ftoks: FormatTokens, ib: InsertBraces): Result = {
      val all = ListBuffer.empty[AllRange]
      val non = ListBuffer.empty[AllRange]
      if (ib.allBlocks) {
        non += rng(owner.cond)
        getAllElseDn(owner, all, non)
        ElseImpl.getAllThenUp(owner, all, non)
      }
      Result(owner.thenp, all = all.toList, non = non.toList)
    }

    @tailrec
    def getAllElseDn(
        owner: Term.If,
        all: ListBuffer[AllRange],
        non: ListBuffer[AllRange],
    )(implicit ftoks: FormatTokens, ib: InsertBraces): Unit =
      if (!TreeOps.ifWithoutElse(owner)) TreeOps.getBlockStat(owner.elsep) match {
        case x: Term.If =>
          non += rng(x.cond)
          all += rng(x.thenp)
          getAllElseDn(x, all, non)
        case x => all += rng(x)
      }

  }

  private object ElseImpl extends Factory {
    def getBlocks(ft: FT, nft: FT)(implicit
        ftoks: FormatTokens,
        ib: InsertBraces,
    ): Result = ft.meta.leftOwner match {
      case x: Term.If if !x.elsep.is[Term.If] =>
        val all = ListBuffer.empty[AllRange]
        val non = ListBuffer.empty[AllRange]
        if (ib.allBlocks) getAllThenUp(x.elsep, all, non)
        Result(x.elsep, all = all.toList, non = non.toList)
      case _ => null
    }

    @tailrec
    def getAllThenUp(
        owner: Term,
        all: ListBuffer[AllRange],
        non: ListBuffer[AllRange],
    )(implicit ftoks: FormatTokens, ib: InsertBraces): Unit = owner.parent match {
      case Some(p: Term.If) if p.elsep eq owner =>
        non += rng(p.cond)
        all += rng(p.thenp)
        getAllThenUp(p, all, non)
      case Some(p: Term.Block) if !ftoks.isEnclosedInBraces(p) =>
        getAllThenUp(p, all, non)
      case _ =>
    }
  }

  private object YieldImpl extends Factory {
    def getBlocks(ft: FT, nft: FT)(implicit
        ftoks: FormatTokens,
        ib: InsertBraces,
    ): Result = ft.meta.leftOwner match {
      case t: Term.ForYield =>
        Result(t.body, all = seq(ib.allBlocks, t.enumsBlock))
      case _ => null
    }
  }

}
