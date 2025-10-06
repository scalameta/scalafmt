package org.scalafmt.internal

import org.scalafmt.config.ScalafmtConfig

import scala.meta.Member
import scala.meta.Term
import scala.meta.Tree
import scala.meta.tokens.{Token => T}

import scala.annotation.tailrec

class SelectLike(val tree: Term, val qual: Term, val nameFt: FT) {}

object SelectLike {
  def apply(tree: Term.Select)(implicit ftoks: FormatTokens): SelectLike =
    new SelectLike(tree, tree.qual, ftoks.getHead(tree.name))
  def apply(tree: Term.SelectMatch, kw: FT): SelectLike =
    new SelectLike(tree, tree.expr, kw)

  def get(ro: Tree)(
      onMatch: Term.SelectMatch => Option[FT],
  )(implicit ftoks: FormatTokens): Option[SelectLike] = ro match {
    case x: Term.Select => Some(apply(x))
    case x: Term.SelectMatch => onMatch(x).map(apply(x, _))
    case _ => None
  }

  def onRightOpt(ft: FT)(implicit ftoks: FormatTokens): Option[SelectLike] =
    get(ft.rightOwner) { _ =>
      val nft = ftoks.nextNonCommentAfter(ft)
      if (nft.right.is[T.KwMatch]) Some(ftoks.next(nft)) else None
    }

  def unapply(tree: Tree)(implicit ftoks: FormatTokens): Option[SelectLike] =
    SelectLike.get(tree)(x => Some(ftoks.tokenBefore(x.casesBlock)))(ftoks)

  @tailrec
  final def findFirst(
      tree: Tree,
      enclosed: Boolean,
      select: Option[SelectLike] = None,
  )(implicit ftoks: FormatTokens): Option[SelectLike] =
    findPrevAndApply(tree, enclosed) match {
      case (x @ Some(prevSelect), _) => findFirst(prevSelect.qual, enclosed, x)
      case _ => select
    }

  @tailrec
  final def findPrevAndApply(
      tree: Tree,
      enclosed: Boolean,
      applyTree: Option[Member.Apply] = None,
  )(implicit
      ftoks: FormatTokens,
  ): (Option[SelectLike], Option[Member.Apply]) = {
    @inline
    def isEnclosed: Boolean = enclosed && ftoks.isEnclosedWithinParens(tree)
    tree match {
      case SelectLike(t) if !isEnclosed => (Some(t), applyTree)
      case t: Member.Apply if !isEnclosed =>
        findPrevAndApply(t.fun, enclosed, applyTree.orElse(Some(t)))
      case t: Term.AnonymousFunction if !enclosed =>
        findPrevAndApply(t.body, false, applyTree)
      case Term.Block(t :: Nil) if !ftoks.isEnclosedInBraces(tree) =>
        findPrevAndApply(t, false, applyTree)
      case _ => (None, applyTree)
    }
  }

  final def findPrevAndApply(tree: Tree)(implicit
      ftoks: FormatTokens,
      style: ScalafmtConfig,
  ): (Option[SelectLike], Option[Member.Apply]) =
    findPrevAndApply(tree, style.newlines.encloseSelectChains)

  def findPrev(tree: SelectLike, enclosed: Boolean)(implicit
      ftoks: FormatTokens,
  ): Option[SelectLike] = findPrevAndApply(tree.qual, enclosed)._1

  def findPrev(
      tree: SelectLike,
  )(implicit ftoks: FormatTokens, style: ScalafmtConfig): Option[SelectLike] =
    findPrev(tree, style.newlines.encloseSelectChains)

}
