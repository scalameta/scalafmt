package org.scalafmt.internal

import org.scalafmt.config.ScalafmtConfig

import scala.meta.tokens.{Token => T}
import scala.meta.{Member, Term, Tree}

import scala.annotation.tailrec

class Select(val tree: Term, val qual: Term, val nameFt: FT) {}

object Select {
  def apply(tree: Term.Select)(implicit ftoks: FormatTokens): Select =
    new Select(tree, tree.qual, ftoks.getHead(tree.name))
  def apply(tree: Term.SelectMatch, kw: FT): Select =
    new Select(tree, tree.expr, kw)

  def get(ro: Tree)(
      onMatch: Term.SelectMatch => Option[FT],
  )(implicit ftoks: FormatTokens): Option[Select] = ro match {
    case x: Term.Select => Some(apply(x))
    case x: Term.SelectMatch => onMatch(x).map(apply(x, _))
    case _ => None
  }

  def onRightOpt(ft: FT)(implicit ftoks: FormatTokens): Option[Select] =
    get(ft.rightOwner) { _ =>
      val nft = ftoks.nextNonCommentAfter(ft)
      if (nft.right.is[T.KwMatch]) Some(ftoks.next(nft)) else None
    }

  def unapply(tree: Tree)(implicit ftoks: FormatTokens): Option[Select] = Select
    .get(tree)(x => Some(ftoks.tokenBefore(x.casesBlock)))(ftoks)

  @tailrec
  final def first(tree: Tree, enclosed: Boolean, select: Option[Select] = None)(
      implicit ftoks: FormatTokens,
  ): Option[Select] = prevAndApply(tree, enclosed) match {
    case (x @ Some(prevSelect), _) => first(prevSelect.qual, enclosed, x)
    case _ => select
  }

  @tailrec
  final def prevAndApply(
      tree: Tree,
      enclosed: Boolean,
      applyTree: Option[Member.Apply] = None,
  )(implicit ftoks: FormatTokens): (Option[Select], Option[Member.Apply]) = {
    @inline
    def isEnclosed: Boolean = enclosed && ftoks.isEnclosedWithinParens(tree)
    tree match {
      case Select(t) if !isEnclosed => (Some(t), applyTree)
      case t: Member.Apply if !isEnclosed =>
        prevAndApply(t.fun, enclosed, applyTree.orElse(Some(t)))
      case t: Term.AnonymousFunction if !enclosed =>
        prevAndApply(t.body, false, applyTree)
      case Term.Block(t :: Nil) if !ftoks.isEnclosedInBraces(tree) =>
        prevAndApply(t, false, applyTree)
      case _ => (None, applyTree)
    }
  }

  final def prevAndApply(tree: Tree)(implicit
      ftoks: FormatTokens,
      style: ScalafmtConfig,
  ): (Option[Select], Option[Member.Apply]) =
    prevAndApply(tree, style.newlines.encloseSelectChains)

  def prev(tree: Select, enclosed: Boolean)(implicit
      ftoks: FormatTokens,
  ): Option[Select] = prevAndApply(tree.qual, enclosed)._1

  def prev(
      tree: Select,
  )(implicit ftoks: FormatTokens, style: ScalafmtConfig): Option[Select] =
    prev(tree, style.newlines.encloseSelectChains)

}
