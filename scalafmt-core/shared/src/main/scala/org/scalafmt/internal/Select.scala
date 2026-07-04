package org.scalafmt
package internal

import org.scalafmt.config.ScalafmtConfig

import scala.meta._
import scala.meta.tokens.{Token => T}

import scala.annotation.tailrec

class Select(val tree: Term, val qual: Term, val nameFt: FT) {}

object Select {
  def apply(tree: Term.Select)(implicit ftoks: FormatTokens): Select =
    new Select(tree, tree.qual, ftoks.getHead(tree.name))
  def apply(tree: Term.SelectMatch, kw: FT): Select =
    new Select(tree, tree.expr, kw)

  def get(ro: Tree)(
      onMatch: Term.SelectMatch => FT,
  )(implicit ftoks: FormatTokens): Select = ro match {
    case x: Term.Select => apply(x)
    case x: Term.SelectMatch => onMatch(x).nnMap(apply(x, _))
    case _ => null
  }

  def getOrNull(ro: Tree)(
      onMatch: Term.SelectMatch => FT,
  )(implicit ftoks: FormatTokens): Select = ro match {
    case x: Term.Select => apply(x)
    case x: Term.SelectMatch => onMatch(x).nnMap(apply(x, _))
    case _ => null
  }

  def onRightOrNull(ft: FT)(implicit ftoks: FormatTokens): Select =
    getOrNull(ft.rightOwner) { _ =>
      val nft = ftoks.nextNonCommentAfter(ft)
      if (nft.right.is[T.KwMatch]) ftoks.next(nft) else null
    }

  def unapply(tree: Tree)(implicit ftoks: FormatTokens): Option[Select] =
    Option(Select.get(tree)(x => ftoks.tokenBefore(x.casesBlock)))

  @tailrec
  final def first(tree: Tree, enclosed: Boolean, select: Select = null)(implicit
      ftoks: FormatTokens,
  ): Select = prevAndApply(tree, enclosed) match {
    case (prevSelect, _) if prevSelect ne null =>
      first(prevSelect.qual, enclosed, prevSelect)
    case _ => select
  }

  @tailrec
  final def prevAndApply(
      tree: Tree,
      enclosed: Boolean,
      applyTree: Member.Apply = null,
  )(implicit ftoks: FormatTokens): (Select, Member.Apply) = {
    @inline
    def isEnclosed: Boolean = enclosed && ftoks.isEnclosedWithinParens(tree)
    tree match {
      case Select(t) if !isEnclosed => (t, applyTree)
      case t: Member.Apply if !isEnclosed =>
        prevAndApply(t.fun, enclosed, if (applyTree ne null) applyTree else t)
      case t: Term.AnonymousFunction if !enclosed =>
        prevAndApply(t.body, false, applyTree)
      case Term.Block(t :: Nil) if !ftoks.isEnclosedInBraces(tree) =>
        prevAndApply(t, false, applyTree)
      case _ => (null, applyTree)
    }
  }

  final def prevAndApply(tree: Tree)(implicit
      ftoks: FormatTokens,
      style: ScalafmtConfig,
  ): (Select, Member.Apply) =
    prevAndApply(tree, style.newlines.encloseSelectChains)

  def prev(tree: Select, enclosed: Boolean)(implicit
      ftoks: FormatTokens,
  ): Select = prevAndApply(tree.qual, enclosed)._1

  def prev(
      tree: Select,
  )(implicit ftoks: FormatTokens, style: ScalafmtConfig): Select =
    prev(tree, style.newlines.encloseSelectChains)

}
