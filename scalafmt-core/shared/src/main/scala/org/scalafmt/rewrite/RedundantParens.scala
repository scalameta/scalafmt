package org.scalafmt.rewrite

import scala.annotation.tailrec
import scala.meta._

object RedundantParens extends Rewrite {
  override def create(implicit ctx: RewriteCtx): RewriteSession =
    new RedundantParens
}

class RedundantParens(implicit ctx: RewriteCtx) extends RewriteSession {

  import ctx.dialect

  override def rewrite(tree: Tree): Unit =
    tree match {
      case t: Defn.Val => maybeRemove(t.rhs)

      case t: Defn.Def => maybeRemove(t.body)

      case t => rewriteFunc.applyOrElse(t, (x: Tree) => remove(x, true))
    }

  private val rewriteFunc: PartialFunction[Tree, Unit] = {
    case g: Enumerator.Guard => remove(g.cond)

    case t: Case => t.cond.foreach(remove(_))

    case t @ (_: Lit | _: Term.Name) => remove(t)

    case t @ Term.Apply(_, List(b: Term.Block))
        if ctx.style.activeForEdition_2020_01 &&
          b.tokens.headOption.exists(_.is[Token.LeftBrace]) =>
      val lastTok = t.tokens.last
      ctx.getMatchingOpt(lastTok).foreach(removeBetween(_, lastTok))
  }

  private def maybeRemove(tree: Tree): Unit =
    if (!rewriteFunc.isDefinedAt(tree)) remove(tree)

  private def remove(tree: Tree, inner: Boolean = false): Unit =
    removeByTokens(tree.tokens, inner)

  private def removeByTokens(toks: Tokens, inner: Boolean = false): Unit =
    toks.headOption.foreach { head =>
      val beg = ctx.getIndex(head)
      removeParensByIndex(beg until (beg + toks.length), inner)
    }

  private def removeBetween(b: Token, e: Token): Unit =
    removeParensByIndex(ctx.getIndex(b) to ctx.getIndex(e), false)

  private def removeParensByIndex(range: Range, inner: Boolean): Unit = {
    val beg = range.head
    val end = range.last
    val mid = (beg + end) / 2
    val toks = ctx.tokens
    @tailrec
    def getOutOfRange(off: Int): Int = {
      val idx = beg + off
      if (idx >= mid) off
      else
        toks(idx) match {
          case lp: Token.LeftParen =>
            ctx.getMatchingOpt(lp) match {
              case Some(rp) if toks(end - off) eq rp => getOutOfRange(off + 1)
              case _ => off
            }
          case _ => off
        }
    }
    val offEnd = getOutOfRange(0) - 1
    if (offEnd >= 0) {
      val offBeg = if (inner) 1 else 0
      if (offBeg <= offEnd) {
        implicit val builder = Seq.newBuilder[TokenPatch]
        (offBeg to offEnd).foreach { x =>
          builder += TokenPatch.Remove(toks(beg + x))
          builder += TokenPatch.Remove(toks(end - x))
        }
        ctx.removeLFToAvoidEmptyLine(toks(beg + offBeg), toks(beg + offEnd))
        ctx.removeLFToAvoidEmptyLine(toks(end - offEnd), toks(end - offBeg))
        ctx.addPatchSet(builder.result(): _*)
      }
    }
  }

}
