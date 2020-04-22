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
      case g: Enumerator.Guard => remove(g.cond)

      case t @ Term.Apply(_, List(b: Term.Block))
          if ctx.style.activeForEdition_2020_01 &&
            b.tokens.headOption.exists(_.is[Token.LeftBrace]) =>
        val lastTok = t.tokens.last
        ctx.getMatchingOpt(lastTok).foreach(removeBetween(_, lastTok))

      case _ =>
    }

  private def remove(tree: Tree): Unit =
    removeByTokens(tree.tokens)

  private def removeByTokens(toks: Tokens): Unit =
    toks.headOption.foreach { head =>
      val beg = ctx.getIndex(head)
      removeParensByIndex(beg until (beg + toks.length))
    }

  private def removeBetween(b: Token, e: Token): Unit =
    removeParensByIndex(ctx.getIndex(b) to ctx.getIndex(e))

  private def removeParensByIndex(range: Range): Unit = {
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
      val offBeg = 0
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
