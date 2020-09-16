package org.scalafmt.rewrite

import scala.annotation.tailrec
import scala.meta._

import org.scalafmt.util.InfixApp

object RedundantParens extends Rewrite {
  override def create(implicit ctx: RewriteCtx): RewriteSession =
    new RedundantParens
}

class RedundantParens(implicit ctx: RewriteCtx) extends RewriteSession {

  import ctx.dialect

  private val usesAvoidInfix = ctx.style.rewrite.rules.contains(AvoidInfix)

  override def rewrite(tree: Tree): Unit =
    tree match {
      case t: Defn.Val => maybeRemove(t.rhs)

      case t: Defn.Def => maybeRemove(t.body)

      case t => rewriteFunc.applyOrElse(t, (x: Tree) => remove(x, 1))
    }

  private[rewrite] val rewriteFunc: PartialFunction[Tree, Unit] = {
    case t @ (_: Term.Tuple | _: Type.Tuple | _: Lit.Unit) => remove(t, 2)

    case g: Enumerator.Guard => maybeRemovePostfix(g.cond)

    case t: Case => t.cond.foreach(maybeRemovePostfix)

    case t if usesAvoidInfix && t.parent.exists {
          case p: Term.ApplyInfix => p.lhs ne t
          case _ => false
        } => // noop, but blocks Term.Name below

    case t @ (_: Lit | _: Term.Name | _: Term.Interpolate) => remove(t)

    case t @ Term.Apply(_, List(b: Term.Block))
        if b.tokens.headOption.exists(_.is[Token.LeftBrace]) =>
      val lastTok = t.tokens.last
      ctx.getMatchingOpt(lastTok).foreach(removeBetween(_, lastTok))
  }

  private def breaksBeforeOpAndNotEnclosed(ia: InfixApp): Boolean = {
    val allToks = ia.all.tokens
    !ctx.isMatching(allToks.head, allToks.last) && breaksBeforeOp(ia)
  }

  private def breaksBeforeOp(ia: InfixApp): Boolean = {
    val lhs = ia.lhs
    val lhsLastTok = lhs.tokens.last
    val hasPreOpLF = ctx.tokenTraverser.findBefore(ia.op.tokens.head) {
      case _: Token.LF => Some(true)
      case `lhsLastTok` => Some(false)
      case _ => None
    }
    hasPreOpLF.isDefined || (lhs match {
      case InfixApp(lhsApp) if breaksBeforeOpAndNotEnclosed(lhsApp) => true
      case _ =>
        ia.rhs match {
          case Seq(InfixApp(rhsApp)) => breaksBeforeOpAndNotEnclosed(rhsApp)
          case _ => false
        }
    })
  }

  private def maybeRemove(tree: Tree, minToKeep: Int = 0): Unit =
    tree match {
      case _ if rewriteFunc.isDefinedAt(tree) =>
      case InfixApp(ia) if breaksBeforeOp(ia) => // can't rewrite
      case _ => remove(tree, minToKeep)
    }

  private def maybeRemovePostfix(tree: Tree): Unit =
    if (RewriteCtx.isPostfixExpr(tree)) remove(tree)

  private def remove(tree: Tree, minToKeep: Int = 0): Unit =
    removeByTokens(tree.tokens, minToKeep)

  private def removeByTokens(toks: Tokens, minToKeep: Int = 0): Unit =
    toks.headOption.foreach { head =>
      val beg = ctx.getIndex(head)
      removeParensByIndex(beg until (beg + toks.length), minToKeep)
    }

  private def removeBetween(b: Token, e: Token): Unit =
    removeParensByIndex(ctx.getIndex(b) to ctx.getIndex(e))

  private def removeParensByIndex(range: Range, minToKeep: Int = 0): Unit = {
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
      val offBeg = minToKeep
      if (offBeg <= offEnd) {
        implicit val builder = Seq.newBuilder[TokenPatch]
        // replace outer with space, to avoid joining with an adjacent keyword
        builder += TokenPatch.AddLeft(toks(beg + offBeg), " ", keepTok = false)
        builder += TokenPatch.AddRight(toks(end - offBeg), " ", keepTok = false)
        ((offBeg + 1) to offEnd).foreach { x =>
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
