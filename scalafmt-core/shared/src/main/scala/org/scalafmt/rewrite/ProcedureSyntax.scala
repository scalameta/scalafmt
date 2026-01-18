package org.scalafmt.rewrite

import org.scalafmt.util.TreeOps

import scala.meta._
import scala.meta.tokens.{Token => T}

object ProcedureSyntax extends RewriteFactory {
  override def create(implicit ctx: RewriteCtx): Option[RewriteSession] =
    if (ctx.dialect.allowProcedureSyntax) Some(new ProcedureSyntax) else None
}

class ProcedureSyntax private (implicit ctx: RewriteCtx)
    extends RewriteSession {

  override def rewrite(tree: Tree): Unit = tree match {
    case t: Defn.Def if TreeOps.isProcedureSyntax(t) =>
      val tok = t.paramClauseGroups.lastOption.getOrElse(t.name).tokens.last
      if (!ctx.tokenTraverser.nextNonTrivialToken(tok).is[T.Equals]) ctx
        .addPatchSet(TokenPatch.AddRight(tok, ": Unit = ", keepTok = true))
    case t: Decl.Def if TreeOps.isProcedureSyntaxDeclTpe(t.decltpe) =>
      val tok = t.paramClauseGroups.lastOption.getOrElse(t.name).tokens.last
      ctx.addPatchSet(TokenPatch.AddRight(tok, ": Unit", keepTok = true))
    case _ =>
  }

}
