package org.scalafmt.rewrite

object ProcedureSyntax extends RewriteFactory {
  override def create(implicit ctx: RewriteCtx): Option[RewriteSession] = None
}
