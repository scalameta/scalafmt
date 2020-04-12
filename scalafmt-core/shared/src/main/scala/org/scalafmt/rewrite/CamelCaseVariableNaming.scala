package org.scalafmt.rewrite

import scala.meta.{Pat, Term, Tree}


object VariableNaming extends Rewrite {
  override def create(implicit ctx: RewriteCtx): RewriteSession =
    new VariableNaming
}

class VariableNaming(implicit ctx: RewriteCtx) extends RewriteSession {

  def isSnakeCaseNaming(str: String): Boolean = {
    str.contains('_')
  }

  def toCamelCaseNaming(str: String): String = {
    str.split('_') match {
      case Array(head) => head
      case Array(head, tail @ _*) => head + tail
          .map(s => s.charAt(0).toUpper + s.slice(1, s.length))
        .reduce( _ + _)
    }
  }

  def rewriteCamelCaseNaming(t: Tree): Unit = {
    t.tokens.headOption.filter(p => isSnakeCaseNaming(p.text)).foreach(
      snakeCase => {
        ctx.addPatchSet(Seq(TokenPatch.Replace(snakeCase, toCamelCaseNaming(snakeCase.text))): _*)
      }
    )
  }

  override def rewrite(tree: Tree): Unit =
    tree match {
      case t: Pat.Var => rewriteCamelCaseNaming(t)
      case _ =>
    }
}
