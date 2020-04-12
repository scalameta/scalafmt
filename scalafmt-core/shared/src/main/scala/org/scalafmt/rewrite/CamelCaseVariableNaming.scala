package org.scalafmt.rewrite

import scala.meta.{Pat, Tree}

object CamelCaseVariableNaming extends Rewrite {
  override def create(implicit ctx: RewriteCtx): RewriteSession =
    new CamelCaseVariableNaming
}

class CamelCaseVariableNaming(implicit ctx: RewriteCtx) extends RewriteSession {

  def toCamelCaseNaming(str: String): String = {
    str.split('_') match {
      case Array(head) => head
      case Array(head, tail @ _*) =>
        head + tail
          .map(s => s.charAt(0).toUpper + s.slice(1, s.length))
          .reduce(_ + _)
    }
  }

  def rewriteCamelCaseNaming(t: Tree): Unit = {
    t.tokens.headOption
      .filter(p => p.text.contains('_'))
      .foreach(snakeCase => {
        ctx
          .addPatchSet(
            Seq(
              TokenPatch.Replace(snakeCase, toCamelCaseNaming(snakeCase.text))
            ): _*
          )
      })
  }

  override def rewrite(tree: Tree): Unit =
    tree match {
      case t: Pat.Var => rewriteCamelCaseNaming(t)
      case _ =>
    }
}
