package org.scalafmt.rewrite

import scala.meta._

import org.scalafmt.config.ReaderUtil
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.TokenTraverser

case class RewriteCtx(
                         style: ScalafmtConfig,
                         tokenTraverser: TokenTraverser
)

abstract class Rewrite {
  def rewrite(code: Tree, ctx: RewriteCtx): Seq[Patch]
}

object Rewrite {
  val reader =
    ReaderUtil.oneOf[Rewrite](
      RedundantBraces,
      SortImports
    )

  private def nameMap[T](t: sourcecode.Text[T]*): Map[String, T] = {
    t.map(x => x.source -> x.value)(scala.collection.breakOut)
  }

  val name2rewrite: Map[String, Rewrite] = nameMap[Rewrite](
    RedundantBraces,
    SortImports
  )
  val rewrite2name: Map[Rewrite, String] = name2rewrite.map(_.swap)
  val available = Rewrite.name2rewrite.keys.mkString(", ")

  val default: Seq[Rewrite] = name2rewrite.values.toSeq

  def apply(input: Input, style: ScalafmtConfig): String = {
    val rewrites = style.rewrite.rules
    def noop = new String(input.chars)
    if (rewrites.isEmpty) {
      noop
    } else {
      input.parse[Source] match {
        case Parsed.Success(ast) =>
          val ctx = RewriteCtx(
            style,
            tokenTraverser = new TokenTraverser(ast.tokens)
          )
          val rr = rewrites.map(_.getClass)
          val patches: Seq[Patch] = rewrites.flatMap(_.rewrite(ast, ctx))
          Patch(ast.tokens, patches)
        case _ => noop
      }
    }
  }
}
