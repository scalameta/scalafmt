package org.scalafmt.rewrite

import scala.meta._

import metaconfig.ConfDecoder
import org.scalafmt.config.ReaderUtil
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.TokenOps.TokenHash
import org.scalafmt.util.{TokenTraverser, TreeOps}

case class RewriteCtx(
    style: ScalafmtConfig,
    tokenTraverser: TokenTraverser,
    matchingParens: Map[TokenHash, Token]
)

abstract class Rewrite {
  def rewrite(code: Tree, ctx: RewriteCtx): Seq[Patch]
}

object Rewrite {
  implicit val reader: ConfDecoder[Rewrite] =
    ReaderUtil.oneOf[Rewrite](
      RedundantBraces,
      RedundantParens,
      SortImports,
      AsciiSortImports,
      PreferCurlyFors,
      ExpandImportSelectors,
      AvoidInfix
    )

  private def nameMap[T](t: sourcecode.Text[T]*): Map[String, T] = {
    t.map(x => x.source -> x.value)(scala.collection.breakOut)
  }

  val name2rewrite: Map[String, Rewrite] = nameMap[Rewrite](
    RedundantBraces,
    RedundantParens,
    SortImports,
    AsciiSortImports,
    PreferCurlyFors,
    ExpandImportSelectors,
    AvoidInfix
  )
  val rewrite2name: Map[Rewrite, String] = name2rewrite.map(_.swap)
  val available = Rewrite.name2rewrite.keys.mkString(", ")

  val default: Seq[Rewrite] = name2rewrite.values.toSeq

  private def incompatibleRewrites: List[(Rewrite, Rewrite)] = List(
    SortImports -> ExpandImportSelectors,
    SortImports -> AsciiSortImports
  )

  def validateRewrites(rewrites: Seq[Rewrite]): Seq[String] = {
    incompatibleRewrites.flatMap {
      case (a, b) =>
        if (rewrites.contains(a) && rewrites.contains(b))
          List(
            s"Incompatible rewrites: $a and $b"
          )
        else Nil
    }
  }

  def apply(input: Input, style: ScalafmtConfig): String = {
    val rewrites = style.rewrite.rules
    def noop = new String(input.chars)
    if (rewrites.isEmpty) {
      noop
    } else {
      style.runner.dialect(input).parse(style.runner.parser) match {
        case Parsed.Success(ast) =>
          val tokens = ast.tokens
          val ctx = RewriteCtx(
            style,
            tokenTraverser = new TokenTraverser(tokens),
            TreeOps.getMatchingParentheses(tokens)
          )
          val patches: Seq[Patch] = rewrites.flatMap(_.rewrite(ast, ctx))
          Patch(ast, patches)(ctx)
        case _ => noop
      }
    }
  }
}
