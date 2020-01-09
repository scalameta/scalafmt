package org.scalafmt.rewrite

import scala.collection.mutable

import metaconfig.ConfCodec
import scala.meta._
import org.scalafmt.config.ReaderUtil
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.{TokenOps, TokenTraverser, TreeOps}

case class RewriteCtx(
    style: ScalafmtConfig,
    tree: Tree
) {
  implicit val dialect = style.runner.dialect

  private val patchBuilder = mutable.Map.empty[(Int, Int), TokenPatch]

  val tokens = tree.tokens
  val tokenTraverser = new TokenTraverser(tokens)
  val matchingParens = TreeOps.getMatchingParentheses(tokens)

  def isMatching(a: Token, b: Token) =
    matchingParens.get(TokenOps.hash(a)).contains(b)

  def applyPatches: String =
    tokens.toIterator
      .map(x => patchBuilder.get(x.start -> x.end).fold(x.syntax)(_.newTok))
      .mkString

  def addPatchSet(patches: TokenPatch*): Unit =
    if (!patches.exists(x => tokenTraverser.isExcluded(x.tok)))
      patches.foreach { patch =>
        val key = (patch.tok.start, patch.tok.end)
        val value = patchBuilder.get(key) match {
          case Some(prev) => Patch.merge(prev, patch)
          case None => patch
        }
        patchBuilder.update(key, value)
      }

}

abstract class Rewrite {
  def rewrite(implicit ctx: RewriteCtx): Unit
}

object Rewrite {

  private val rewrites = Seq[sourcecode.Text[Rewrite]](
    RedundantBraces,
    RedundantParens,
    SortImports,
    AsciiSortImports,
    PreferCurlyFors,
    ExpandImportSelectors,
    AvoidInfix,
    SortModifiers
  )

  implicit val reader: ConfCodec[Rewrite] = ReaderUtil.oneOf(rewrites: _*)

  val name2rewrite: Map[String, Rewrite] =
    rewrites.view.map(x => x.source -> x.value).toMap
  val rewrite2name: Map[Rewrite, String] = name2rewrite.map(_.swap)
  val available = name2rewrite.keys.mkString(", ")

  val default: Seq[Rewrite] = name2rewrite.values.toSeq

  private def incompatibleRewrites: List[(Rewrite, Rewrite)] = List(
    SortImports -> ExpandImportSelectors,
    SortImports -> AsciiSortImports,
    AsciiSortImports -> ExpandImportSelectors
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

  def apply(input: Input, style: ScalafmtConfig): Input = {
    val rewrites = style.rewrite.rules
    if (rewrites.isEmpty) {
      input
    } else {
      style.runner.dialect(input).parse(style.runner.parser) match {
        case Parsed.Success(ast) =>
          val ctx = RewriteCtx(style, ast)
          rewrites.foreach(_.rewrite(ctx))
          val out = ctx.applyPatches
          input match {
            case Input.File(path, _) =>
              Input.VirtualFile(path.toString(), out)
            case Input.VirtualFile(path, _) => Input.VirtualFile(path, out)
            case _ => Input.String(out)
          }
        case _ => input
      }
    }
  }
}
