package org.scalafmt.rewrite

import scala.collection.mutable

import metaconfig.ConfCodec
import scala.meta._
import scala.meta.tokens.Token.LF
import scala.meta.transversers.SimpleTraverser

import org.scalafmt.config.ReaderUtil
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.config.TrailingCommas
import org.scalafmt.util.{TokenOps, TokenTraverser, TreeOps, Whitespace}

case class RewriteCtx(
    style: ScalafmtConfig,
    tree: Tree
) {
  implicit val dialect = style.runner.dialect

  private val patchBuilder = mutable.Map.empty[(Int, Int), TokenPatch]

  val tokens = tree.tokens
  val tokenTraverser = new TokenTraverser(tokens)
  val matchingParens = TreeOps.getMatchingParentheses(tokens)

  @inline def getMatching(a: Token): Token =
    matchingParens(TokenOps.hash(a))

  @inline def getMatchingOpt(a: Token): Option[Token] =
    matchingParens.get(TokenOps.hash(a))

  @inline def isMatching(a: Token, b: Token) =
    getMatchingOpt(a).contains(b)

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

  def onlyWhitespaceBefore(token: Token): Boolean =
    tokenTraverser.findBefore(token)(RewriteCtx.isLFSkipWhitespace).isDefined

  def getPatchToAvoidEmptyLine(token: Token): Option[TokenPatch] =
    if (!onlyWhitespaceBefore(token)) None
    else
      tokenTraverser
        .findAfter(token)(RewriteCtx.isLFSkipWhitespace)
        .map(TokenPatch.Remove)

  def removeLFToAvoidEmptyLine(
      token: Token
  )(implicit builder: Rewrite.PatchBuilder): Unit =
    getPatchToAvoidEmptyLine(token).foreach(builder += _)

}

abstract class Rewrite {
  def create(implicit ctx: RewriteCtx): RewriteSession
}

abstract class RewriteSession(implicit ctx: RewriteCtx) {
  def rewrite(tree: Tree): Unit
}

object Rewrite {

  type PatchBuilder =
    scala.collection.mutable.Builder[TokenPatch, Seq[TokenPatch]]

  private val rewrites = Seq[sourcecode.Text[Rewrite]](
    RedundantBraces,
    CamelCaseVariableNaming,
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
    val trailingCommaRewrite =
      if (!style.runner.dialect.allowTrailingCommas ||
        style.trailingCommas == TrailingCommas.preserve) Seq.empty
      else Seq(RewriteTrailingCommas)

    val rewrites = style.rewrite.rules ++ trailingCommaRewrite
    if (rewrites.isEmpty) {
      input
    } else {
      style.runner.dialect(input).parse(style.runner.parser) match {
        case Parsed.Success(ast) =>
          val ctx = RewriteCtx(style, ast)
          val rewriteSessions = rewrites.map(_.create(ctx)).toList
          val traverser = new SimpleTraverser {
            override def apply(tree: Tree): Unit = {
              rewriteSessions.foreach(_.rewrite(tree))
              super.apply(tree)
            }
          }
          traverser(ast)
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

object RewriteCtx {

  // this is a helper function to be used with the token traverser
  // finds a newline not blocked by any non-whitespace characters
  private def isLFSkipWhitespace(token: Token): Option[Boolean] =
    token match {
      case _: LF => Some(true)
      case Whitespace() => None
      case _ => Some(false)
    }

}
