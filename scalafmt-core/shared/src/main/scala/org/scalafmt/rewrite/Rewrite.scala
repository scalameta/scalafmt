package org.scalafmt
package rewrite

import org.scalafmt.config.{RewriteSettings, ScalafmtConfig}
import org.scalafmt.internal._
import org.scalafmt.util.{TokenOps, TokenTraverser, TreeOps}

import scala.meta.tokens.{Token => T}
import scala.meta.transversers.SimpleTraverser
import scala.meta.{Token => _, _}

import scala.collection.mutable

import metaconfig.ConfCodecEx

case class RewriteCtx(style: ScalafmtConfig, input: Input, tree: Tree) {
  import RewriteCtx._

  implicit val dialect: Dialect = style.dialect

  private val patchBuilder = mutable.LongMap.empty[TokenPatch]

  val tokens: Tokens = tree.tokens
  val tokenTraverser = new TokenTraverser(tokens, input)(style)
  private val matchingParens = TreeOps
    .getMatchingDelims(tokens)(TokenOps.hash)(identity)

  @inline
  def getMatchingOrNull(a: T): T = matchingParens.get(TokenOps.hash(a))

  @inline
  def isMatching(a: T, b: => T): Boolean = getMatchingOrNull(a).nnHas(_ eq b)

  @inline
  def getIndex(token: T): Int = tokenTraverser.getIndex(token)

  def applyPatches: String = {
    // pre-sized, coder-matched builder; append directly, avoiding an intermediate
    // mapped iterator, mkString's regrowth, and a Some per patched token
    val sb = FormatWriter.presizedStringBuilder(input.text)
    tokens.foreach { x =>
      val p = patchBuilder.getOrElse(lookupKey(x), null)
      sb.append(if (p eq null) x.text else p.newTok)
    }
    sb.toString
  }

  def addPatchSet(ps: TokenPatch*): Unit =
    if (!ps.exists(p => tokenTraverser.isExcluded(p.tok))) ps.foreach(p =>
      patchBuilder
        .updateWith(lookupKey(p.tok))(v => Some(v.fold(p)(TokenPatch.merge(_, p)))),
    )

  def onlyWhitespaceBefore(index: Int): Boolean = tokenTraverser
    .findAtOrBefore(index - 1) {
      case _: T.AtEOL | _: T.BOF => MaybeBool.True
      case _: T.Whitespace => MaybeBool.Maybe
      case _ => MaybeBool.False
    } ne null

  def findNonWhitespaceWith(f: (T => MaybeBool) => T): (T, T.AtEOL) = {
    var lf: T.AtEOL = null
    val nonWs = f {
      case t: T.AtEOL =>
        if (lf ne null) MaybeBool.False else { lf = t; MaybeBool.Maybe }
      case _: T.Whitespace => MaybeBool.Maybe
      case _ => MaybeBool.True
    }
    if (nonWs eq null) null else (nonWs, lf)
  }

  // end is inclusive
  def removeLFToAvoidEmptyLine(beg: Int, end: Int)(implicit
      builder: Rewrite.PatchBuilder,
  ): Unit = if (onlyWhitespaceBefore(beg)) tokenTraverser.findAtOrAfter(end + 1) {
    case t: T.AtEOL =>
      if (t.newlines > 1) builder += TokenPatch.Replace(t, t.text.stripLineEnd)
      else builder += TokenPatch.Remove(t)
      MaybeBool.False
    case _: T.HSpace => MaybeBool.Maybe
    case _ => MaybeBool.False
  }

}

trait Rewrite

abstract class RewriteFactory extends Rewrite {
  def create(implicit ctx: RewriteCtx): RewriteSession
  def hasChanged(v1: RewriteSettings, v2: RewriteSettings): Boolean = false
}

abstract class RewriteSession(implicit ctx: RewriteCtx) {
  def rewrite(tree: Tree): Unit
  implicit val dialect: Dialect = ctx.dialect
}

object Rewrite {

  type PatchBuilder =
    scala.collection.mutable.Builder[TokenPatch, Seq[TokenPatch]]

  private val rewrites = Seq[sourcecode.Text[Rewrite]](
    RedundantBraces,
    RedundantParens,
    Imports,
    SortImports,
    AsciiSortImports,
    PreferCurlyFors,
    ExpandImportSelectors,
    AvoidInfix,
    SortModifiers,
    RemoveSemicolons,
    RewriteLiterals,
    ProcedureSyntax,
  )

  implicit val reader: ConfCodecEx[Rewrite] = ConfCodecEx.oneOf(rewrites: _*)

  val name2rewrite: Map[String, Rewrite] = rewrites.view
    .map(x => x.source -> x.value).toMap

  val default: Seq[Rewrite] = name2rewrite.values.toSeq

  def apply(input: Input, style: ScalafmtConfig): String = {
    val rewrites = style.rewrite.rewriteFactoryRules
    if (rewrites.isEmpty) null
    else style.runner.parse(input) match {
      case Parsed.Success(ast) =>
        val ctx = RewriteCtx(style, input, ast)
        val rewriteSessions = {
          val arr = Array.newBuilder[RewriteSession]
          rewrites.foreach { x =>
            val session = x.create(ctx)
            if (session ne null) arr += session
          }
          arr.result()
        }
        val traverser = new SimpleTraverser {
          override def apply(tree: Tree): Unit = {
            rewriteSessions.foreach(_.rewrite(tree))
            super.apply(tree)
          }
        }
        traverser(ast)
        ctx.applyPatches
      case _ => null
    }
  }

}

object RewriteCtx {

  // https://www.scala-lang.org/files/archive/spec/2.13/06-expressions.html#prefix-infix-and-postfix-operations
  def isSimpleExprOr(
      expr: Tree,
  )(orElse: PartialFunction[Tree, Boolean]): Boolean = expr match {
    case _: Lit | _: Name | _: Term.Interpolate => true
    case _: Term.New | _: Term.NewAnonymous => true
    case _: Term.Apply | _: Term.ApplyUnary => true
    case _: Term.Select | _: Term.SelectMatch | _: Term.PartialFunction => true
    case _ => orElse.applyOrElse(expr, (_: Tree) => false)
  }

  @inline
  def isPrefixExpr(expr: Tree): Boolean =
    isSimpleExprOr(expr)(PartialFunction.empty)

  @inline
  def isPostfixExpr(expr: Tree)(implicit style: ScalafmtConfig): Boolean =
    isSimpleExprOr(expr) {
      case _: Term.SelectPostfix | _: Term.ApplyInfix => true
      case _: Term.Match => style.dialect.allowMatchAsOperator
    }

  @inline
  private def lookupKey(tok: T): Long = tok.start.toLong << 32 |
    tok.end.toLong & 0xffffffffL

}
