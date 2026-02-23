package org.scalafmt.rewrite

import org.scalafmt.config.{RewriteSettings, ScalafmtConfig}
import org.scalafmt.util.{TokenOps, TokenTraverser, TreeOps}

import scala.meta.tokens.{Token => T}
import scala.meta.transversers.SimpleTraverser
import scala.meta.{Token => _, _}

import scala.collection.mutable

import metaconfig.ConfCodecEx

case class RewriteCtx(style: ScalafmtConfig, input: Input, tree: Tree) {
  import RewriteCtx._

  implicit val dialect: Dialect = style.dialect

  private val patchBuilder = mutable.Map.empty[(Int, Int), TokenPatch]

  val tokens: Tokens = tree.tokens
  val tokenTraverser = new TokenTraverser(tokens, input)(style)
  private val matchingParens = TreeOps
    .getMatchingDelims(tokens)(TokenOps.hash)(identity)

  @inline
  def getMatchingOpt(a: T): Option[T] = matchingParens.get(TokenOps.hash(a))

  @inline
  def isMatching(a: T, b: => T): Boolean = getMatchingOpt(a).exists(_ eq b)

  @inline
  def getIndex(token: T): Int = tokenTraverser.getIndex(token)

  def applyPatches: String = tokens.iterator
    .map(x => patchBuilder.get(lookupKey(x)).fold(x.text)(_.newTok)).mkString

  def addPatchSet(ps: TokenPatch*): Unit =
    if (!ps.exists(p => tokenTraverser.isExcluded(p.tok))) ps.foreach(p =>
      patchBuilder
        .updateWith(lookupKey(p.tok))(v => Some(v.fold(p)(TokenPatch.merge(_, p)))),
    )

  def onlyWhitespaceBefore(index: Int): Boolean = tokenTraverser
    .findAtOrBefore(index - 1) {
      case _: T.AtEOL | _: T.BOF => Some(true)
      case _: T.Whitespace => None
      case _ => Some(false)
    }.isDefined

  def findNonWhitespaceWith(
      f: (T => Option[Boolean]) => Option[T],
  ): Option[(T, Option[T.AtEOL])] = {
    var lf: Option[T.AtEOL] = None
    val nonWs = f {
      case t: T.AtEOL =>
        if (lf.nonEmpty) Some(false) else { lf = Some(t); None }
      case _: T.Whitespace => None
      case _ => Some(true)
    }
    nonWs.map((_, lf))
  }

  // end is inclusive
  def removeLFToAvoidEmptyLine(beg: Int, end: Int)(implicit
      builder: Rewrite.PatchBuilder,
  ): Unit = if (onlyWhitespaceBefore(beg)) tokenTraverser.findAtOrAfter(end + 1) {
    case t: T.AtEOL =>
      if (t.newlines > 1) builder += TokenPatch.Replace(t, t.text.stripLineEnd)
      else builder += TokenPatch.Remove(t)
      Some(false)
    case _: T.HSpace => None
    case _ => Some(false)
  }

}

trait Rewrite

abstract class RewriteFactory extends Rewrite {
  def create(implicit ctx: RewriteCtx): Option[RewriteSession]
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

  def apply(input: Input, style: ScalafmtConfig): Option[String] = {
    val rewrites = style.rewrite.rewriteFactoryRules
    if (rewrites.isEmpty) None
    else style.runner.parse(input) match {
      case Parsed.Success(ast) =>
        val ctx = RewriteCtx(style, input, ast)
        val rewriteSessions = rewrites.flatMap(_.create(ctx)).toArray
        val traverser = new SimpleTraverser {
          override def apply(tree: Tree): Unit = {
            rewriteSessions.foreach(_.rewrite(tree))
            super.apply(tree)
          }
        }
        traverser(ast)
        Some(ctx.applyPatches)
      case _ => None
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
  private def lookupKey(tok: T) = tok.start -> tok.end

}
