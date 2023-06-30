package org.scalafmt.rewrite

import scala.collection.mutable

import metaconfig.ConfCodecEx
import scala.meta._
import scala.meta.tokens.Token.LF
import scala.meta.transversers.SimpleTraverser

import org.scalafmt.config.ReaderUtil
import org.scalafmt.config.RewriteSettings
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.{TokenOps, TokenTraverser, TreeOps, Trivia, Whitespace}

case class RewriteCtx(
    style: ScalafmtConfig,
    input: Input,
    tree: Tree
) {
  implicit val dialect: Dialect = style.dialect

  private val patchBuilder = mutable.Map.empty[(Int, Int), TokenPatch]

  val tokens = tree.tokens
  val tokenTraverser = new TokenTraverser(tokens, input)
  val matchingParens = TreeOps.getMatchingParentheses(tokens)

  @inline def getMatching(a: Token): Token =
    matchingParens(TokenOps.hash(a))

  @inline def getMatchingOpt(a: Token): Option[Token] =
    matchingParens.get(TokenOps.hash(a))

  @inline def isMatching(a: Token, b: => Token) =
    getMatchingOpt(a).exists(_ eq b)

  @inline def getIndex(token: Token) = tokenTraverser.getIndex(token)

  def applyPatches: String =
    tokens.iterator
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

  def onlyWhitespaceBefore(index: Int): Boolean =
    tokenTraverser
      .findAtOrBefore(index - 1) {
        case _: LF | _: Token.BOF => Some(true)
        case Whitespace() => None
        case _ => Some(false)
      }
      .isDefined

  def findNonWhitespaceWith(
      f: (Token => Option[Boolean]) => Option[Token]
  ): Option[(Token, Option[LF])] = {
    var lf: Option[LF] = None
    val nonWs = f {
      case t: LF =>
        if (lf.nonEmpty) Some(false)
        else { lf = Some(t); None }
      case Whitespace() => None
      case _ => Some(true)
    }
    nonWs.map((_, lf))
  }

  // end is inclusive
  def removeLFToAvoidEmptyLine(
      beg: Int,
      end: Int
  )(implicit builder: Rewrite.PatchBuilder): Unit =
    if (onlyWhitespaceBefore(beg))
      tokenTraverser
        .findAtOrAfter(end + 1) {
          case _: LF => Some(true)
          case Whitespace() => None
          case _ => Some(false)
        }
        .map(TokenPatch.Remove)
        .foreach(builder += _)

  // special case for Select which might contain a space instead of dot
  def isPrefixExpr(expr: Tree): Boolean =
    RewriteCtx.isSimpleExprOr(expr) { case t: Term.Select =>
      val maybeDot = tokenTraverser.findBefore(t.name.tokens.head) {
        case Trivia() => None
        case x => Some(x.is[Token.Dot])
      }
      maybeDot.isDefined
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

object RewriteSession {
  final class None(implicit ctx: RewriteCtx) extends RewriteSession {
    def rewrite(tree: Tree): Unit = {}
  }
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
    SortModifiers
  )

  implicit val reader: ConfCodecEx[Rewrite] = ReaderUtil.oneOf(rewrites: _*)

  val name2rewrite: Map[String, Rewrite] =
    rewrites.view.map(x => x.source -> x.value).toMap
  val rewrite2name: Map[Rewrite, String] = name2rewrite.map(_.swap)
  val available = name2rewrite.keys.mkString(", ")

  val default: Seq[Rewrite] = name2rewrite.values.toSeq

  def apply(
      input: Input,
      style: ScalafmtConfig,
      toInput: String => Input
  ): Input = {
    val rewrites = style.rewrite.rewriteFactoryRules
    if (rewrites.isEmpty) {
      input
    } else {
      style.runner.parse(input) match {
        case Parsed.Success(ast) =>
          val ctx = RewriteCtx(style, input, ast)
          val rewriteSessions = rewrites.map(_.create(ctx)).toList
          val traverser = new SimpleTraverser {
            override def apply(tree: Tree): Unit = {
              rewriteSessions.foreach(_.rewrite(tree))
              super.apply(tree)
            }
          }
          traverser(ast)
          toInput(ctx.applyPatches)
        case _ => input
      }
    }
  }

}

object RewriteCtx {

  // https://www.scala-lang.org/files/archive/spec/2.13/06-expressions.html#prefix-infix-and-postfix-operations
  def isSimpleExprOr(
      expr: Tree
  )(orElse: PartialFunction[Tree, Boolean]): Boolean =
    expr match {
      case _: Lit | _: Name | _: Term.Interpolate => true
      case _: Term.New | _: Term.NewAnonymous => true
      case _: Term.Apply | _: Term.ApplyUnary => true
      case _ => orElse.applyOrElse(expr, (_: Tree) => false)
    }

  @inline
  def isPostfixExpr(expr: Tree)(implicit style: ScalafmtConfig): Boolean =
    isSimpleExprOr(expr) {
      case _: Term.Select | _: Term.ApplyInfix => true
      case _: Term.Match if style.dialect.allowMatchAsOperator => true
    }

}
