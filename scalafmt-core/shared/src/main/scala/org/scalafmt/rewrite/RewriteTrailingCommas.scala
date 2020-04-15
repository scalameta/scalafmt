package org.scalafmt.rewrite

import scala.meta._
import scala.meta.classifiers.Classifier
import scala.meta.tokens.Token

import org.scalafmt.util.TreeOps
import org.scalafmt.util.Whitespace

object RewriteTrailingCommas extends Rewrite {
  override def create(implicit ctx: RewriteCtx): RewriteSession =
    new RewriteTrailingCommas
}

class RewriteTrailingCommas(implicit ctx: RewriteCtx) extends RewriteSession {

  /* even if this rule is called for 'always', we'll still remove any trailing
   * commas here, to avoid any idempotence problems caused by an extra comma,
   * with a different AST, in a subsequent runs; in FormatWriter, we'll add
   * them back if there is a newline before the closing bracket/paren/brace */
  override def rewrite(tree: Tree): Unit =
    tree match {
      case t: Importer =>
        t.tokens.lastOption match {
          case Some(close: Token.RightBrace) => before(close)
          case _ =>
        }

      case TreeOps.SplitDefnIntoParts(_, _, tparams, paramss) =>
        afterTParams(tparams)
        afterEachArgs(paramss)

      case TreeOps.SplitCallIntoParts(_, either) =>
        either match {
          case Left(args) => afterArgs(args)
          case Right(argss) => afterEachArgs(argss)
        }

      case _ =>
    }

  private def patch(token: Token): Unit =
    ctx.addPatchSet(TokenPatch.Remove(token))

  private def before(close: Token): Unit =
    ctx.tokenTraverser.findBefore(close) {
      case c: Token.Comma => patch(c); Some(false)
      case Whitespace() | _: Token.Comment => None
      case _ => Some(false)
    }

  private def forwardBeforeType[A](
      start: Token
  )(implicit cls: Classifier[Token, A]): Unit = {
    var comma: Token.Comma = null
    ctx.tokenTraverser.findAfter(start) {
      case Whitespace() | _: Token.Comment => None
      case c: Token.Comma => comma = c; None
      case t => if (comma != null && cls(t)) patch(comma); Some(false)
    }
  }

  private def afterTParams(tparams: Seq[Type.Param]): Unit =
    tparams.lastOption.foreach {
      _.tokens.lastOption.foreach(forwardBeforeType[Token.RightBracket])
    }

  private def afterArgs(args: Seq[Tree]): Unit =
    args.lastOption.foreach {
      _.tokens.lastOption.foreach(forwardBeforeType[Token.RightParen])
    }

  private def afterEachArgs(argss: Seq[Seq[Tree]]): Unit =
    argss.foreach(afterArgs)

}
