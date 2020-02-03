package org.scalafmt.rewrite

import scala.meta._
import scala.meta.classifiers.Classifier
import scala.meta.tokens.Token

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
  override def rewrite(tree: Tree): Unit = tree match {
    case t: Importer =>
      t.tokens.lastOption match {
        case Some(close: Token.RightBrace) => before(close)
        case _ =>
      }

    case t: Decl.Def => afterTParams(t.tparams); afterEachArgs(t.paramss)
    case t: Defn.Def => afterTParams(t.tparams); afterEachArgs(t.paramss)
    case t: Defn.Macro => afterTParams(t.tparams); afterEachArgs(t.paramss)
    case t: Defn.Class => afterTParams(t.tparams)
    case t: Defn.Trait => afterTParams(t.tparams)
    case t: Ctor.Secondary => afterEachArgs(t.paramss)
    case t: Decl.Type => afterTParams(t.tparams)
    case t: Defn.Type => afterTParams(t.tparams)
    case t: Type.Apply => afterArgs(t.args)
    case t: Type.Param => afterTParams(t.tparams)
    case t: Type.Tuple => afterArgs(t.args)
    case t: Term.Function => afterArgs(t.params)
    case t: Type.Function => afterArgs(t.params)
    case t: Ctor.Primary => afterEachArgs(t.paramss)

    case t: Term.Apply => afterArgs(t.args)
    case t: Pat.Extract => afterArgs(t.args)
    case t: Pat.Tuple => afterArgs(t.args)
    case t: Term.Tuple => afterArgs(t.args)
    case t: Term.ApplyType => afterArgs(t.targs)
    case t: Init => afterEachArgs(t.argss)

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

  private def afterTParams(tparams: List[Type.Param]): Unit =
    tparams.lastOption.foreach {
      _.tokens.lastOption.foreach(forwardBeforeType[Token.RightBracket])
    }

  private def afterArgs(args: List[Tree]): Unit =
    args.lastOption.foreach {
      _.tokens.lastOption.foreach(forwardBeforeType[Token.RightParen])
    }

  private def afterEachArgs(argss: List[List[Tree]]): Unit =
    argss.foreach(afterArgs)

}
