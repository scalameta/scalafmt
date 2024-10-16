package org.scalafmt.rewrite

import org.scalafmt.config.RewriteSettings
import org.scalafmt.util.InfixApp

import scala.meta._
import scala.meta.internal.trees.PlaceholderChecks.hasPlaceholder
import scala.meta.tokens.Token

import scala.annotation.tailrec

object AvoidInfix extends RewriteFactory {

  override def hasChanged(v1: RewriteSettings, v2: RewriteSettings): Boolean =
    v2.avoidInfix ne v1.avoidInfix

  override def create(implicit ctx: RewriteCtx): RewriteSession = new AvoidInfix

}

class AvoidInfix(implicit ctx: RewriteCtx) extends RewriteSession {

  private val matcher = ctx.style.rewrite.avoidInfix

  // In a perfect world, we could just use
  // Tree.transform {
  //   case t: Term.ApplyInfix => Term.Apply(Term.Select(t.lhs, t.op), t.args)
  // }
  // and be done with it. However, until transform becomes token aware (see https://github.com/scalameta/scalameta/pull/457)
  // we will do these dangerous rewritings by hand.

  override def rewrite(tree: Tree): Unit = tree match {
    case x: Term.ApplyInfix => rewriteInfix(x)
    case _ =>
  }

  private def rewriteInfix(x: Term.ApplyInfix): Unit = {
    val (lhsHead, lhsLast) = ends(x.lhs)
    val beforeLhsHead = ctx.tokenTraverser.prevNonTrivialToken(lhsHead)
    val lhsIsWrapped = isWrapped(lhsHead, lhsLast, beforeLhsHead)

    val lhsIsOK = lhsIsWrapped ||
      (x.lhs match {
        case y: Term.ApplyInfix => checkMatchingInfix(y)
        case _ => false
      })

    if (!checkMatchingInfix(x, Some(lhsIsOK))) return
    if (!ctx.dialect.allowTryWithAnyExpr)
      if (beforeLhsHead.exists(_.is[Token.KwTry])) return

    val builder = Seq.newBuilder[TokenPatch]

    val (opHead, opLast) = ends(x.op)
    builder += TokenPatch.AddLeft(opHead, ".", keepTok = true)

    def moveOpenDelim(prev: Token, open: Token): Unit = {
      // move delimiter (before comment or newline)
      builder += TokenPatch.AddRight(prev, open.syntax, keepTok = true)
      builder += TokenPatch.Remove(open)
    }

    // move the left bracket if targs
    val beforeLp =
      if (x.targClause.values.isEmpty) opLast
      else {
        val (targsHead, targsLast) = ends(x.targClause)
        moveOpenDelim(opLast, targsHead)
        targsLast
      }
    // move the left paren if enclosed, else enclose
    val (argsHead, argsLast) = ends(x.argClause)
    if (ctx.getMatchingOpt(argsHead).exists(argsLast.end <= _.end))
      moveOpenDelim(beforeLp, argsHead)
    else {
      builder += TokenPatch.AddRight(beforeLp, "(", keepTok = true)
      builder += TokenPatch.AddRight(argsLast, ")", keepTok = true)
    }

    val shouldWrapLhs = !lhsIsWrapped &&
      (x.lhs match {
        case _: Term.ApplyInfix => !lhsIsOK
        // foo _ compose bar => (foo _).compose(bar)
        // new Foo compose bar => (new Foo).compose(bar)
        case _: Term.Eta | _: Term.New => true
        case _ => false
      })
    if (shouldWrapLhs) {
      builder += TokenPatch.AddLeft(lhsHead, "(", keepTok = true)
      builder += TokenPatch.AddRight(lhsLast, ")", keepTok = true)
    }

    ctx.addPatchSet(builder.result(): _*)
  }

  @tailrec
  private def checkMatchingInfix(
      ai: Term.ApplyInfix,
      lhsIsOK: Option[Boolean] = None,
  ): Boolean = {
    val op = ai.op.value
    InfixApp.isLeftAssoc(op) && matcher.matches(ai.lhs.text, op) &&
    (ai.argClause match {
      case ac @ Term.ArgClause(arg :: Nil, _) if !isWrapped(ac) =>
        !hasPlaceholder(arg, ctx.style.rewrite.isAllowInfixPlaceholderArg)
      case _ => true
    }) &&
    (ai.lhs match {
      case lhs: Term.ApplyInfix if hasPlaceholder(lhs, true) =>
        lhsIsOK match {
          case Some(x) => x
          case None => isWrapped(lhs) || checkMatchingInfix(lhs)
        }
      case _ => true
    })
  }

  @inline
  private def isMatching(head: Token, last: => Token): Boolean = head
    .is[Token.LeftParen] && ctx.isMatching(head, last)

  private def isWrapped(
      head: Token,
      last: Token,
      beforeHead: => Option[Token],
  ): Boolean = isMatching(head, last) ||
    beforeHead
      .exists(isMatching(_, ctx.tokenTraverser.nextNonTrivialToken(last).orNull))

  private def isWrapped(head: Token, last: Token): Boolean =
    isWrapped(head, last, ctx.tokenTraverser.prevNonTrivialToken(head))

  private def ends(t: Tree): (Token, Token) = {
    val tokens = t.tokens
    (tokens.head, tokens.last)
  }

  private def isWrapped(t: Tree): Boolean = {
    val (head, last) = ends(t)
    isWrapped(head, last)
  }

}
