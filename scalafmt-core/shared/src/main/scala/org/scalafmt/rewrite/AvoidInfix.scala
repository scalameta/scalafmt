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

  private val cfg = ctx.style.rewrite.avoidInfix

  // In a perfect world, we could just use
  // Tree.transform {
  //   case t: Term.ApplyInfix => Term.Apply(Term.Select(t.lhs, t.op), t.args)
  // }
  // and be done with it. However, until transform becomes token aware (see https://github.com/scalameta/scalameta/pull/457)
  // we will do these dangerous rewritings by hand.

  override def rewrite(tree: Tree): Unit = tree match {
    case x: Term.ApplyInfix => rewriteImpl(x.lhs, x.op, x.arg, x.targClause)
    case x: Term.Select if !cfg.excludePostfix =>
      val maybeDot = ctx.tokenTraverser.prevNonTrivialToken(x.name.tokens.head)
      if (!maybeDot.forall(_.is[Token.Dot])) rewriteImpl(x.qual, x.name)
    case _ =>
  }

  private def rewriteImpl(
      lhs: Term,
      op: Name,
      rhs: Tree = null,
      targs: Member.SyntaxValuesClause = null,
  ): Unit = {
    val (lhsHead, lhsLast) = ends(lhs)
    val beforeLhsHead = ctx.tokenTraverser.prevNonTrivialToken(lhsHead)
    val lhsIsWrapped = isWrapped(lhsHead, lhsLast, beforeLhsHead)

    val lhsIsOK = lhsIsWrapped ||
      (lhs match {
        case y: Term.ApplyInfix => checkMatchingInfix(y.lhs, y.op, y.arg)
        case _ => false
      })

    if (!checkMatchingInfix(lhs, op, rhs, Some(lhsIsOK))) return
    if (!ctx.dialect.allowTryWithAnyExpr)
      if (beforeLhsHead.exists(_.is[Token.KwTry])) return

    val builder = Seq.newBuilder[TokenPatch]

    val (opHead, opLast) = ends(op)
    builder += TokenPatch.AddLeft(opHead, ".", keepTok = true)

    if (rhs ne null) {
      def moveOpenDelim(prev: Token, open: Token): Unit = {
        // move delimiter (before comment or newline)
        builder += TokenPatch.AddRight(prev, open.text, keepTok = true)
        builder += TokenPatch.Remove(open)
      }

      // move the left bracket if targs
      val beforeLp =
        if ((targs eq null) || targs.values.isEmpty) opLast
        else {
          val (targsHead, targsLast) = ends(targs)
          moveOpenDelim(opLast, targsHead)
          targsLast
        }
      // move the left paren if enclosed, else enclose
      val (argsHead, argsLast) = ends(rhs)
      if (ctx.getMatchingOpt(argsHead).exists(argsLast.end <= _.end))
        moveOpenDelim(beforeLp, argsHead)
      else {
        builder += TokenPatch.AddRight(beforeLp, "(", keepTok = true)
        builder += TokenPatch.AddRight(argsLast, ")", keepTok = true)
      }
    }

    val shouldWrapLhs = !lhsIsWrapped &&
      (lhs match {
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
      lhs: Term,
      name: Name,
      rhs: Tree,
      lhsIsOK: Option[Boolean] = None,
  ): Boolean = {
    val op = name.value
    InfixApp.isLeftAssoc(op) && cfg.matches(lhs.text, op) &&
    (rhs match {
      case ac @ Term.ArgClause(arg :: Nil, _) if !isWrapped(ac) =>
        !hasPlaceholder(arg, ctx.style.rewrite.isAllowInfixPlaceholderArg)
      case _ => true
    }) &&
    (lhs match {
      case lhs: Term.ApplyInfix if hasPlaceholder(lhs, true) =>
        lhsIsOK match {
          case Some(x) => x
          case None => isWrapped(lhs) ||
            checkMatchingInfix(lhs.lhs, lhs.op, lhs.arg)
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
