package org.scalafmt.rewrite

import org.scalafmt.config.RewriteSettings
import org.scalafmt.util.InfixApp

import scala.meta._
import scala.meta.internal.trees.PlaceholderChecks.hasPlaceholder
import scala.meta.tokens.{Token => T}

import scala.annotation.tailrec

object AvoidInfix extends RewriteFactory {

  override def hasChanged(v1: RewriteSettings, v2: RewriteSettings): Boolean =
    v2.avoidInfix ne v1.avoidInfix

  override def create(implicit ctx: RewriteCtx): Option[RewriteSession] =
    Some(new AvoidInfix)

}

class AvoidInfix(implicit ctx: RewriteCtx) extends RewriteSession {

  private val cfg = ctx.style.rewrite.avoidInfix
  private val allowMatchAsOperator = dialect.allowMatchAsOperator

  // In a perfect world, we could just use
  // Tree.transform {
  //   case t: Term.ApplyInfix => Term.Apply(Term.Select(t.lhs, t.op), t.args)
  // }
  // and be done with it. However, until transform becomes token aware (see https://github.com/scalameta/scalameta/pull/457)
  // we will do these dangerous rewritings by hand.

  override def rewrite(tree: Tree): Unit = tree match {
    case x: Term.ApplyInfix =>
      rewriteImpl(x.lhs, Right(x.op), x.arg, x.targClause)
    case x: Term.SelectPostfix if !cfg.excludePostfix =>
      rewriteImpl(x.qual, Right(x.name))
    case x: Term.Match => noDotMatch(x)
        .foreach(op => rewriteImpl(x.expr, Left(op), null))
    case _ =>
  }

  private def noDotMatch(t: Term.Match): Option[T] =
    if (allowMatchAsOperator && t.mods.isEmpty && !cfg.excludeMatch) ctx
      .tokenTraverser.prevNonTrivialToken(t.casesBlock.tokens.head)
    else None

  private def rewriteImpl(
      lhs: Term,
      op: Either[T, Name],
      rhs: Tree = null,
      targs: Member.SyntaxValuesClause = null,
  ): Unit = {
    val (lhsHead, lhsLast) = ends(lhs)
    val beforeLhsHead = ctx.tokenTraverser.prevNonTrivialToken(lhsHead)
    val lhsIsWrapped = isWrapped(lhsHead, lhsLast, beforeLhsHead)

    val lhsIsOK = lhsIsWrapped ||
      (lhs match {
        case t: Term.ApplyInfix => checkMatchingInfix(t.lhs, t.op.value, t.arg)
        case t: Term.Match => noDotMatch(t) match {
            case None => false
            case Some(kw) => checkMatchingInfix(t.expr, kw.text, t.casesBlock)
          }
        case _ => false
      })

    if (!checkMatchingInfix(lhs, op.fold(_.text, _.value), rhs, Some(lhsIsOK)))
      return
    if (!ctx.dialect.allowTryWithAnyExpr)
      if (beforeLhsHead.exists(_.is[T.KwTry])) return

    val builder = Seq.newBuilder[TokenPatch]

    val (opHead, opLast) = op.fold((_, null), ends)
    builder += TokenPatch.AddLeft(opHead, ".", keepTok = true)

    if ((rhs ne null) && (opLast ne null)) {
      def moveOpenDelim(prev: T, open: T): Unit = {
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
      def rhsWrapped = ctx.getMatchingOpt(argsHead).exists(_.end >= argsLast.end)
      val shouldMoveOpenDelim = rhs match {
        case _: Lit.Unit => ctx.dialect.allowEmptyInfixArgs
        case ac: Term.ArgClause => ac.values match {
            case Nil => ctx.dialect.allowEmptyInfixArgs
            case arg :: Nil => (arg.tokens.head ne argsHead) ||
              !arg.is[Lit.Unit] && rhsWrapped
            case _ => true
          }
        case _ => rhsWrapped
      }
      if (shouldMoveOpenDelim) moveOpenDelim(beforeLp, argsHead)
      else {
        builder += TokenPatch.AddRight(beforeLp, "(", keepTok = true)
        builder += TokenPatch.AddRight(argsLast, ")", keepTok = true)
      }
    }

    val shouldWrapLhs = !lhsIsWrapped &&
      (lhs match {
        case _: Term.ApplyInfix | _: Term.Match => !lhsIsOK
        // foo _ compose bar => (foo _).compose(bar)
        // new Foo compose bar => (new Foo).compose(bar)
        case _: Term.Eta | _: Term.New | _: Term.Annotate => true
        case _: Term.SelectPostfix => rhs eq null
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
      op: String,
      rhs: Tree,
      lhsIsOK: => Option[Boolean] = None,
  ): Boolean = InfixApp.isLeftAssoc(op) && cfg.matches(lhs.text, op) &&
    (rhs match {
      case ac @ Term.ArgClause(arg :: Nil, _) if !isWrapped(ac) =>
        !hasPlaceholder(arg, cfg.excludePlaceholderArg)
      case _ => true
    }) &&
    (lhs match {
      case lhs: Term.ApplyInfix if hasPlaceholder(lhs, includeArg = true) =>
        lhsIsOK match {
          case Some(x) => x
          case None => isWrapped(lhs) ||
            checkMatchingInfix(lhs.lhs, lhs.op.value, lhs.arg)
        }
      case lhs: Term.Match if hasPlaceholder(lhs, includeArg = true) =>
        lhsIsOK match {
          case Some(x) => x
          case None if isWrapped(lhs) => true
          case None => noDotMatch(lhs) match {
              case None => false
              case Some(op) => checkMatchingInfix(lhs.expr, op.text, null)
            }
        }
      case _ => true
    })

  @inline
  private def isMatching(head: T, last: => T): Boolean = head.is[T.LeftParen] &&
    ctx.isMatching(head, last)

  private def isWrapped(head: T, last: T, beforeHead: => Option[T]): Boolean =
    isMatching(head, last) || beforeHead.exists(
      isMatching(_, ctx.tokenTraverser.nextNonTrivialToken(last).orNull),
    )

  private def isWrapped(head: T, last: T): Boolean =
    isWrapped(head, last, ctx.tokenTraverser.prevNonTrivialToken(head))

  private def ends(t: Tree): (T, T) = {
    val tokens = t.tokens
    (tokens.head, tokens.last)
  }

  private def isWrapped(t: Tree): Boolean = {
    val (head, last) = ends(t)
    isWrapped(head, last)
  }

}
