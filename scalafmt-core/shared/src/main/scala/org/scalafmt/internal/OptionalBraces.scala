package org.scalafmt
package internal

import org.scalafmt.config._
import org.scalafmt.util.ParamClauseParent
import org.scalafmt.util.PolicyOps._
import org.scalafmt.util.TreeOps._

import org.scalameta.FileLine
import scala.meta._
import scala.meta.classifiers.Classifier
import scala.meta.tokens.{Token => T}

import scala.annotation.tailrec

abstract class OptionalBraces {

  /** this is really the end-marker owner */
  def owner: Tree
  def block: Tree
  def splits: Seq[Split]
}

object OptionalBraces {

  private trait Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): OptionalBraces
  }

  // Optional braces in templates after `:|with`
  // Optional braces after any token that can start indentation:
  // )  =  =>  ?=>  <-  catch  do  else  finally  for
  // if  match  return  then  throw  try  while  yield

  def getWithBrace(ft: FT)(implicit
      style: ScalafmtConfig,
      ftoks: FormatTokens,
  ): (OptionalBraces, FT, Boolean) = {
    val impl: Factory = ft.left match {
      case _ if !style.dialect.allowSignificantIndentation => null
      case _: T.Colon => ColonEolImpl
      case _: T.KwWith => WithImpl
      case _: T.RightArrow => RightArrowImpl
      case _: T.ContextArrow => ContextArrowImpl
      case _: T.RightParen => RightParenImpl
      case _: T.KwFor => ForImpl
      case _: T.KwWhile => WhileImpl
      case _: T.KwDo => DoImpl
      case _: T.Equals => EqualsImpl
      case _: T.KwTry => TryImpl
      case _: T.KwCatch => CatchImpl
      case _: T.KwFinally => FinallyImpl
      case _: T.KwMatch => MatchImpl
      case _: T.KwThen => ThenImpl
      case _: T.KwIf => IfImpl
      case _: T.KwElse => ElseImpl
      case _: T.KwReturn | _: T.ContextArrow | _: T.LeftArrow | _: T.KwThrow |
          _: T.KwYield => BlockImpl
      case _ => null
    }
    if (impl eq null) null
    else {
      implicit val ift: FT = ft
      val nft = ftoks.nextNonComment(ft)
      impl.create(nft).nnMap { ob =>
        val skip = nft.right.is[T.LeftBrace] && nft.meta.rightOwner == ob.block
        (ob, nft, skip)
      }
    }
  }

  def get(
      ft: FT,
  )(implicit style: ScalafmtConfig, ftoks: FormatTokens): OptionalBraces =
    getWithBrace(ft) match {
      case (ob, _, false) => ob
      case _ => null
    }

  def at(ft: FT)(implicit style: ScalafmtConfig, ftoks: FormatTokens): Boolean =
    get(ft) ne null

  private def getSplits(
      tree: Tree,
      forceNL: Boolean,
      danglingKeyword: Boolean = true,
      indentOpt: Option[Int] = None,
      forceNLIfTrailingStandaloneComments: Boolean = true,
      nlModOpt: NewlineT = null,
  )(implicit
      fileLine: FileLine,
      style: ScalafmtConfig,
      ft: FT,
      ftoks: FormatTokens,
  ): Seq[Split] = {
    import ftoks._
    val end = getOnOrAfterLast(tree)
    val nonTrivialEnd = prevNonComment(end)
    val slbExpire = nextNonCommentSameLine(nonTrivialEnd)
    def head = getHead(tree)
    val close = {
      val close = tree match {
        case _: Member.Tuple => null
        case Term.Block((_: Member.Tuple) :: Nil)
            if !head.left.is[T.LeftBrace] => null
        case _ => getClosingIfWithinParens(nonTrivialEnd)(head)
      }
      if (close eq null) nextNonCommentSameLine(end)
      else prevNonCommentSameLine(close)
    }
    def nlPolicy(implicit fileLine: FileLine) = Policy ? danglingKeyword && {
      val couldBeTucked = close.right.is[T.CloseDelim] &&
        close.rightOwner.is[Member.SyntaxValuesClause]
      if (!couldBeTucked) decideNewlinesOnlyAfterClose(close)
      else decideNewlinesOnlyAfterToken(rank = 1, ifAny = true)(close)
    }
    val indentLen = indentOpt.getOrElse(style.indent.getSignificant)
    val indent = Indent(indentLen, close, ExpiresOn.After)
    def nlOnly = forceNLIfTrailingStandaloneComments &&
      slbExpire.right.is[T.Comment] && slbExpire.idx < close.idx
    val nlMod = nlModOpt ?? Newline2x(ft)
    if (forceNL || nlMod.isDouble || nlOnly)
      Seq(Split(nlMod, 0).withIndent(indent).withPolicy(nlPolicy))
    else Seq(
      Split(Space, 0).withSingleLine(slbExpire).withIndent(indent),
      Split(nlMod, 1).withIndent(indent).withPolicy(nlPolicy),
    )
  }

  // https://dotty.epfl.ch/docs/reference/other-new-features/indentation.html#variant-indentation-marker-
  // TODO: amend for additional cases when the parser supports them
  private object ColonEolImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): OptionalBraces = {
      import ftoks._
      val lo = ft.meta.leftOwner
      def createImpl(ownerOpt: => Tree) = new OptionalBraces {
        def owner = ownerOpt
        def block: Tree = lo
        def splits = getSplits(lo, forceNL = true)
      }
      lo match {
        case t: Template.Body if getHead(t) eq ft =>
          createImpl(t.parent.parent.orNull)
        case t: Pkg.Body if getHead(t) eq ft => createImpl(t.parentOrNull)
        case t: Stat.Block if getHead(t) eq ft => createImpl(t.parentOrNull)
        case t: Term.ArgClause if getHead(t) eq ft => onArgClause(t, t.values)
        case t: Term => t.parent match {
            case Some(p: Term.ArgClause)
                if hasSingleElement(p, t) && (getHead(p) eq ft) =>
              val stats = t match {
                case b: Term.Block => b.stats
                case _ => t :: Nil
              }
              onArgClause(p, stats)
            case _ => null
          }
        case _ => null
      }
    }

    private def onArgClause(ac: Term.ArgClause, args: List[Tree])(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): OptionalBraces = {
      import ftoks._
      def funcSplit(arg: Member.Function)(implicit fl: FileLine) = {
        val end = getLast(arg)
        getFuncArrow(arg).nnFold(Split(Space, 0).withSingleLine(end)) { arrow =>
          val afterArrow = nextNonCommentSameLine(arrow)
          /* TODO: dialect doesn't indicate if one-line lambdas are allowed.
           * On the other hand, with experimental features, that would force
           * use of Scala3Future which is not granular and might be overkill. */
          val nlOnly = style.newlines.unfold ||
            style.newlines.keepBreak(afterArrow) ||
            !style.runner.dialectFeatures
              .contains(RunnerSettings.DialectFeature.relaxedLambdaSyntax)
          val policy =
            if (nlOnly) decideNewlinesOnlyAfterToken(afterArrow)
            else Policy.onlyFor(afterArrow, "FBARR") { ss =>
              val slb = SingleLineBlock(end)
              val opt = OptimalToken(end, killOnFail = true)
              ss.map(s =>
                if (s.isNL) s
                else s.andPolicy(slb)
                  .withOptimalAt(opt, extend = true, ignore = !s.noCost),
              )
            }
          Split(Space, 0).withSingleLine(afterArrow).andPolicy(policy)
        }
      }
      val indent = ac.parent match {
        case Some(p: Term.Apply) =>
          @tailrec
          def isSelect(ma: Member.Apply): Boolean = ma.fun match {
            case x: Member.Apply => isSelect(x)
            case x => x.is[Term.Select]
          }
          val ok =
            (style.getFewerBraces() match {
              case Indents.FewerBraces.never => true
              case Indents.FewerBraces.always => false
              case Indents.FewerBraces.beforeSelect => !p.parent.is[Term.Select]
            }) || isSelect(p)
          if (ok) None // select is taken care off elsewhere
          else Some(style.indent.main + style.indent.getSignificant)
        case _ => None
      }
      new OptionalBraces {
        def owner = ac.parentOrNull
        def block: Tree = ac
        def splits = args match {
          case (tf: Member.Function) :: Nil
              if (style.newlines.beforeCurlyLambdaParams ne
                Newlines.BeforeCurlyLambdaParams.always) &&
                // https://dotty.epfl.ch/docs/internals/syntax.html
                (tf.paramClause match { // LambdaStart
                  case tpc @ Term.ParamClause(tp :: Nil, mod) => mod.isEmpty &&
                    tp.mods.isEmpty && tp.decltpe.isEmpty ||
                    isEnclosedWithinParens(tpc)
                  case _ => true // multiple params are always in parens
                }) =>
            getSplits(ac, forceNL = false, indentOpt = indent) match {
              case Seq(s, rs @ _*) if !s.isNL => funcSplit(tf)(s.fileLine) +: rs
              case ss => ss
            }
          case _ => getSplits(ac, forceNL = true, indentOpt = indent)
        }
      }
    }
  }

  private object WithImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): OptionalBraces = {
      val lo = ft.meta.leftOwner
      def createImpl(tb: Tree.Block, ownerOpt: => Tree) =
        if (
          nft.right.is[T.LeftBrace] && (nft.meta.rightOwner eq tb) ||
          tb.begOffset > nft.right.start
        ) null
        else new OptionalBraces {
          def owner = ownerOpt
          def block: Tree = lo
          def splits = getSplits(lo, forceNL = true)
        }
      lo match {
        case t: Template => createImpl(t.body, t.parentOrNull)
        case t: Type.Refine => createImpl(t.body, t)
        case _ => null
      }
    }
  }

  private object BlockImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): OptionalBraces = {
      val leftOwner = ft.meta.leftOwner
      findTreeWithParentSimple(nft.meta.rightOwner)(_ eq leftOwner) match {
        case t: Term.Block =>
          val b = getBlockWithNonSingleTermStat(t)
          if ((b eq null) || b.stats.isEmpty) null
          else WithStats(nft, b.stats.head, t, t.parentOrNull)
        case _ => null
      }
    }
  }

  private object RightParenImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): OptionalBraces = {
      import ftoks._
      def createImpl(ownerTree: => Tree, blockTree: => Tree)(
          splitsRef: => Seq[Split],
      ) = new OptionalBraces {
        def owner = ownerTree
        def block: Tree = blockTree
        def splits = splitsRef
      }
      def createKwDo(ownerTree: => Tree, blockTree: => Tree) =
        if (nft.right.is[T.KwDo]) null
        else createImpl(ownerTree, blockTree)(
          if (isTreeSingleExpr(blockTree)) Nil else getSplits(blockTree, true),
        )
      ft.meta.leftOwner match {
        case ParamClauseParent(pp: Defn.ExtensionGroup)
            if !nft.right.is[T.LeftBrace] && (tokenBefore(pp.body) eq ft) =>
          createImpl(pp, pp.body)(
            getSplits(pp.body, shouldBreakInOptionalBraces(ft, nft)),
          )
        case t: Term.If if !nft.right.is[T.KwThen] && {
              !isTreeSingleExpr(t.thenp) || t.thenp.is[Tree.CasesBlock] || {
                val x = getLastNotTrailingComment(t.thenp)
                (x ne null) && x.isLeft
              } ||
              !ifWithoutElse(t) &&
              (isElsePWithOptionalBraces(t) ||
                existsBlockIfWithoutElse(t.thenp, false))
            } => createImpl(t, t.thenp)(getSplitsForIf(nft, t))
        case t: Term.EnumeratorsBlock => t.parent match {
            case Some(p: Term.For) => createKwDo(p, p.body)
            case _ => null
          }
        case t: Term.While => createKwDo(t, t.body)
        case _ => null
      }
    }
  }

  private object RightArrowImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): OptionalBraces = ft.meta.leftOwner match {
      case t: Case => // unsupported except for right brace, or when ends in comment
        new OptionalBraces {
          def owner = null
          def block: Tree = t.body
          def splits = {
            val x = ftoks.getLastNotTrailingComment(t)
            if ((x eq null) || x.isRight) Nil else Seq(Split(Newline2x(ft), 0))
          }
        }
      case t @ Tree.WithBody(b: Tree.CasesBlock) if nft.right.is[T.KwCase] =>
        new OptionalBraces {
          def owner = t
          def block: Tree = b
          def splits = getSplits(b, forceNL = true)
        }
      case t: Term.FunctionLike => FunctionArrowImpl.get(t)
      case _ => BlockImpl.create(nft)
    }
  }

  private object ContextArrowImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): OptionalBraces = ft.leftOwner match {
      case t: Term.FunctionLike => FunctionArrowImpl.get(t)
      case _ => BlockImpl.create(nft)
    }
  }

  private object FunctionArrowImpl {
    def get(t: Term.FunctionLike)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): OptionalBraces = {
      val skip = isTreeSingleExpr(t.body) || isBlockFunction(t)
      if (skip) null // not really optional braces
      else new OptionalBraces {
        def owner = t
        def block: Tree = t.body
        def splits = {
          val (afterCurlySpace, afterCurlyNewlines) = Modification
            .getSpaceAndNewlineAfterCurlyLambda(ft.newlinesBetween)
          getSplits(
            t.body,
            forceNL = !afterCurlySpace || isTreeMultiStatBlock(t.body),
            nlModOpt = afterCurlyNewlines,
          )
        }
      }
    }
  }

  private object ForImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): OptionalBraces = ft.meta.leftOwner match {
      case t @ Tree.WithEnums(x) if isSeqMulti(x) =>
        WithStats(nft, x.head, x.last, t, nlOnly = false)
      case _ => null
    }
  }

  private object WhileImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): OptionalBraces = ft.meta.leftOwner match {
      case t: Term.While => t.expr match {
          case b: Term.Block
              if isMultiStatBlock(b) && ftoks.matchingRightOrNull(nft)
                .orHas(_.left.end < b.endOffset) =>
            new OptionalBraces {
              def owner = t
              def block: Tree = b
              def splits = {
                val dangle = style.danglingParentheses.ctrlSite
                val forceNL = !nft.right.is[T.LeftParen]
                getSplits(b, forceNL = forceNL, danglingKeyword = dangle)
              }
            }
          case _ => null
        }
      case _ => null
    }
  }

  private object DoImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): OptionalBraces = {
      val lo = ft.meta.leftOwner
      def createImpl(body: Tree) = new OptionalBraces {
        def owner = lo
        def block: Tree = body
        def splits = getSplitsMaybeBlock(nft, body)
      }
      lo match {
        case t: Tree.WithBody => createImpl(t.body)
        case _ => null
      }
    }
  }

  private object EqualsImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): OptionalBraces = ft.meta.leftOwner match {
      case t: Ctor.Secondary =>
        if (t.body.stats.isEmpty) null
        else WithStats(nft, t.body.init, t.body, t.parentOrNull)
      case t @ Tree.WithBody(b) =>
        val block = b match {
          case x: Term.Block => getBlockWithNonSingleTermStat(x)
          case x: Tree.CasesBlock => x
          case _ => null
        }
        if (block eq null) getMaybeFewerBracesSelectSplits(b)
        else WithStats(nft, block.stats.head, b, t)
      case _ => BlockImpl.create(nft)
    }
  }

  private object TryImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): OptionalBraces = {
      val lo = ft.meta.leftOwner
      def createImpl(expr: Term, finallyp: Option[Term], usesOB: => Boolean) =
        new OptionalBraces {
          def owner = lo
          def block: Tree = expr
          def splits =
            if (!isTreeSingleExpr(expr)) getSplits(expr, true)
            else if (finallyp.exists(isTreeUsingOptionalBraces) || usesOB)
              getSplits(expr, shouldBreakInOptionalBraces(ft, nft))
            else Nil
        }
      ft.meta.leftOwner match {
        case t: Term.Try =>
          createImpl(t.expr, t.finallyp, isCatchUsingOptionalBraces(t))
        case t: Term.TryWithHandler => createImpl(t.expr, t.finallyp, false)
        case _ => null
      }
    }
  }

  private def isCatchUsingOptionalBraces(
      tree: Term.Try,
  )(implicit ftoks: FormatTokens): Boolean = tree.catchp.headOption
    .exists(x => !ftoks.tokenBefore(x).left.is[T.LeftBrace])

  private object CatchImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): OptionalBraces = ft.meta.leftOwner match {
      case t: Term.Try => t.catchClause match {
          case Some(cb: Term.CasesBlock) =>
            val nlOnly = cb.cases match {
              // to avoid next expression being interpreted as body
              case head :: Nil => shouldBreakInOptionalBraces(ft, nft) ||
                t.finallyp.isEmpty &&
                (isEmptyTree(head.body) || {
                  val x = ftoks.getLast(cb)
                  (x ne null) && {
                    val xend = ftoks.nextNonCommentSameLine(x)
                    xend.right match {
                      case _: T.Comment => !xend.hasBlankLine
                      case _ => false
                    }
                  }
                })
              case _ => true
            }
            new OptionalBraces {
              def owner = t
              def block: Tree = cb
              def splits = getSplits(
                cb,
                forceNL = nlOnly,
                forceNLIfTrailingStandaloneComments = false,
              )
            }
          case _ => null
        }
      case _ => null
    }
  }

  private object FinallyImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): OptionalBraces = {
      val lo = ft.meta.leftOwner
      def createImpl(usingOB: => Boolean)(finallyExpr: Tree) = {
        val isMulti = !isTreeSingleExpr(finallyExpr)
        def usesOB = isMulti || usingOB
        def forceNL = isMulti || shouldBreakInOptionalBraces(ft, nft)
        new OptionalBraces {
          def owner = lo
          def block: Tree = finallyExpr
          def splits = if (usesOB) getSplits(finallyExpr, forceNL) else Nil
        }
      }
      lo match {
        case t: Term.Try => t.finallyp match {
            case Some(x) => createImpl(
                isCatchUsingOptionalBraces(t) ||
                  isTreeUsingOptionalBraces(t.expr),
              )(x)
            case _ => null
          }
        case t: Term.TryWithHandler => t.finallyp match {
            case Some(x) => createImpl(isTreeUsingOptionalBraces(t.expr))(x)
            case _ => null
          }
        case _ => null
      }
    }
  }

  private object MatchImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): OptionalBraces = ft.meta.leftOwner match {
      case t @ Tree.WithCasesBlock(x) =>
        val ind = style.indent.matchSite
        WithStats.fromStats(nft, x.cases, t, t, indentOpt = ind)
      case _ => null
    }
  }

  private object ThenImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): OptionalBraces = ft.meta.leftOwner match {
      case t: Term.If => new OptionalBraces {
          def owner = t
          def block: Tree = t.thenp
          def splits = getSplitsForIf(nft, t)
        }
      case _ => null
    }
  }

  private object IfImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): OptionalBraces = ft.meta.leftOwner match {
      case t: Term.If => t.cond match {
          case b: Term.Block
              if ftoks.matchingRightOrNull(nft)
                .nnFold(isMultiStatBlock(b))(_.left.end < b.endOffset) =>
            new OptionalBraces {
              def owner = t
              def block: Tree = b
              def splits = {
                val dangle = style.danglingParentheses.ctrlSite
                val forceNL = !nft.right.is[T.LeftParen]
                getSplits(b, forceNL, dangle)
              }
            }
          case _ => null
        }
      case _ => null
    }
  }

  private object ElseImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): OptionalBraces = ft.meta.leftOwner match {
      case t: Term.If =>
        val forceNL = getTreeSingleExpr(t.elsep) match {
          case x: Term.If =>
            val forceNL = ftoks.isJustBeforeTree(nft)(x) && ft.hasBreak &&
              ((ft ne nft) || style.newlines.keep)
            if (forceNL) MaybeBool.True else MaybeBool.Maybe
          case _: Tree.CasesBlock | null => MaybeBool.True
          case _ if {
                val x = ftoks.getLastNotTrailingComment(t)
                (x ne null) && x.isLeft
              } => MaybeBool.True
          case _ if !isThenPWithOptionalBraces(t) => MaybeBool.Maybe
          case _ => MaybeBool(shouldBreakInOptionalBraces(ft, nft))
        }
        if (forceNL eq MaybeBool.Maybe) null
        else new OptionalBraces {
          def owner = t
          def block: Tree = t.elsep
          def splits = getSplits(t.elsep, forceNL.asBoolean)
        }
      case _ => null
    }
  }

  private def getSplitsMaybeBlock(
      nft: FT,
      tree: Tree,
      danglingKeyword: Boolean = true,
  )(implicit
      fileLine: FileLine,
      style: ScalafmtConfig,
      ft: FT,
      ftoks: FormatTokens,
  ): Seq[Split] = {
    val forceNL = !hasSingleTermStatIfBlock(tree) ||
      shouldBreakInOptionalBraces(ft, nft) || tree.is[Tree.CasesBlock]
    getSplits(tree, forceNL, danglingKeyword)
  }

  private def getMaybeFewerBracesSelectSplits(body: Tree)(implicit
      ft: FT,
      style: ScalafmtConfig,
      ftoks: FormatTokens,
  ): OptionalBraces = {
    import ftoks._
    val selectLike = Select.first(body, enclosed = true)
    val keep = selectLike.nnHas(_.qual match {
      case q @ (_: Term.ForClause | _: Term.ApplyInfix |
          _: Term.SelectPostfix) => !isEnclosedInMatching(q)
      case _ => false
    })
    if (!keep) return null
    new OptionalBraces {
      def splits: Seq[Split] = {
        val noFbIndent = style.getFewerBraces() == Indents.FewerBraces.never
        val indentLen =
          if (noFbIndent) style.indent.getSignificant
          else style.indent.main + style.indent.getSignificant
        val dot = prev(prevNonCommentBefore(selectLike.nameFt))
        val beforeDot = prevNonCommentSameLine(dot)
        val policy = Policy.End <= beforeDot ==>
          Policy.onRight(dot, "NL-NOIND-SELECT") {
            case Decision(`beforeDot`, ss) => ss.flatMap(s =>
                if ((dot eq beforeDot) && !s.isNL) None
                else Some(s.deActivateFor(SplitTag.SelectChainFirstNL)),
              )
            case Decision(_, ss) => ss
                .map(_.deActivateFor(SplitTag.SelectChainFirstNL))
          }
        val nlSplit = Split(Newline2x(ft), 1, policy)
          .withIndent(indentLen, beforeDot, ExpiresOn.After)
        Seq(nlSplit)
      }
      def block: Tree = body
      def owner: Tree = body.parentOrNull
    }
  }

  private class WithStats private (
      nft: FT,
      body: Tree,
      val owner: Tree,
      nlOnly: Boolean,
      indentOpt: Option[Int],
  )(implicit
      fileLine: FileLine,
      style: ScalafmtConfig,
      ft: FT,
      ftoks: FormatTokens,
  ) extends OptionalBraces {
    def splits: Seq[Split] = {
      val forceNL = nlOnly || shouldBreakInOptionalBraces(ft, nft)
      getSplits(body, forceNL = forceNL, indentOpt = indentOpt)
    }
    def block: Tree = body
  }

  private object WithStats {
    def apply(
        nft: FT,
        head: Tree,
        body: => Tree,
        owner: => Tree,
        nlOnly: Boolean = true,
        indentOpt: Option[Int] = None,
    )(implicit
        fileLine: FileLine,
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): WithStats =
      if (head eq null) null
      else if (!ftoks.isJustBeforeTree(nft)(head)) null
      else new WithStats(nft, body, owner, nlOnly, indentOpt)

    def fromStats(
        nft: FT,
        stats: Seq[Tree],
        body: => Tree,
        owner: => Tree,
        nlOnly: Boolean = true,
        indentOpt: Option[Int] = None,
    )(implicit
        fileLine: FileLine,
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): WithStats =
      if (stats.isEmpty) null
      else apply(nft, stats.head, body, owner, nlOnly, indentOpt)
  }

  private def getSplitsForIf(nft: FT, t: Term.If)(implicit
      fileLine: FileLine,
      style: ScalafmtConfig,
      ft: FT,
      ftoks: FormatTokens,
  ): Seq[Split] = {
    def nestedIf(x: Term.If) = {
      val forceNL = shouldBreakInOptionalBraces(ft, nft) ||
        !ifWithoutElse(t) && existsIfWithoutElse(x)
      getSplits(t.thenp, forceNL)
    }
    t.thenp match {
      case x: Term.If => nestedIf(x)
      case Term.Block((x: Term.If) :: Nil) => nestedIf(x)
      case x => getSplitsMaybeBlock(nft, x, false)
    }
  }

  private def isThenPWithOptionalBraces(
      tree: Term.If,
  )(implicit ftoks: FormatTokens): Boolean = {
    import ftoks._
    val thenp = tree.thenp
    val before = tokenJustBefore(thenp)
    prevNonComment(before).left match {
      case _: T.KwThen => true
      case _: T.LeftBrace => false
      case _ => !isTreeSingleExpr(thenp) &&
        (!before.right.is[T.LeftBrace] || {
          val m = matchingRightOrNull(before)
          (m ne null) && m.left.end < thenp.endOffset
        })
    }
  }

  @tailrec
  private def isElsePWithOptionalBraces(
      tree: Term.If,
  )(implicit ftoks: FormatTokens): Boolean = {
    val elsep = tree.elsep
    !ftoks.getHead(elsep).left.is[T.LeftBrace] &&
    (elsep match {
      case t: Term.If => isThenPWithOptionalBraces(t) ||
        !ifWithoutElse(t) && isElsePWithOptionalBraces(t)
      case Term.Block((t: Term.If) :: Nil) => isThenPWithOptionalBraces(t) ||
        !ifWithoutElse(t) && isElsePWithOptionalBraces(t)
      case t => !isTreeSingleExpr(t)
    })
  }

  private def shouldBreakInOptionalBraces(ft: FT, nft: FT)(implicit
      style: ScalafmtConfig,
  ): Boolean = style.newlines.source match {
    case Newlines.unfold => true
    case Newlines.fold => false
    case Newlines.keep => ft.hasBreak
    case _ => (ft ne nft) && ft.hasBreak
  }

  private def isTreeUsingOptionalBraces(tree: Tree)(implicit
      ftoks: FormatTokens,
  ): Boolean = !isTreeSingleExpr(tree) && !ftoks.tokenBefore(tree)
    .left.is[T.LeftBrace]

  def indentAndBreakBeforeCtrl[A](tree: Tree, split: Split)(implicit
      style: ScalafmtConfig,
      classifier: Classifier[T, A],
      ftoks: FormatTokens,
  ): Split = {
    import ftoks._
    if (!style.dialect.allowSignificantIndentation) return null

    val head = headTokenOrNull(tree)
    val hft = after(head)
    if (hft.left.eq(head) && tree.is[Term.Block] && !split.isNL) return null

    val beg = getOnOrBeforeOwned(hft, tree)
    val end = getLastNonTrivial(tree)
    val kw = next {
      val close = getClosingIfWithinParens(end)(beg)
      if (close eq null) end else next(close)
    }
    if (!kw.left.is[A]) return null

    val indent = style.indent.ctrlSite.getOrElse(style.indent.getSignificant)
    def policy =
      if (split.isNL) decideNewlinesOnlyBeforeClose(kw)
      else decideNewlinesOnlyBeforeCloseOnBreak(kw)
    split.withIndent(indent, kw, ExpiresOn.Before)
      .andPolicy(if (style.danglingParentheses.ctrlSite) policy else null)
  }

}
