package org.scalafmt.internal

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
  def owner: Option[Tree]
  def block: Tree
  def splits: Option[Seq[Split]]
}

object OptionalBraces {

  private trait Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): Option[OptionalBraces]
  }

  // Optional braces in templates after `:|with`
  // Optional braces after any token that can start indentation:
  // )  =  =>  ?=>  <-  catch  do  else  finally  for
  // if  match  return  then  throw  try  while  yield

  def getWithBrace(ft: FT)(implicit
      style: ScalafmtConfig,
      ftoks: FormatTokens,
  ): Option[(OptionalBraces, FT, Boolean)] =
    if (!style.dialect.allowSignificantIndentation) None
    else Option {
      ft.left match {
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
    }.flatMap { impl =>
      implicit val ift: FT = ft
      val nft = ftoks.nextNonComment(ft)
      impl.create(nft).map { ob =>
        val skip = nft.right.is[T.LeftBrace] && nft.meta.rightOwner == ob.block
        (ob, nft, skip)
      }
    }

  def get(ft: FT)(implicit
      style: ScalafmtConfig,
      ftoks: FormatTokens,
  ): Option[OptionalBraces] = getWithBrace(ft) match {
    case Some((ob, _, false)) => Some(ob)
    case _ => None
  }

  def at(ft: FT)(implicit style: ScalafmtConfig, ftoks: FormatTokens): Boolean =
    get(ft).nonEmpty

  private def getSplits(
      tree: Tree,
      forceNL: Boolean,
      danglingKeyword: Boolean = true,
      indentOpt: Option[Int] = None,
      forceNLIfTrailingStandaloneComments: Boolean = true,
      nlModOpt: Option[NewlineT] = None,
  )(implicit
      fileLine: FileLine,
      style: ScalafmtConfig,
      ft: FT,
      ftoks: FormatTokens,
  ): Seq[Split] = {
    import ftoks._
    val treeTokens = tree.tokens
    val end = getOnOrAfterLast(treeTokens, tree)
    val nonTrivialEnd = prevNonComment(end)
    val slbExpire = nextNonCommentSameLine(nonTrivialEnd)
    def head = getHead(treeTokens, tree)
    val close = (tree match {
      case _: Member.Tuple => None
      case Term.Block((_: Member.Tuple) :: Nil) if !head.left.is[T.LeftBrace] =>
        None
      case _ => getClosingIfWithinParens(nonTrivialEnd)(head)
          .map(prevNonCommentSameLine)
    }).getOrElse(nextNonCommentSameLine(end))
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
    val nlMod = nlModOpt.getOrElse(Newline2x(ft))
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
    ): Option[OptionalBraces] = {
      import ftoks._
      val lo = ft.meta.leftOwner
      def createImpl(ownerOpt: => Option[Tree]) = Some(new OptionalBraces {
        def owner = ownerOpt
        def block: Tree = lo
        def splits = Some(getSplits(lo, forceNL = true))
      })
      lo match {
        case t: Template.Body if getHeadOpt(t).contains(ft) =>
          createImpl(t.parent.parent)
        case t: Pkg.Body if getHeadOpt(t).contains(ft) => createImpl(t.parent)
        case t: Stat.Block if getHeadOpt(t).contains(ft) => createImpl(t.parent)
        case t: Term.ArgClause if getHead(t) eq ft => onArgClause(t, t.values)
        case t: Term => t.parent match {
            case Some(p: Term.ArgClause)
                if hasSingleElement(p, t) && (getHead(p) eq ft) =>
              val stats = t match {
                case b: Term.Block => b.stats
                case _ => t :: Nil
              }
              onArgClause(p, stats)
            case _ => None
          }
        case _ => None
      }
    }

    private def onArgClause(ac: Term.ArgClause, args: List[Tree])(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): Option[OptionalBraces] = {
      import ftoks._
      def funcSplit(arg: Member.Function)(implicit fl: FileLine) = {
        val end = getLast(arg)
        getFuncArrow(arg).fold(Split(Space, 0).withSingleLine(end)) { arrow =>
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
              val opt = Some(OptimalToken(end, killOnFail = true))
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
      Some {
        new OptionalBraces {
          def owner = ac.parent
          def block: Tree = ac
          def splits = Some {
            args match {
              case (tf: Member.Function) :: Nil
                  if (style.newlines.beforeCurlyLambdaParams ne
                    Newlines.BeforeCurlyLambdaParams.always) &&
                    // https://dotty.epfl.ch/docs/internals/syntax.html
                    (tf.paramClause match { // LambdaStart
                      case tpc @ Term.ParamClause(tp :: Nil, mod) =>
                        mod.isEmpty && tp.mods.isEmpty && tp.decltpe.isEmpty ||
                        isEnclosedWithinParens(tpc)
                      case _ => true // multiple params are always in parens
                    }) =>
                getSplits(ac, forceNL = false, indentOpt = indent) match {
                  case s +: rs if !s.isNL => funcSplit(tf)(s.fileLine) +: rs
                  case ss => ss
                }
              case _ => getSplits(ac, forceNL = true, indentOpt = indent)
            }
          }
        }
      }
    }
  }

  private object WithImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): Option[OptionalBraces] = {
      val lo = ft.meta.leftOwner
      def createImpl(tb: Tree.Block, ownerOpt: => Option[Tree]) =
        if (
          nft.right.is[T.LeftBrace] && (nft.meta.rightOwner eq tb) ||
          tb.pos.start > nft.right.start
        ) None
        else Some(new OptionalBraces {
          def owner = ownerOpt
          def block: Tree = lo
          def splits = Some(getSplits(lo, forceNL = true))
        })
      lo match {
        case t: Template => createImpl(t.body, t.parent)
        case t: Type.Refine => createImpl(t.body, Some(t))
        case _ => None
      }
    }
  }

  private object BlockImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): Option[OptionalBraces] = {
      val leftOwner = ft.meta.leftOwner
      findTreeWithParentSimple(nft.meta.rightOwner)(_ eq leftOwner) match {
        case Some(t: Term.Block) => getBlockWithNonSingleTermStat(t)
            .flatMap(b => WithStats(nft, b.stats.headOption, t, t.parent))
        case _ => None
      }
    }
  }

  private object RightParenImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): Option[OptionalBraces] = {
      import ftoks._
      def createImpl(ownerTree: => Tree, blockTree: => Tree)(
          splitsRef: => Option[Seq[Split]],
      ) = Some(new OptionalBraces {
        def owner = Some(ownerTree)
        def block: Tree = blockTree
        def splits = splitsRef
      })
      def createKwDo(ownerTree: => Tree, blockTree: => Tree) =
        if (nft.right.is[T.KwDo]) None
        else createImpl(ownerTree, blockTree)(
          if (isTreeSingleExpr(blockTree)) None
          else Some(getSplits(blockTree, true)),
        )
      ft.meta.leftOwner match {
        case ParamClauseParent(pp: Defn.ExtensionGroup)
            if !nft.right.is[T.LeftBrace] && (tokenBefore(pp.body) eq ft) =>
          createImpl(pp, pp.body)(Some(
            getSplits(pp.body, shouldBreakInOptionalBraces(ft, nft)),
          ))
        case t: Term.If if !nft.right.is[T.KwThen] && {
              !isTreeSingleExpr(t.thenp) || t.thenp.is[Tree.CasesBlock] ||
              getLastNotTrailingCommentOpt(t.thenp).exists(_.isLeft) ||
              !ifWithoutElse(t) &&
              (isElsePWithOptionalBraces(t) ||
                existsBlockIfWithoutElse(t.thenp, false))
            } => createImpl(t, t.thenp)(Some(getSplitsForIf(nft, t)))
        case t: Term.EnumeratorsBlock => t.parent.flatMap {
            case p: Term.For => createKwDo(p, p.body)
            case _ => None
          }
        case t: Term.While => createKwDo(t, t.body)
        case _ => None
      }
    }
  }

  private object RightArrowImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): Option[OptionalBraces] = ft.meta.leftOwner match {
      case t: Case => // unsupported except for right brace, or when ends in comment
        Some(new OptionalBraces {
          def owner = None
          def block: Tree = t.body
          def splits =
            if (ftoks.getLastNotTrailingCommentOpt(t).forall(_.isRight)) None
            else Some(Seq(Split(Newline2x(ft), 0)))
        })
      case t @ Tree.WithBody(b: Tree.CasesBlock) if nft.right.is[T.KwCase] =>
        Some(new OptionalBraces {
          def owner = Some(t)
          def block: Tree = b
          def splits = Some(getSplits(b, forceNL = true))
        })
      case t: Term.FunctionLike => FunctionArrowImpl.get(t, nft)
      case _ => BlockImpl.create(nft)
    }
  }

  private object ContextArrowImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): Option[OptionalBraces] = ft.leftOwner match {
      case t: Term.FunctionLike => FunctionArrowImpl.get(t, nft)
      case _ => BlockImpl.create(nft)
    }
  }

  private object FunctionArrowImpl {
    def get(t: Term.FunctionLike, nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): Option[OptionalBraces] = {
      val skip = isTreeSingleExpr(t.body) || isBlockFunction(t)
      if (skip) None // not really optional braces
      else Some(new OptionalBraces {
        def owner = Some(t)
        def block: Tree = t.body
        def splits = {
          val (afterCurlySpace, afterCurlyNewlines) = Modification
            .getSpaceAndNewlineAfterCurlyLambda(ft.newlinesBetween)
          Some(getSplits(
            t.body,
            forceNL = !afterCurlySpace || isTreeMultiStatBlock(t.body),
            nlModOpt = Some(afterCurlyNewlines),
          ))
        }
      })
    }
  }

  private object ForImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): Option[OptionalBraces] = ft.meta.leftOwner match {
      case t @ Tree.WithEnums(x) if isSeqMulti(x) =>
        WithStats(nft, x.headOption, x.last, Some(t), nlOnly = false)
      case _ => None
    }
  }

  private object WhileImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): Option[OptionalBraces] = ft.meta.leftOwner match {
      case t: Term.While => t.expr match {
          case b: Term.Block
              if isMultiStatBlock(b) &&
                !ftoks.matchingOptRight(nft).exists(_.left.end >= b.pos.end) =>
            Some(new OptionalBraces {
              def owner = Some(t)
              def block: Tree = b
              def splits = Some {
                val dangle = style.danglingParentheses.ctrlSite
                val forceNL = !nft.right.is[T.LeftParen]
                getSplits(b, forceNL = forceNL, danglingKeyword = dangle)
              }
            })
          case _ => None
        }
      case _ => None
    }
  }

  private object DoImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): Option[OptionalBraces] = {
      val lo = ft.meta.leftOwner
      def createImpl(body: Tree) = Some(new OptionalBraces {
        def owner = Some(lo)
        def block: Tree = body
        def splits = Some(getSplitsMaybeBlock(nft, body))
      })
      lo match {
        case t: Tree.WithBody => createImpl(t.body)
        case _ => None
      }
    }
  }

  private object EqualsImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): Option[OptionalBraces] = ft.meta.leftOwner match {
      case t: Ctor.Secondary =>
        if (t.body.stats.isEmpty) None
        else WithStats(nft, Some(t.body.init), t.body, t.parent)
      case t @ Tree.WithBody(b) => (b match {
          case x: Term.Block => getBlockWithNonSingleTermStat(x)
          case x: Tree.CasesBlock => Some(x)
          case _ => None
        }).fold(getMaybeFewerBracesSelectSplits(b))(x =>
          WithStats(nft, x.stats.headOption, b, Some(t)),
        )
      case _ => BlockImpl.create(nft)
    }
  }

  private object TryImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): Option[OptionalBraces] = {
      val lo = ft.meta.leftOwner
      def createImpl(expr: Term, finallyp: Option[Term], usesOB: => Boolean) =
        Some(new OptionalBraces {
          def owner = Some(lo)
          def block: Tree = expr
          def splits =
            if (!isTreeSingleExpr(expr)) Some(getSplits(expr, true))
            else if (finallyp.exists(isTreeUsingOptionalBraces) || usesOB)
              Some(getSplits(expr, shouldBreakInOptionalBraces(ft, nft)))
            else None
        })
      ft.meta.leftOwner match {
        case t: Term.Try =>
          createImpl(t.expr, t.finallyp, isCatchUsingOptionalBraces(t))
        case t: Term.TryWithHandler => createImpl(t.expr, t.finallyp, false)
        case _ => None
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
    ): Option[OptionalBraces] = ft.meta.leftOwner match {
      case t: Term.Try => t.catchClause match {
          case Some(cb: Term.CasesBlock) =>
            val nlOnly = cb.cases match {
              // to avoid next expression being interpreted as body
              case head :: Nil => shouldBreakInOptionalBraces(ft, nft) ||
                t.finallyp.isEmpty &&
                (isEmptyTree(head.body) || ftoks.getLastOpt(cb).exists { x =>
                  val xend = ftoks.nextNonCommentSameLine(x)
                  xend.right match {
                    case _: T.Comment => !xend.hasBlankLine
                    case _ => false
                  }
                })
              case _ => true
            }
            Some(new OptionalBraces {
              def owner = Some(t)
              def block: Tree = cb
              def splits = Some(getSplits(
                cb,
                forceNL = nlOnly,
                forceNLIfTrailingStandaloneComments = false,
              ))
            })
          case _ => None
        }
      case _ => None
    }
  }

  private object FinallyImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): Option[OptionalBraces] = {
      val lo = ft.meta.leftOwner
      def createImpl(usingOB: => Boolean)(finallyExpr: Tree) = {
        val isMulti = !isTreeSingleExpr(finallyExpr)
        def usesOB = isMulti || usingOB
        def forceNL = isMulti || shouldBreakInOptionalBraces(ft, nft)
        new OptionalBraces {
          def owner = Some(lo)
          def block: Tree = finallyExpr
          def splits =
            if (usesOB) Some(getSplits(finallyExpr, forceNL)) else None
        }
      }
      lo match {
        case t: Term.Try => t.finallyp.map(createImpl(
            isCatchUsingOptionalBraces(t) || isTreeUsingOptionalBraces(t.expr),
          ))
        case t: Term.TryWithHandler => t.finallyp
            .map(createImpl(isTreeUsingOptionalBraces(t.expr)))
        case _ => None
      }
    }
  }

  private object MatchImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): Option[OptionalBraces] = ft.meta.leftOwner match {
      case t @ Tree.WithCasesBlock(x) =>
        val ind = style.indent.matchSite
        WithStats(nft, x.cases.headOption, t, Some(t), indentOpt = ind)
      case _ => None
    }
  }

  private object ThenImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): Option[OptionalBraces] = ft.meta.leftOwner match {
      case t: Term.If => Some(new OptionalBraces {
          def owner = Some(t)
          def block: Tree = t.thenp
          def splits = Some(getSplitsForIf(nft, t))
        })
      case _ => None
    }
  }

  private object IfImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): Option[OptionalBraces] = ft.meta.leftOwner match {
      case t: Term.If => t.cond match {
          case b: Term.Block if (ftoks.matchingOptRight(nft) match {
                case Some(t) => t.left.end < b.pos.end
                case None => isMultiStatBlock(b)
              }) =>
            Some(new OptionalBraces {
              def owner = Some(t)
              def block: Tree = b
              def splits = Some {
                val dangle = style.danglingParentheses.ctrlSite
                val forceNL = !nft.right.is[T.LeftParen]
                getSplits(b, forceNL, dangle)
              }
            })
          case _ => None
        }
      case _ => None
    }
  }

  private object ElseImpl extends Factory {
    def create(nft: FT)(implicit
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): Option[OptionalBraces] = ft.meta.leftOwner match {
      case t: Term.If => (getTreeSingleExpr(t.elsep) match {
          case Some(x: Term.If) =>
            val forceNL = ftoks.isJustBeforeTree(nft)(x) && ft.hasBreak &&
              ((ft ne nft) || style.newlines.keep)
            if (forceNL) Some(true) else None
          case Some(_: Tree.CasesBlock) => Some(true)
          case Some(_)
              if !ftoks.getLastNotTrailingCommentOpt(t).exists(_.isLeft) =>
            if (!isThenPWithOptionalBraces(t)) None
            else Some(shouldBreakInOptionalBraces(ft, nft))
          case _ => Some(true)
        }).map(forceNL =>
          new OptionalBraces {
            def owner = Some(t)
            def block: Tree = t.elsep
            def splits = Some(getSplits(t.elsep, forceNL))
          },
        )
      case _ => None
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

  private def getMaybeFewerBracesSelectSplits(
      body: Tree,
  )(implicit ft: FT, style: ScalafmtConfig, ftoks: FormatTokens) = {
    import ftoks._
    Select.first(body, enclosed = true).filter(_.qual match {
      case q @ (_: Term.ForClause | _: Term.ApplyInfix |
          _: Term.SelectPostfix) => !isEnclosedInMatching(q)
      case _ => false
    }).map { selectLike =>
      new OptionalBraces {
        def splits: Option[Seq[Split]] = {
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
          Some(Seq(nlSplit))
        }
        def block: Tree = body
        def owner: Option[Tree] = body.parent
      }
    }
  }

  private class WithStats private (
      nft: FT,
      body: Tree,
      val owner: Option[Tree],
      nlOnly: Boolean,
      indentOpt: Option[Int],
  )(implicit
      fileLine: FileLine,
      style: ScalafmtConfig,
      ft: FT,
      ftoks: FormatTokens,
  ) extends OptionalBraces {
    def splits: Option[Seq[Split]] = {
      val forceNL = nlOnly || shouldBreakInOptionalBraces(ft, nft)
      Some(getSplits(body, forceNL = forceNL, indentOpt = indentOpt))
    }
    def block: Tree = body
  }

  private object WithStats {
    def apply(
        nft: FT,
        head: Option[Tree],
        body: => Tree,
        owner: => Option[Tree],
        nlOnly: Boolean = true,
        indentOpt: Option[Int] = None,
    )(implicit
        fileLine: FileLine,
        style: ScalafmtConfig,
        ft: FT,
        ftoks: FormatTokens,
    ): Option[WithStats] = head.flatMap(head =>
      if (!ftoks.isJustBeforeTree(nft)(head)) None
      else Some(new WithStats(nft, body, owner, nlOnly, indentOpt)),
    )
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
        (!before.right.is[T.LeftBrace] || matchingOptRight(before)
          .exists(_.left.end < thenp.pos.end))
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
  ): Option[Split] = Some {
    import ftoks._
    if (!style.dialect.allowSignificantIndentation) return None

    val treeTokens = tree.tokens
    val head = treeTokens.head
    val hft = after(head)
    if (hft.left.eq(head) && tree.is[Term.Block] && !split.isNL) return None

    val beg = getOnOrBeforeOwned(hft, tree)
    val end = getLastNonTrivial(treeTokens, tree)
    val kw = next(getClosingIfWithinParens(end)(beg).fold(end)(next))
    if (!kw.left.is[A]) return None

    val indent = style.indent.ctrlSite.getOrElse(style.indent.getSignificant)
    def policy =
      if (split.isNL) decideNewlinesOnlyBeforeClose(kw)
      else decideNewlinesOnlyBeforeCloseOnBreak(kw)
    split.withIndent(indent, kw, ExpiresOn.Before)
      .andPolicy(policy, !style.danglingParentheses.ctrlSite)
  }

}
