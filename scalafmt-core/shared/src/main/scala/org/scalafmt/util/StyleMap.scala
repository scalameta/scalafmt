package org.scalafmt.util

import scala.annotation.tailrec
import scala.collection.mutable
import scala.meta.Lit
import scala.meta.Name
import scala.meta.Term
import scala.meta.Tree
import scala.meta.tokens.Token
import scala.meta.tokens.Token.Comment
import scala.meta.tokens.Token.LeftParen
import scala.meta.tokens.Token.RightParen
import metaconfig.Configured
import org.scalafmt.config.Config
import org.scalafmt.config.FilterMatcher
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.FormatToken
import org.scalafmt.internal.FormatTokens
import org.scalafmt.util.TokenOps.TokenHash
import org.scalameta.FileLine
import org.scalameta.logger

class StyleMap(
    tokens: FormatTokens,
    val init: ScalafmtConfig
) {
  import StyleMap._
  import TokenOps.hash
  val literalR: FilterMatcher = init.binPack.literalsRegex
  private val prefix = "\\s*scalafmt: ".r
  val forcedBinPack: mutable.Set[Tree] = mutable.Set.empty
  val (isEmpty: Boolean, tok2style: Map[TokenHash, ScalafmtConfig]) = {
    var curr = init
    var empty = true
    val map = Map.newBuilder[TokenHash, ScalafmtConfig]
    val disableBinPack = mutable.Set.empty[Token]
    def warn(err: String)(implicit fileLine: FileLine): Unit = logger.elem(err)
    tokens.arr.foreach { tok =>
      def changeStyle(style: ScalafmtConfig): Boolean = {
        val changing = curr != style
        if (changing) {
          empty = false
          curr = style
        }
        changing
      }
      tok.left match {
        case Comment(c) if prefix.findFirstIn(c).isDefined =>
          Config.fromHoconString(c, Some("scalafmt"), init) match {
            case Configured.Ok(style) =>
              if (init.rewrite ne style.rewrite)
                warn("May not override rewrite settings")
              else if (
                init.trailingCommas != style.trailingCommas ||
                init.runner.dialect.allowTrailingCommas !=
                  style.runner.dialect.allowTrailingCommas
              )
                warn("May not override rewrite settings (trailingCommas)")
              changeStyle(style)
            case Configured.NotOk(e) =>
              // TODO(olafur) report error via callback
              logger.elem(e)
          }
        case open @ LeftParen()
            if curr.binPack.literalArgumentLists &&
              opensLiteralArgumentList(tok)(curr) =>
          forcedBinPack += tok.meta.leftOwner
          if (changeStyle(setBinPack(curr, callSite = true)))
            tokens.matchingOpt(open).foreach(disableBinPack += _)
        case close @ RightParen() if disableBinPack(close) =>
          changeStyle(setBinPack(curr, callSite = false))
        case _ =>
      }
      if (!empty) {
        // Minor optimization? Only add to map if needed.
        map += hash(tok.left) -> curr
      }
    }
    (empty, map.result())
  }

  @tailrec
  private def isBasicLiteral(
      tree: Tree
  )(implicit style: ScalafmtConfig): Boolean =
    tree match {
      case lit: Lit =>
        val strName = tree match {
          case t: Lit.Int
              if 0 <= t.value && t.value < Byte.MaxValue &&
                lit.tokens.head.toString.startsWith("0x") =>
            "Byte"
          case _: Lit.Null => "Null"
          case _ => lit.value.getClass.getName
        }
        literalR.matches(strName)
      case x: Name => literalR.matches(x.productPrefix)
      case _ if !style.binPack.literalsIncludeSimpleExpr => false
      case t: Term.Select => isBasicLiteral(t.qual)
      case t: Term.Assign => isBasicLiteral(t.rhs)
      case _ =>
        tree.children match {
          case Nil => true
          case one :: Nil => isBasicLiteral(one)
          case _ => false
        }
    }

  @tailrec
  private def isLiteral(tree: Tree)(implicit style: ScalafmtConfig): Boolean =
    isBasicLiteral(tree) ||
      style.binPack.literalsIncludeSimpleExpr && (tree match {
        case t: Term.Assign => isLiteral(t.rhs)
        case t: Term.Apply =>
          isBasicLiteral(t.fun) && (t.args match {
            case Nil => true
            case arg :: Nil => isLiteral(arg)
            case _ => false
          })
        case _ =>
          tree.children match {
            case Nil => true
            case one :: Nil => isLiteral(one)
            case _ => false
          }
      })

  def opensLiteralArgumentList(
      ft: FormatToken
  )(implicit style: ScalafmtConfig): Boolean =
    ft.meta.leftOwner match {
      case TreeOps.SplitCallIntoParts(_, eitherArgs) =>
        eitherArgs
          .fold(Some(_), TokenOps.findArgsFor(ft.left, _, tokens.matchingOpt))
          .exists { args =>
            args.length > style.binPack.literalsMinArgCount &&
            args.forall(isLiteral)
          }
      case _ => false
    }

  @inline
  def at(token: FormatToken): ScalafmtConfig =
    at(token.left)

  @inline
  def at(token: Token): ScalafmtConfig =
    tok2style.getOrElse(hash(token), init)

  private[util] def numEntries: Int = tok2style.size

}

object StyleMap {

  def setBinPack(curr: ScalafmtConfig, callSite: Boolean): ScalafmtConfig =
    if (curr.binPack.unsafeCallSite == callSite) curr
    else curr.copy(binPack = curr.binPack.copy(unsafeCallSite = callSite))

}
