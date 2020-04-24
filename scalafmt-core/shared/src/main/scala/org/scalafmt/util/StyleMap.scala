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
import org.scalameta.logger

class StyleMap(
    tokens: FormatTokens,
    init: ScalafmtConfig,
    owners: collection.Map[TokenHash, Tree],
    matching: Map[TokenHash, Token]
) {
  import TokenOps.hash
  val literalR: FilterMatcher = init.binPack.literalsRegex
  private val prefix = "\\s*scalafmt: ".r
  val forcedBinPack: mutable.Set[Tree] = mutable.Set.empty
  val (isEmpty: Boolean, tok2style: Map[TokenHash, ScalafmtConfig]) = {
    var curr = init
    var empty = true
    val map = Map.newBuilder[TokenHash, ScalafmtConfig]
    val disableBinPack = mutable.Set.empty[Token]
    tokens.arr.foreach { tok =>
      tok.left match {
        case Comment(c) if prefix.findFirstIn(c).isDefined =>
          Config.fromHoconString(c, Some("scalafmt"), init) match {
            case Configured.Ok(style) =>
              empty = false
              curr = style
            case Configured.NotOk(
                  e
                ) => // TODO(olafur) report error via callback
              logger.elem(e)
          }
        case open @ LeftParen()
            if init.binPack.literalArgumentLists &&
              opensLiteralArgumentList(tok) =>
          forcedBinPack += owners(hash(open))
          empty = false
          curr = setBinPack(curr, callSite = true)
          disableBinPack += matching(hash(open))
        case close @ RightParen() if disableBinPack(close) =>
          curr = setBinPack(curr, callSite = false)
        case _ =>
      }
      if (!empty) {
        // Minor optimization? Only add to map if needed.
        map += hash(tok.left) -> curr
      }
    }
    (empty, map.result())
  }

  def setBinPack(curr: ScalafmtConfig, callSite: Boolean): ScalafmtConfig =
    curr.copy(
      binPack = curr.binPack.copy(
        unsafeCallSite = callSite
      )
    )

  @tailrec
  private def isBasicLiteral(tree: Tree): Boolean =
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
      case _ if !init.binPack.literalsIncludeSimpleExpr => false
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
  private def isLiteral(tree: Tree): Boolean =
    isBasicLiteral(tree) ||
      init.binPack.literalsIncludeSimpleExpr && (tree match {
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

  def opensLiteralArgumentList(ft: FormatToken): Boolean =
    ft.meta.leftOwner match {
      case TreeOps.SplitCallIntoParts(_, eitherArgs) =>
        eitherArgs
          .fold(Some(_), TokenOps.findArgsFor(ft.left, _, matching))
          .exists { args =>
            args.length > init.binPack.literalsMinArgCount &&
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

}
