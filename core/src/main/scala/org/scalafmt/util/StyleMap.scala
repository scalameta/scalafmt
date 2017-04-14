package org.scalafmt.util

import scala.collection.mutable
import scala.meta.Lit
import scala.meta.Term
import scala.meta.Tree
import scala.meta.tokens.Token
import scala.meta.tokens.Token.Comment
import scala.meta.tokens.Token.LeftParen
import scala.meta.tokens.Token.RightParen
import scala.meta.tokens.Tokens

import metaconfig.Configured
import org.scalafmt.config.Config
import org.scalafmt.config.FilterMatcher
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.FormatToken
import org.scalafmt.util.TokenOps.TokenHash
import org.scalameta.logger

class StyleMap(tokens: Array[FormatToken],
               init: ScalafmtConfig,
               owners: Map[TokenHash, Tree],
               matching: Map[TokenHash, Token]) {
  import TokenOps.hash
  val literalR: FilterMatcher = init.binPack.literalsRegex
  private val prefix = "\\s*scalafmt: ".r
  val forcedBinPack: mutable.Set[Tree] = mutable.Set.empty
  val (isEmpty: Boolean, tok2style: Map[FormatToken, ScalafmtConfig]) = {
    var curr = init
    var empty = true
    val map = Map.newBuilder[FormatToken, ScalafmtConfig]
    val disableBinPack = mutable.Set.empty[Token]
    tokens.foreach { tok =>
      tok.left match {
        case Comment(c) if prefix.findFirstIn(c).isDefined =>
          Config.fromHoconString(c, Some("scalafmt")) match {
            case Configured.Ok(style) =>
              empty = false
              curr = style
            case Configured.NotOk(e) => // TODO(olafur) report error via callback
              logger.elem(e)
          }
        case open @ LeftParen()
            if init.binPack.literalArgumentLists &&
              isLiteralArgumentList(open) =>
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
        map += (tok -> curr)
      }
    }
    (empty, map.result())
  }

  def setBinPack(curr: ScalafmtConfig, callSite: Boolean): ScalafmtConfig =
    curr.copy(
      binPack = curr.binPack.copy(
        callSite = callSite
      )
    )

  private def isLiteral(tree: Tree): Boolean = tree match {
    case lit @ Lit(value: Any) =>
      val syntax = lit.tokens.mkString
      val strName =
        if (syntax.startsWith("0x")) "Byte"
        else value.getClass.getName
      literalR.matches(strName)
    case _ => false
  }

  private def isLiteralArgumentList(open: LeftParen): Boolean =
    owners(hash(open)) match {
      case t: Term.Apply =>
        t.args.length > init.binPack.literalsMinArgCount &&
          t.args.forall(isLiteral)
      case _ => false
    }

  def at(token: FormatToken): ScalafmtConfig = {
    tok2style.getOrElse(token, init)
  }

}
