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
import org.scalameta.FileLine
import org.scalameta.logger

class StyleMap(
    tokens: FormatTokens,
    val init: ScalafmtConfig
) {
  import StyleMap._
  val literalR: FilterMatcher = init.binPack.literalsRegex
  private val prefix = "\\s*scalafmt: ".r
  val forcedBinPack: mutable.Set[Tree] = mutable.Set.empty
  private val (
    starts: Array[Int],
    styles: Array[ScalafmtConfig]
  ) = {
    var curr = init
    val startBuilder = Array.newBuilder[Int]
    val styleBuilder = Array.newBuilder[ScalafmtConfig]
    startBuilder += 0
    styleBuilder += init
    val disableBinPack = mutable.Set.empty[Token]
    def warn(err: String)(implicit fileLine: FileLine): Unit = logger.elem(err)
    tokens.arr.foreach { tok =>
      def changeStyle(style: ScalafmtConfig): Boolean = {
        val changing = curr != style
        if (changing) {
          startBuilder += tok.left.start
          styleBuilder += style
          curr = style
        }
        changing
      }
      tok.left match {
        case Comment(c) if prefix.findFirstIn(c).isDefined =>
          Config.fromHoconString(c, Some("scalafmt"), init) match {
            case Configured.Ok(style) =>
              if (init.rewrite.rulesChanged(style.rewrite))
                warn("May not override rewrite settings")
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
    }
    (startBuilder.result(), styleBuilder.result())
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
  def forall(f: ScalafmtConfig => Boolean): Boolean = styles.forall(f)

  def at(token: Token): ScalafmtConfig = {
    // since init is at pos 0, idx cannot be -1
    val idx = java.util.Arrays.binarySearch(starts, token.start)
    if (idx >= 0) styles(idx) else styles(-idx - 2)
  }

  private[util] def numEntries: Int = styles.length

}

object StyleMap {

  def setBinPack(curr: ScalafmtConfig, callSite: Boolean): ScalafmtConfig =
    if (curr.binPack.unsafeCallSite == callSite) curr
    else curr.copy(binPack = curr.binPack.copy(unsafeCallSite = callSite))

}
