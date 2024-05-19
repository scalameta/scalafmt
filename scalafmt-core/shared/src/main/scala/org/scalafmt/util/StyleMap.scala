package org.scalafmt.util

import org.scalafmt.config.BinPack
import org.scalafmt.config.FilterMatcher
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.FormatToken
import org.scalafmt.internal.FormatTokens

import org.scalameta.FileLine
import org.scalameta.logger
import scala.meta._
import scala.meta.tokens.Token

import scala.annotation.tailrec
import scala.collection.mutable

class StyleMap(tokens: FormatTokens, val init: ScalafmtConfig) {
  import StyleMap._
  val literalR: FilterMatcher = init.binPack.literalsRegex
  private val prefix = "\\s*scalafmt: ".r.pattern
  val forcedBinPack: mutable.Set[Tree] = mutable.Set.empty
  private val (starts: Array[Int], styles: Array[ScalafmtConfig]) = {
    var curr = init
    val startBuilder = Array.newBuilder[Int]
    val styleBuilder = Array.newBuilder[ScalafmtConfig]
    startBuilder += 0
    styleBuilder += init
    val disableBinPack = mutable.Map.empty[Token, BinPack.Site]
    def warn(err: String)(implicit fileLine: FileLine): Unit = logger.elem(err)
    tokens.arr.foreach { ft =>
      def changeStyle(style: ScalafmtConfig): Option[ScalafmtConfig] = {
        val changing = curr != style
        if (!changing) None
        else {
          startBuilder += ft.left.start
          styleBuilder += style
          val prev = curr
          curr = style
          Some(prev)
        }
      }
      ft.left match {
        case Token.Comment(c) if prefix.matcher(c).find() =>
          val configured = ScalafmtConfig
            .fromHoconString(c, init, Some("scalafmt"))
          // TODO(olafur) report error via callback
          configured.foreach(logger.elem(_)) { style =>
            init.rewrite.rulesChanged(style.rewrite).foreach { x =>
              val rules = x.mkString(",")
              warn(s"May not override rewrite settings; changed=[$rules]: $c")
            }
            changeStyle(style)
          }
        case tok: Token.LeftParen
            if curr.binPack.literalArgumentLists &&
              opensLiteralArgumentList(ft)(curr) =>
          forcedBinPack += ft.meta.leftOwner
          changeStyle(setBinPack(curr, callSite = BinPack.Site.Always))
            .foreach { x =>
              tokens.matchingOpt(tok)
                .foreach(disableBinPack.update(_, x.binPack.callSite))
            }
        case tok: Token.RightParen => disableBinPack.remove(tok)
            .foreach(x => changeStyle(setBinPack(curr, callSite = x)))
        case _ =>
      }
    }
    (startBuilder.result(), styleBuilder.result())
  }

  @tailrec
  private def isBasicLiteral(
      tree: Tree,
  )(implicit style: ScalafmtConfig): Boolean = tree match {
    case lit: Lit =>
      val strName = tree match {
        case t: Lit.Int
            if 0 <= t.value && t.value < Byte.MaxValue &&
              lit.tokens.head.toString.startsWith("0x") => "Byte"
        case _: Lit.Null => "Null"
        case _ => lit.value.getClass.getName
      }
      literalR.matches(strName)
    case x: Name => literalR.matches(x.productPrefix)
    case _ if !style.binPack.literalsIncludeSimpleExpr => false
    case t: Term.Select => isBasicLiteral(t.qual)
    case t: Term.Assign => isBasicLiteral(t.rhs)
    case _ => tree.children match {
        case Nil => true
        case one :: Nil => isBasicLiteral(one)
        case _ => false
      }
  }

  @tailrec
  private def isLiteral(tree: Tree)(implicit style: ScalafmtConfig): Boolean =
    isBasicLiteral(tree) ||
      style.binPack.literalsIncludeSimpleExpr &&
      (tree match {
        case t: Term.Assign => isLiteral(t.rhs)
        case t: Term.Apply => isBasicLiteral(t.fun) &&
          (t.argClause match {
            case Term.ArgClause(Nil, None) => true
            case Term.ArgClause(arg :: Nil, None) => isLiteral(arg)
            case _ => false
          })
        case Term.New(t) => isBasicLiteral(t.name) &&
          (t.argClauses match {
            case Nil | Term.ArgClause(Nil, None) :: Nil => true
            case Term.ArgClause(arg :: Nil, None) :: Nil => isLiteral(arg)
            case _ => false
          })
        case _ => tree.children match {
            case Nil => true
            case one :: Nil => isLiteral(one)
            case _ => false
          }
      })

  def opensLiteralArgumentList(
      ft: FormatToken,
  )(implicit style: ScalafmtConfig): Boolean = (ft.meta.leftOwner match {
    case Member.Tuple(v) => Some(v)
    case Member.SyntaxValuesClause(v) => Some(v)
    case _ => None
  }).exists { args =>
    args.lengthCompare(style.binPack.literalsMinArgCount) >= 0 &&
    args.forall(isLiteral)
  }

  @inline
  def at(token: FormatToken): ScalafmtConfig = at(token.left)

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

  def setBinPack(curr: ScalafmtConfig, callSite: BinPack.Site): ScalafmtConfig =
    if (curr.binPack.callSite == callSite) curr
    else curr.copy(binPack = curr.binPack.copy(callSite = callSite))

}
