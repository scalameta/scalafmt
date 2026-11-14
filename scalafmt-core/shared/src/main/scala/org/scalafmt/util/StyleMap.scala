package org.scalafmt.util

import org.scalafmt.config.{BinPack, FilterMatcher, ScalafmtConfig}
import org.scalafmt.internal._

import org.scalameta.{FileLine, logger}
import scala.meta._
import scala.meta.tokens.{Token => T}

import scala.annotation.tailrec
import scala.collection.mutable

/** @param initStyle
  *   the initial configuration, before `transform`
  * @param transform
  *   applies any custom transformations to a configuration
  */
class StyleMap(
    tokens: FormatTokens,
    initStyle: ScalafmtConfig,
    transform: ScalafmtConfig => ScalafmtConfig,
) {

  import StyleMap._
  private val prefix = "\\s*scalafmt: ".r.pattern
  val forcedBinPack: mutable.Set[Tree] = mutable.Set.empty
  private val (starts: Array[Int], styles: Array[ScalafmtConfig]) = {
    var init = initStyle
    var initMapped = transform(init)
    implicit var curr: ScalafmtConfig = initMapped
    implicit var literalR: FilterMatcher = initMapped.binPack.literalsRegex
    val startBuilder = Array.newBuilder[Int]
    val styleBuilder = Array.newBuilder[ScalafmtConfig]
    startBuilder += 0
    styleBuilder += curr
    val disableBinPack = mutable.Map.empty[Int, BinPack.Site]
    def warn(err: String)(implicit fileLine: FileLine): Unit = logger.elem(err)
    tokens.arr.foreach { ft =>
      def changeStyle(style: ScalafmtConfig): ScalafmtConfig =
        if (curr == style) null
        else {
          startBuilder += ft.left.start
          styleBuilder += style
          val prev = curr
          curr = style
          prev
        }
      ft.left match {
        case T.Comment(c) if prefix.matcher(c).find() =>
          val res = ScalafmtConfig.fromHoconString(c, init, Some("scalafmt"))
          res.foreach(logger.elem(_)) { style =>
            val mapped = transform(style)
            literalR = mapped.binPack.literalsRegex
            if ((init eq initStyle) && tokens.prevNonCommentBefore(ft).idx == 0) {
              init = style // right after BOF
              initMapped = mapped
              curr = mapped
            } else {
              initMapped.rewrite.rulesChanged(mapped.rewrite).foreach { x =>
                val rules = x.mkString(",")
                warn(s"May not override rewrite settings; changed=[$rules]: $c")
              }
              changeStyle(mapped)
            }
          }
        case _: T.LeftParen
            if curr.binPack.literalArgumentLists &&
              opensLiteralArgumentList(ft) =>
          forcedBinPack += ft.meta.leftOwner
          val prev = changeStyle(setBinPack(callSite = BinPack.Site.Always))
          if (null ne prev) {
            val y = tokens.matchingLeftOrNull(ft)
            if (y ne null) disableBinPack.update(y.idx, prev.binPack.callSite)
          }
        case _: T.RightParen => disableBinPack.remove(ft.idx)
            .foreach(x => changeStyle(setBinPack(callSite = x)))
        case _ =>
      }
    }
    val styles = styleBuilder.result()
    styles(0) = initMapped
    (startBuilder.result(), styles)
  }

  val init: ScalafmtConfig = styles(0)

  private def isBasicLiteral(
      tree: Tree,
  )(implicit literalR: FilterMatcher): Boolean = tree match {
    case lit: Lit =>
      val strName = tree match {
        case t: Lit.Int
            if 0 <= t.value && t.value < Byte.MaxValue &&
              TreeOps.headTokenOrNull(lit).toString.startsWith("0x") => "Byte"
        case _: Lit.Null => "Null"
        case _ => lit.value.getClass.getName
      }
      literalR.matches(strName)
    case x: Name => literalR.matches(x.productPrefix)
    case _ => false
  }

  @tailrec
  private def isSimpleLiteral(
      tree: Tree,
  )(implicit literalR: FilterMatcher): Boolean = tree match {
    case t: Term.SelectLike => isSimpleLiteral(t.qual)
    case t: Term.Assign => isSimpleLiteral(t.rhs)
    case _ => isBasicLiteral(tree) || {
        val child = getSingleChild(tree)
        (child eq Tree.NoTree) || (child ne null) && isSimpleLiteral(child)
      }
  }

  @tailrec
  private def isComplexLiteral(
      tree: Tree,
  )(implicit literalR: FilterMatcher): Boolean = tree match {
    case t: Term.Assign => isComplexLiteral(t.rhs)
    case t: Term.Apply => isSimpleLiteral(t.fun) &&
      (t.argClause match {
        case Term.ArgClause(Nil, None) => true
        case Term.ArgClause(arg :: Nil, None) => isComplexLiteral(arg)
        case _ => false
      })
    case Term.New(t) => isSimpleLiteral(t.name) &&
      (t.argClauses match {
        case Nil | Term.ArgClause(Nil, None) :: Nil => true
        case Term.ArgClause(arg :: Nil, None) :: Nil => isComplexLiteral(arg)
        case _ => false
      })
    case _ => isSimpleLiteral(tree) || {
        val child = getSingleChild(tree)
        (child eq Tree.NoTree) || (child ne null) && isComplexLiteral(child)
      }
  }

  private def opensLiteralArgumentList(ft: FT)(implicit
      style: ScalafmtConfig,
      literalR: FilterMatcher,
  ): Boolean = (ft.meta.leftOwner match {
    case Member.Tuple(v) => Some(v)
    case Member.SyntaxValuesClause(v) => Some(v)
    case _ => None
  }).exists { args =>
    implicit val pred: Tree => Boolean =
      if (style.binPack.literalsIncludeSimpleExpr) isComplexLiteral
      else isBasicLiteral
    @tailrec
    def iter(rest: List[Tree], cnt: Int)(implicit
        pred: Tree => Boolean,
    ): Boolean = rest match {
      case head :: tail => pred(head) && iter(tail, cnt + 1)
      case Nil => cnt >= style.binPack.literalsMinArgCount
    }
    iter(args, 0)
  }

  @inline
  def at(token: FT): ScalafmtConfig = at(token.left)

  @inline
  def forall(f: ScalafmtConfig => Boolean): Boolean = styles.forall(f)

  def at(token: T): ScalafmtConfig = {
    // since init is at pos 0, idx cannot be -1
    val idx = java.util.Arrays.binarySearch(starts, token.start)
    if (idx >= 0) styles(idx) else styles(-idx - 2)
  }

  def isForcedBinPack(ft: FT): Boolean = forcedBinPack.contains(ft.leftOwner)

  private[util] def numEntries: Int = styles.length

}

object StyleMap {

  def setBinPack(
      callSite: BinPack.Site,
  )(implicit curr: ScalafmtConfig): ScalafmtConfig =
    if (curr.binPack.callSite == callSite) curr
    else curr.copy(binPack = curr.binPack.copy(callSite = callSite))

  private def getSingleChild(tree: Tree): Tree = {
    var singleChild = Tree.NoTree
    tree.foreachChild { child =>
      if (singleChild ne null)
        singleChild = if (singleChild eq Tree.NoTree) child else null
    }
    singleChild
  }

}
