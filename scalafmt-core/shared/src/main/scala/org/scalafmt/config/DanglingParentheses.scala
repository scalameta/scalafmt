package org.scalafmt.config

import scala.meta._

import DanglingParentheses.Exclude
import metaconfig._

case class DanglingParentheses(
    callSite: Boolean,
    private[config] val defnSite: Boolean,
    ctrlSite: Boolean = true,
    private[config] val tupleSite: Option[Boolean] = None,
    private val exclude: Option[List[Exclude]] = None,
) {
  @inline
  def tupleOrCallSite(isTuple: Boolean) =
    if (isTuple) tupleSite.getOrElse(callSite) else callSite

  @inline
  def atDefnSite(lpOwner: Tree): Boolean = defnSite && isExcluded(lpOwner)

  @inline
  def atVerticalMultilineSite(lpOwner: Tree): Boolean = defnSite &&
    isExcluded(lpOwner, Exclude.defaultVerticalMultiline)

  def isExcluded(lpOwner: Tree, altExcludes: List[Exclude] = Nil): Boolean = {
    val excludes = exclude.getOrElse(altExcludes)
    excludes.isEmpty || !lpOwner.parent.flatMap {
      case _: Ctor.Primary | _: Defn.Class => Some(Exclude.`class`)
      case _: Defn.Trait => Some(Exclude.`trait`)
      case _: Defn.Enum => Some(Exclude.`enum`)
      case t: Member.ParamClauseGroup => t.parent.collect {
          case _: Defn.ExtensionGroup => Exclude.`extension`
          case _: Decl.Def | _: Defn.Def | _: Defn.Macro => Exclude.`def`
          case _: Decl.Given | _: Defn.Given | _: Defn.GivenAlias =>
            Exclude.`given`
        }
      case _ => None
    }.exists(excludes.contains)
  }

}

object DanglingParentheses {

  private[config] val shortcutTrue = DanglingParentheses(true, true)
  private[config] val shortcutFalse = DanglingParentheses(false, false, false)

  val default = shortcutTrue

  implicit lazy val surface: generic.Surface[DanglingParentheses] =
    generic.deriveSurface

  implicit val encoder: ConfEncoder[DanglingParentheses] = generic.deriveEncoder

  implicit val decoder: ConfDecoderEx[DanglingParentheses] = Presets
    .mapDecoder(generic.deriveDecoderEx(default).noTypos, "danglingParentheses") {
      case Conf.Bool(true) => shortcutTrue
      case Conf.Bool(false) => shortcutFalse
    }

  sealed abstract class Exclude

  private object Exclude {
    case object `class` extends Exclude
    case object `trait` extends Exclude
    case object `enum` extends Exclude
    case object `extension` extends Exclude
    case object `def` extends Exclude
    case object `given` extends Exclude

    implicit val reader: ConfCodecEx[Exclude] = ReaderUtil
      .oneOf[Exclude](`class`, `trait`, `enum`, `extension`, `def`, `given`)

    val defaultVerticalMultiline = List(`class`, `trait`)
  }

}
