package org.scalafmt.config

import metaconfig._

case class DanglingParentheses(
    callSite: Boolean,
    defnSite: Boolean,
    ctrlSite: Boolean = true,
    private[config] val tupleSite: Option[Boolean] = None,
    private val exclude: Option[List[DanglingParentheses.Exclude]] = None
) {
  @inline
  def tupleOrCallSite(isTuple: Boolean) =
    if (isTuple) tupleSite.getOrElse(callSite) else callSite

  def getExclude(
      isVerticalMultiline: Boolean
  ): Seq[DanglingParentheses.Exclude] = exclude.getOrElse {
    if (!isVerticalMultiline) Nil
    else DanglingParentheses.Exclude.defaultVerticalMultiline
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

  object Exclude {
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
