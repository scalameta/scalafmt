package org.scalafmt.config

import metaconfig._

case class DanglingParentheses(
    callSite: Boolean,
    defnSite: Boolean,
    ctrlSite: Boolean = true,
    private[config] val tupleSite: Option[Boolean] = None,
    exclude: List[DanglingParentheses.Exclude] = Nil
) {
  def getTupleSite = tupleSite.getOrElse(callSite)
}

object DanglingParentheses {

  private[config] val shortcutTrue = DanglingParentheses(true, true)
  private[config] val shortcutFalse = DanglingParentheses(false, false, false)

  val default = shortcutTrue

  implicit lazy val surface: generic.Surface[DanglingParentheses] =
    generic.deriveSurface

  implicit val encoder: ConfEncoder[DanglingParentheses] =
    generic.deriveEncoder

  implicit val decoder: ConfDecoderEx[DanglingParentheses] = Presets.mapDecoder(
    generic.deriveDecoderEx(default).noTypos,
    "danglingParentheses"
  ) {
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

    implicit val reader: ConfCodecEx[Exclude] =
      ReaderUtil
        .oneOf[Exclude](`class`, `trait`, `enum`, `extension`, `def`, `given`)
  }

}
