package org.scalafmt.config

import metaconfig._

case class DanglingParentheses(
    callSite: Boolean,
    defnSite: Boolean,
    exclude: List[DanglingParentheses.Exclude] = Nil
) {
  val decoder: ConfDecoder[DanglingParentheses] =
    generic.deriveDecoder(this).noTypos
}

object DanglingParentheses {

  private val shortcutTrue = DanglingParentheses(true, true)
  private val shortcutFalse = DanglingParentheses(false, false)

  val default = shortcutTrue

  implicit lazy val surface: generic.Surface[DanglingParentheses] =
    generic.deriveSurface

  implicit val decoder: ConfDecoder[DanglingParentheses] =
    ConfDecoder.instance[DanglingParentheses] {
      case Conf.Bool(true) => Configured.Ok(shortcutTrue)
      case Conf.Bool(false) => Configured.Ok(shortcutFalse)
      case els => default.decoder.read(els)
    }

  implicit val encoder: ConfEncoder[DanglingParentheses] =
    generic.deriveEncoder

  sealed abstract class Exclude

  object Exclude {
    case object `class` extends Exclude
    case object `trait` extends Exclude
    case object `def` extends Exclude

    implicit val reader: ConfCodec[Exclude] =
      ReaderUtil.oneOf[Exclude](`class`, `trait`, `def`)
  }

}
