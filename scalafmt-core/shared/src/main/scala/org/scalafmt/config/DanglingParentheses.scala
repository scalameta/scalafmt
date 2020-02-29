package org.scalafmt.config

import metaconfig._

case class DanglingParentheses(
    callSite: Boolean,
    defnSite: Boolean
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

}
