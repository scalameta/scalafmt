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
  val default = DanglingParentheses(true, true)
  implicit lazy val surface: generic.Surface[DanglingParentheses] =
    generic.deriveSurface

  implicit val decoder: ConfDecoder[DanglingParentheses] =
    ConfDecoder.instance[DanglingParentheses] {
      case Conf.Bool(true) => Configured.Ok(DanglingParentheses(true, true))
      case Conf.Bool(false) => Configured.Ok(DanglingParentheses(false, false))
      case els => default.decoder.read(els)
    }

  implicit val encoder: ConfEncoder[DanglingParentheses] =
    ConfEncoder.instance[DanglingParentheses] {
      case DanglingParentheses(true, true) => Conf.Bool(true)
      case DanglingParentheses(true, false) =>
        Conf.Obj("callSite" -> Conf.Bool(true), "defnSite" -> Conf.Bool(false))
      case DanglingParentheses(false, true) =>
        Conf.Obj("callSite" -> Conf.Bool(false), "defnSite" -> Conf.Bool(true))
      case DanglingParentheses(false, false) => Conf.Bool(false)

    }
}
