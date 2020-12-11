package org.scalafmt.config

import metaconfig._

case class DanglingParentheses(
    callSite: Boolean,
    defnSite: Boolean,
    ctrlSite: Boolean = true,
    exclude: List[DanglingParentheses.Exclude] = Nil
) extends Decodable[DanglingParentheses] {
  override protected[config] def baseDecoder =
    generic.deriveDecoder(this).noTypos
}

object DanglingParentheses {

  private val shortcutTrue = DanglingParentheses(true, true)
  private val shortcutFalse = DanglingParentheses(false, false, false)

  val default = shortcutTrue

  implicit lazy val surface: generic.Surface[DanglingParentheses] =
    generic.deriveSurface

  implicit val encoder: ConfEncoder[DanglingParentheses] =
    generic.deriveEncoder

  implicit val preset: PartialFunction[Conf, DanglingParentheses] = {
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

    implicit val reader: ConfCodec[Exclude] =
      ReaderUtil.oneOf[Exclude](`class`, `trait`, `enum`, `extension`, `def`)
  }

}
