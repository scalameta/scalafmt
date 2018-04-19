package org.scalafmt.config

import metaconfig._

@DeriveConfDecoder
case class SortSettings(
    order: Vector[SortSettings.ModKey]
)

object SortSettings {

  implicit val SortSettingsModKeyReader: ConfDecoder[ModKey] =
    ReaderUtil.oneOfIgnoreBackticks[ModKey](
      `implicit`,
      `final`,
      `sealed`,
      `abstract`,
      `override`,
      `private`,
      `protected`,
      `lazy`
    )

  val defaultOrder: Vector[ModKey] =
    Vector(
      `implicit`,
      //
      `final`,
      `sealed`,
      `abstract`,
      //
      `override`,
      //
      `private`,
      `protected`,
      //
      `lazy`
    )

  def default: SortSettings =
    SortSettings(defaultOrder)

  sealed trait ModKey

  case object `private` extends ModKey
  case object `protected` extends ModKey
  case object `final` extends ModKey
  case object `sealed` extends ModKey
  case object `abstract` extends ModKey
  case object `implicit` extends ModKey
  case object `override` extends ModKey
  case object `lazy` extends ModKey
}
