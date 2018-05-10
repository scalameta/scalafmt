package org.scalafmt.config

import metaconfig._

case class SortSettings(
    order: List[SortSettings.ModKey]
)

object SortSettings {

  implicit val SortSettingsModKeyReader: ConfCodec[ModKey] =
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

  val defaultOrder: List[ModKey] = List(
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

  implicit val surface: generic.Surface[SortSettings] = generic.deriveSurface
  implicit lazy val encoder: ConfEncoder[SortSettings] =
    generic.deriveEncoder
  implicit val reader: ConfDecoder[SortSettings] =
    generic.deriveDecoder(SortSettings(defaultOrder)).flatMap { result =>
      if (result.order.distinct.length != 8) {
        val diff = defaultOrder.diff(result.order.distinct)
        ConfError
          .message(
            s"Incomplete 'sortModifiers.order', missing values: ${diff.mkString(", ")}. " +
              s"If specified, it has to contain all of the following values in the order you wish them sorted:" +
              """["private", "protected" , "abstract", "final", "sealed", "implicit", "override", "lazy"]"""
          )
          .notOk
      } else {
        Configured.ok(result)
      }
    }

  def default: SortSettings =
    SortSettings(defaultOrder)

  sealed trait ModKey extends Product

  case object `private` extends ModKey
  case object `protected` extends ModKey
  case object `final` extends ModKey
  case object `sealed` extends ModKey
  case object `abstract` extends ModKey
  case object `implicit` extends ModKey
  case object `override` extends ModKey
  case object `lazy` extends ModKey
}
