package org.scalafmt.config

import metaconfig._

case class SortSettings(
    order: List[SortSettings.ModKey]
)

object SortSettings {
  implicit val reader: ConfDecoder[SortSettings] = ConfDecoder.instance {
    case Conf.Lst(values) =>
      Configured.traverse(values.map(_.as[ModKey])).andThen { mods =>
        if (mods.distinct.length != 8) {
          val diff = defaultOrder.diff(mods.distinct)
          ConfError
            .message(
              s"'sortModifiers.order' if missing values ${diff.mkString(", ")}. " +
                s"If specified, it has to contain all of the following values in the order you wish them sorted:" +
                """["private", "protected" , "abstract", "final", "sealed", "implicit", "override", "lazy"]"""
            )
            .notOk
        } else {
          Configured.ok(SortSettings(mods))
        }
      }
  }

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
