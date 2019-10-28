package org.scalafmt.config

import metaconfig.{Conf, ConfDecoder, ConfEncoder, Configured}

case class Edition(year: Int, month: Int)

object Edition {
  val Latest = Edition(Int.MaxValue, Int.MaxValue)
  implicit val ordering: Ordering[Edition] =
    Ordering.by[Edition, (Int, Int)](e => e.year -> e.month)
  lazy val format = "(\\d{4})-(\\d{1,2})".r

  implicit val decoder: ConfDecoder[Edition] =
    ConfDecoder.instanceExpect("'$year-$month', for example '2019-08'") {
      case Conf.Str(format(year, month)) =>
        Configured.ok(Edition(year.toInt, month.toInt))
      case Conf.Str("latest") => Configured.ok(Latest)
    }

  implicit val encoder: ConfEncoder[Edition] =
    ConfEncoder.instance(
      edition => Conf.Str(f"${edition.year}%d-${edition.month}%02d")
    )
}
