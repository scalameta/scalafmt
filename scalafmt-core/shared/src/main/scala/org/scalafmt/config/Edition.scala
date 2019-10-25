package org.scalafmt.config

import metaconfig.{Conf, ConfDecoder, ConfEncoder, Configured}

sealed abstract class Edition(val order: Int, val name: String)

object Edition  {
  implicit val ordering: Ordering[Edition] = Ordering.by[Edition, Int](_.order)

  implicit val decoder: ConfDecoder[Edition] = ConfDecoder.instance {
    case Conf.Str("2019-10") => Configured.ok(Edition201910)
    case _ => Configured.ok(EditionLatest)
  }

  implicit val encoder: ConfEncoder[Edition] =
    ConfEncoder.instance(edition => Conf.Str(edition.name))
}

case object Edition201910 extends Edition(0, "2019-10")
case object EditionLatest extends Edition(Int.MaxValue, "latest")
