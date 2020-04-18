package org.scalafmt.config

import metaconfig._
import org.scalafmt.config.SpaceBeforeContextBound.{Always, IfMultipleBounds}

sealed abstract class SpaceBeforeContextBound
    extends Decodable[SpaceBeforeContextBound] {
  override protected[config] def baseDecoder = SpaceBeforeContextBound.decoder

  def isIfMultipleBounds = this == IfMultipleBounds
  def isAlways = this == Always
}

object SpaceBeforeContextBound {

  val codec: ConfCodec[SpaceBeforeContextBound] =
    ReaderUtil.oneOf[SpaceBeforeContextBound](Always, Never, IfMultipleBounds)

  implicit val encoder: ConfEncoder[SpaceBeforeContextBound] = codec
  implicit val decoder: ConfDecoder[SpaceBeforeContextBound] = codec

  implicit val preset: PartialFunction[Conf, SpaceBeforeContextBound] = {
    case Conf.Bool(true) => Always
    case Conf.Bool(false) => Never
  }

  case object Always extends SpaceBeforeContextBound
  case object Never extends SpaceBeforeContextBound
  case object IfMultipleBounds extends SpaceBeforeContextBound
}
