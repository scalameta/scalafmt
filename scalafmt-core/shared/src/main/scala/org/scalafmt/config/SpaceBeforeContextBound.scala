package org.scalafmt.config

import metaconfig._
import org.scalafmt.config.SpaceBeforeContextBound.{Always, IfMultipleBounds}

sealed trait SpaceBeforeContextBound {
  def isIfMultipleBounds = this == IfMultipleBounds
  def isAlways = this == Always
}

object SpaceBeforeContextBound {

  val codec: ConfCodec[SpaceBeforeContextBound] =
    ReaderUtil.oneOf[SpaceBeforeContextBound](Always, Never, IfMultipleBounds)

  implicit val encoder: ConfEncoder[SpaceBeforeContextBound] = codec
  implicit val reader: ConfDecoder[SpaceBeforeContextBound] =
    ConfDecoder.instance[SpaceBeforeContextBound] {
      case c => preset.lift(c).fold(codec.read(c))(Configured.ok)
    }

  implicit val preset: PartialFunction[Conf, SpaceBeforeContextBound] = {
    case Conf.Bool(true) => Always
    case Conf.Bool(false) => Never
  }

  case object Always extends SpaceBeforeContextBound
  case object Never extends SpaceBeforeContextBound
  case object IfMultipleBounds extends SpaceBeforeContextBound
}
