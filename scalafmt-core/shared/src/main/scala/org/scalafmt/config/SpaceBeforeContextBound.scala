package org.scalafmt.config

import metaconfig.ConfCodec
import metaconfig.ConfEncoder
import metaconfig.Configured.Ok
import metaconfig.{Conf, ConfDecoder}
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
      case Conf.Bool(true) => Ok(Always)
      case Conf.Bool(false) => Ok(Never)
      case x => codec.read(x)
    }

  case object Always extends SpaceBeforeContextBound
  case object Never extends SpaceBeforeContextBound
  case object IfMultipleBounds extends SpaceBeforeContextBound
}
