package org.scalafmt.config

import metaconfig.Configured.Ok
import metaconfig.{Conf, ConfDecoder}
import org.scalafmt.config.SpaceBeforeContextBound.{Always, IfMultipleBounds}

object SpaceBeforeContextBound {

  implicit val reader = ConfDecoder.instance[SpaceBeforeContextBound] {
    case Conf.Bool(true)  => Ok(Always)
    case Conf.Bool(false) => Ok(Never)
    case x =>
      ReaderUtil
        .oneOf[SpaceBeforeContextBound](Always, Never, IfMultipleBounds)
        .read(x)
  }

  case object Always extends SpaceBeforeContextBound
  case object Never extends SpaceBeforeContextBound
  case object IfMultipleBounds extends SpaceBeforeContextBound
}
sealed trait SpaceBeforeContextBound {
  def isIfMultipleBounds = this == IfMultipleBounds
  def isAlways = this == Always
}
