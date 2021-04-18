package org.scalafmt.config

import metaconfig._

sealed abstract class SpaceBeforeContextBound

object SpaceBeforeContextBound {

  implicit val codec: ConfCodec[SpaceBeforeContextBound] = ReaderUtil
    .oneOfCustom[SpaceBeforeContextBound](Always, Never, IfMultipleBounds) {
      case Conf.Bool(true) => Configured.ok(Always)
      case Conf.Bool(false) => Configured.ok(Never)
    }

  case object Always extends SpaceBeforeContextBound
  case object Never extends SpaceBeforeContextBound
  case object IfMultipleBounds extends SpaceBeforeContextBound
}
