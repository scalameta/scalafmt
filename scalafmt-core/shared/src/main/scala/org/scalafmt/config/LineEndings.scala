package org.scalafmt.config

import metaconfig._

sealed abstract class LineEndings

object LineEndings {
  implicit val reader: ConfCodecEx[LineEndings] = ReaderUtil
    .oneOfCustom[LineEndings](unix, windows, preserve) {
      case Conf.Str("keep") => Configured.Ok(preserve)
    }
  case object unix extends LineEndings
  case object windows extends LineEndings
  case object preserve extends LineEndings
}
