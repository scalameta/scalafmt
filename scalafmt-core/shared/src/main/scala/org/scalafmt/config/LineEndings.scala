package org.scalafmt.config

import metaconfig.Conf
import metaconfig.ConfCodec
import metaconfig.Configured

sealed abstract class LineEndings

object LineEndings {
  implicit val reader: ConfCodec[LineEndings] =
    ReaderUtil.oneOfCustom[LineEndings](unix, windows, preserve) {
      case Conf.Str("keep") => Configured.Ok(preserve)
    }
  case object unix extends LineEndings
  case object windows extends LineEndings
  case object preserve extends LineEndings
}
