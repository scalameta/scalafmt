package org.scalafmt.config

import metaconfig.ConfDecoder

sealed abstract class LineEndings

object LineEndings {
  implicit val reader: ConfDecoder[LineEndings] =
    ReaderUtil.oneOf[LineEndings](unix, windows, preserve)
  case object unix extends LineEndings
  case object windows extends LineEndings
  case object preserve extends LineEndings
}
