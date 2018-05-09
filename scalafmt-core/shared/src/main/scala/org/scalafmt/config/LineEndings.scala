package org.scalafmt.config

import metaconfig.ConfCodec

sealed abstract class LineEndings

object LineEndings {
  implicit val reader: ConfCodec[LineEndings] =
    ReaderUtil.oneOf[LineEndings](unix, windows, preserve)
  case object unix extends LineEndings
  case object windows extends LineEndings
  case object preserve extends LineEndings
}
