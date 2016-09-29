package org.scalafmt.config

sealed abstract class LineEndings

object LineEndings {
  val reader = ReaderUtil.oneOf[LineEndings](unix, windows, preserve)
  case object unix extends LineEndings
  case object windows extends LineEndings
  case object preserve extends LineEndings
}
