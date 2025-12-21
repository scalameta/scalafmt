package org.scalafmt.config

import metaconfig._

sealed abstract class LineEndings

object LineEndings {
  implicit val reader: ConfCodecEx[LineEndings] = ReaderUtil
    .oneOfCustom[LineEndings](unix, windows, keep) {
      case Conf.Str(str) if str.equalsIgnoreCase("preserve") =>
        Configured.Ok(keep)
    }
  case object unix extends LineEndings
  case object windows extends LineEndings
  case object keep extends LineEndings

  def eol(isWin: Boolean): String = if (isWin) "\r\n" else "\n"
}
