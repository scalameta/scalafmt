package org.scalafmt.config

import metaconfig._

case class RedundantParensSettings(
)

object RedundantParensSettings {
  val default = RedundantParensSettings()
  implicit lazy val surface: generic.Surface[RedundantParensSettings] =
    generic.deriveSurface
  implicit lazy val codec: ConfCodecEx[RedundantParensSettings] =
    generic.deriveCodecEx(default).noTypos
}
