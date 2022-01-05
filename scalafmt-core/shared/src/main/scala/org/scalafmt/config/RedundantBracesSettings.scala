package org.scalafmt.config

import metaconfig._
import metaconfig.annotation.ExtraName

case class RedundantBracesSettings(
    @ExtraName("methodBodies")
    defnBodies: Boolean = true,
    includeUnitMethods: Boolean = true,
    @ExtraName("maxLines")
    maxBreaks: Int = 100,
    stringInterpolation: Boolean = false,
    parensForOneLineApply: Boolean = true,
    generalExpressions: Boolean = true,
    ifElseExpressions: Boolean = false
)

object RedundantBracesSettings {
  implicit lazy val surface: generic.Surface[RedundantBracesSettings] =
    generic.deriveSurface
  implicit lazy val codec: ConfCodecEx[RedundantBracesSettings] =
    generic.deriveCodecEx(RedundantBracesSettings()).noTypos
}
