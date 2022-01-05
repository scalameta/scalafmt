package org.scalafmt.config

import metaconfig._

case class RedundantBracesSettings(
    methodBodies: Boolean = true,
    includeUnitMethods: Boolean = true,
    maxLines: Int = 100,
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
