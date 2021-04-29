package org.scalafmt.config

import metaconfig._

case class RedundantBracesSettings(
    methodBodies: Boolean = true,
    includeUnitMethods: Boolean = true,
    maxLines: Int = 100,
    stringInterpolation: Boolean = false,
    parensForOneLineApply: Option[Boolean] = None,
    generalExpressions: Boolean = true,
    ifElseExpressions: Boolean = false
) {
  val reader: ConfDecoder[RedundantBracesSettings] =
    generic.deriveDecoder(this).noTypos
}

object RedundantBracesSettings {
  implicit lazy val surface: generic.Surface[RedundantBracesSettings] =
    generic.deriveSurface
  implicit lazy val encoder: ConfEncoder[RedundantBracesSettings] =
    generic.deriveEncoder
}
