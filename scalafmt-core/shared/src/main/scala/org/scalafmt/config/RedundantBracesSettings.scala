package org.scalafmt.config

import metaconfig._

/**
  * @param methodBodies if true, removes redundant braces around method bodies
  * @param includeUnitMethods if true, also removes redundant braces around method bodies
  *                           with unit return types
  * @param maxLines the maximum block size to process (anything larger will be ignored)
  * @param stringInterpolation if true, removes redundant braces around values interpolated
  *                            in strings, as in s"Hello, ${name}" => s"Hello $name"
  * @param generalExpressions if true, removes redundant braces around basically any
  *                           expression (danger, possibly superfluous)
  * @param alwaysAroundMultilineLambda if true, will *add* redundant braces around
  *                                    any muliti-line lambda body, regardless of
  *                                    whether it is a single statement
  */
case class RedundantBracesSettings(
    methodBodies: Boolean = true,
    includeUnitMethods: Boolean = true,
    maxLines: Int = 100,
    stringInterpolation: Boolean = false,
    // Re-enable generalExpressions once
    // https://github.com/scalameta/scalafmt/issues/1147 is fixed
    generalExpressions: Boolean = false,
    alwaysAroundMultilineLambda: Boolean = false
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
