package org.scalafmt.config

import metaconfig._

@DeriveConfDecoder
case class RedundantBracesSettings(
    methodBodies: Boolean = true,
    includeUnitMethods: Boolean = true,
    maxLines: Int = 100,
    stringInterpolation: Boolean = false,
    generalExpressions: Boolean = true)
