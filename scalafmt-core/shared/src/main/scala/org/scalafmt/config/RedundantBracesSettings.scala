package org.scalafmt.config

import metaconfig._

@DeriveConfDecoder
case class RedundantBracesSettings(
    includeUnitMethods: Boolean = true,
    maxLines: Int = 100,
    stringInterpolation: Boolean = false,
    ifElseClauses: Boolean = true,
    caseClauses: Boolean = true
)
