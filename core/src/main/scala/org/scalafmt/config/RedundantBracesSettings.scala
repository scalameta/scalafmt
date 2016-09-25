package org.scalafmt.config

import metaconfig.ConfigReader

@ConfigReader
case class RedundantBracesSettings(
    includeInferredType: Boolean = true,
    includeUnitMethods: Boolean = true,
    maxLines: Int = 100
)
