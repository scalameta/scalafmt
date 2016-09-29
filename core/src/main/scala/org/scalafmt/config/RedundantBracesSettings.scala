package org.scalafmt.config

import metaconfig.ConfigReader

@ConfigReader
case class RedundantBracesSettings(
    includeUnitMethods: Boolean = true,
    maxLines: Int = 100
)
