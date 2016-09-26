package org.scalafmt.config

import metaconfig.ConfigReader

@ConfigReader
case class ContinuationIndent(
    callSite: Int = 2,
    defnSite: Int = 4
)
