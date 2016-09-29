package org.scalafmt.config

import metaconfig.ConfigReader

@ConfigReader
case class BinPack(
    callSite: Boolean = false,
    defnSite: Boolean = false,
    parentConstructors: Boolean = false
)
