package org.scalafmt.config

import metaconfig.ConfigReader

@ConfigReader
case class OptIn(
    configStyleArguments: Boolean = true,
    breakChainOnFirstMethodDot: Boolean = false
)
