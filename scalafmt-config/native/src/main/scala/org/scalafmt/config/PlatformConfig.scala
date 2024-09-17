package org.scalafmt.config

import metaconfig.MetaconfigParser

object PlatformConfig {
  def isNative = true
  implicit val parser: MetaconfigParser =
    metaconfig.sconfig.sConfigMetaconfigParser
}
