package org.scalafmt.config

import metaconfig.MetaconfigParser

object PlatformConfig {
  def isNative = false
  implicit val parser: MetaconfigParser =
    metaconfig.typesafeconfig.typesafeConfigMetaconfigParser
}
