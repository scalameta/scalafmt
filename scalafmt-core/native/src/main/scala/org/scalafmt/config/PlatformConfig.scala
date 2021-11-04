package org.scalafmt.config

object PlatformConfig {
  def isNative = true
  implicit val parser =
    metaconfig.sconfig.sConfigMetaconfigParser
}
