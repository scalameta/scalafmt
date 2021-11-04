package org.scalafmt.config

object PlatformConfig {
  def isNative = false
  implicit val parser =
    metaconfig.typesafeconfig.typesafeConfigMetaconfigParser
}
