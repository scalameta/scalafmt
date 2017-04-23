package org.scalafmt.config

object PlatformConfig {
  implicit val parser =
    metaconfig.typesafeconfig.typesafeConfigMetaconfigParser
}
