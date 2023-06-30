package org.scalafmt.config

import metaconfig.MetaconfigParser

object PlatformConfig {
  implicit val parser: MetaconfigParser =
    metaconfig.typesafeconfig.typesafeConfigMetaconfigParser
}
