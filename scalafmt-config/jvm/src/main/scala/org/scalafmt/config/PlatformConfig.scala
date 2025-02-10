package org.scalafmt.config

import metaconfig._

object PlatformConfig {
  implicit val parser: MetaconfigParser =
    typesafeconfig.typesafeConfigMetaconfigParser
}
