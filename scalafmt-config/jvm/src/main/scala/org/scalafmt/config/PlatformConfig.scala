package org.scalafmt.config

import java.nio.file.Path

import metaconfig.Input
import metaconfig.MetaconfigParser

object PlatformConfig {
  val isScalaNative = false
  implicit val parser: MetaconfigParser =
    metaconfig.typesafeconfig.typesafeConfigMetaconfigParser
  def metaconfigInputFromFile(input: Path) = Input.File(input)
}
