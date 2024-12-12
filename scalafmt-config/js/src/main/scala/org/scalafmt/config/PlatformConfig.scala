package org.scalafmt.config

import java.nio.file.Path

import metaconfig._

object PlatformConfig {
  val isScalaNative = false
  implicit val parser = hocon.hoconMetaconfigParser
  def metaconfigInputFromFile(input: Path) = Input.File(input)
}
