package org.scalafmt.config

import java.nio.file._

import metaconfig._

object PlatformConfig {
  val isScalaNative = true
  implicit val parser: MetaconfigParser = sconfig.sConfigMetaconfigParser
  def metaconfigInputFromFile(input: Path) = Input
    .String(new String(Files.readAllBytes(input)))
}
