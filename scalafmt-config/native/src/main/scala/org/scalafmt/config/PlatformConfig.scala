package org.scalafmt.config

import java.nio.file.Files
import java.nio.file.Path

import metaconfig.Input
import metaconfig.MetaconfigParser

object PlatformConfig {
  val isScalaNative = true
  implicit val parser: MetaconfigParser =
    metaconfig.sconfig.sConfigMetaconfigParser
  def metaconfigInputFromFile(input: Path) = Input
    .String(new String(Files.readAllBytes(input)))
}
