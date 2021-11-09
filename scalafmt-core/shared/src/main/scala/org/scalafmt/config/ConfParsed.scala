package org.scalafmt.config

import java.nio.file.Path

import metaconfig._
import org.scalafmt.config.PlatformConfig._

class ConfParsed(val conf: Configured[Conf]) extends AnyVal

object ConfParsed {

  def apply(input: Configured[Input], path: Option[String] = None): ConfParsed =
    new ConfParsed(input.andThen(_.parse(path)))

  def fromInput(input: Input, path: Option[String] = None): ConfParsed =
    apply(Configured.Ok(input), path)

  def fromString(input: String, path: Option[String] = None): ConfParsed =
    fromInput(Input.String(input), path)

  def fromPath(input: Path, path: Option[String] = None): ConfParsed =
    apply(Configured.fromExceptionThrowing(Input.File(input)), path)

}
