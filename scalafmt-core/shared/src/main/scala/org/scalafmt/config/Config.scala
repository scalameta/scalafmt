package org.scalafmt.config

import java.io.File

import metaconfig._
import org.scalafmt.config.PlatformConfig._
import org.scalafmt.Versions.{stable => stableVersion}
import org.scalafmt.internal.PlatformCompat

// NOTE: these methods are intended for internal usage and are subject to
// binary and source breaking changes between any release. For a stable API
// use org.scalafmt.Scalafmt. Documentation on using scalafmt as a library
// can be seen here https://scalameta.org/scalafmt/#Standalonelibrary
object Config {

  def hoconStringToConf(input: String, path: Option[String]): Configured[Conf] =
    Input.String(input).parse(path)

  def hoconFileToConf(input: File, path: Option[String]): Configured[Conf] = {
    Configured
      .fromExceptionThrowing(
        PlatformCompat.metaconfigInputFromFile(input)
      )
      .andThen(_.parse(path))
  }

  def fromHoconString(
      string: String,
      default: ScalafmtConfig = ScalafmtConfig.default,
      path: Option[String] = None
  ): Configured[ScalafmtConfig] =
    fromConf(hoconStringToConf(string, path), default = default)

  /** Read ScalafmtConfig from String contents from an optional HOCON path. */
  def fromHoconFile(
      file: File,
      default: ScalafmtConfig = ScalafmtConfig.default,
      path: Option[String] = None
  ): Configured[ScalafmtConfig] =
    fromConf(hoconFileToConf(file, path), default = default)

  def fromConf(
      conf: Configured[Conf],
      default: ScalafmtConfig
  ): Configured[ScalafmtConfig] = {
    ScalafmtConfig.decoder.read(Option(default), conf) match {
      case Configured.Ok(x)
          if default.eq(ScalafmtConfig.uncheckedDefault) &&
            x.version != stableVersion =>
        val version = Option(x.version).getOrElse("missing")
        Configured.error(s"version [expected $stableVersion]: $version")
      case Configured.Ok(x)
          if default.eq(ScalafmtConfig.uncheckedDefault) &&
            x.runner.isDefaultDialect =>
        Configured.error(ScalafmtRunner.Dialect.getUnknownError)
      case x => x
    }
  }

}
