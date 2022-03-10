package org.scalafmt.config

import java.nio.file.Path

import metaconfig._
import org.scalafmt.Versions

// NOTE: these methods are intended for internal usage and are subject to
// binary and source breaking changes between any release. For a stable API
// use org.scalafmt.Scalafmt. Documentation on using scalafmt as a library
// can be seen here https://scalameta.org/scalafmt/#Standalonelibrary
object Config {

  def fromHoconString(
      string: String,
      default: ScalafmtConfig = ScalafmtConfig.default,
      path: Option[String] = None
  ): Configured[ScalafmtConfig] =
    fromConf(ConfParsed.fromString(string, path), default = default)

  /** Read ScalafmtConfig from String contents from an optional HOCON path. */
  def fromHoconFile(
      file: Path,
      default: ScalafmtConfig = ScalafmtConfig.default,
      path: Option[String] = None
  ): Configured[ScalafmtConfig] =
    fromConf(ConfParsed.fromPath(file, path), default = default)

  def fromConf(
      parsed: ConfParsed,
      default: ScalafmtConfig
  ): Configured[ScalafmtConfig] = {
    ScalafmtConfig.decoder.read(Option(default), parsed.conf) match {
      case Configured.Ok(x)
          if default.version == null &&
            x.version != Versions.stable && x.version != Versions.version =>
        val version = Option(x.version).getOrElse("missing")
        val expected = s"${Versions.stable} or ${Versions.version}"
        Configured.error(s"version [expected $expected]: $version")
      case Configured.Ok(x)
          if default.eq(ScalafmtConfig.uncheckedDefault) &&
            x.runner.isDefaultDialect =>
        Configured.error(NamedDialect.getUnknownError)
      case x => x
    }
  }

}
