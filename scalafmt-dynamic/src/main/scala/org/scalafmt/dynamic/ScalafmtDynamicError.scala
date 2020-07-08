package org.scalafmt.dynamic

import java.net.URL
import java.nio.file.Path

import scala.util.control.NoStackTrace

sealed abstract class ScalafmtDynamicError(
    msg: String,
    cause: Throwable = null
) extends Error(msg, cause)

object ScalafmtDynamicError {
  sealed abstract class ConfigError(
      val configPath: Path,
      msg: String,
      cause: Throwable = null
  ) extends ScalafmtDynamicError(msg, cause)

  class ConfigDoesNotExist(configPath: Path)
      extends ConfigError(configPath, "Missing config")

  class ConfigMissingVersion(configPath: Path)
      extends ConfigError(configPath, "Missing version")

  class ConfigParseError(configPath: Path, why: String)
      extends ConfigError(configPath, s"Invalid config: $why")

  class CannotDownload(
      configPath: Path,
      version: ScalafmtVersion,
      cause: Throwable = null
  ) extends ConfigError(configPath, s"failed to download v=$version", cause)

  class CorruptedClassPath(
      configPath: Path,
      version: ScalafmtVersion,
      val urls: Seq[URL],
      cause: Throwable
  ) extends ConfigError(configPath, s"corrupted class path v=$version", cause)

  class ConfigInvalidVersion(configPath: Path, version: String)
      extends ConfigError(configPath, s"Invalid version: $version")
      with NoStackTrace

  case class UnknownError(cause: Throwable)
      extends ScalafmtDynamicError("unknown error", cause)
}
