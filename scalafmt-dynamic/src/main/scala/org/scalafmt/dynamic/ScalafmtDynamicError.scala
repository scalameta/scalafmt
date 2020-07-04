package org.scalafmt.dynamic

import java.net.URL
import java.nio.file.Path

sealed abstract class ScalafmtDynamicError(
    msg: String,
    cause: Throwable = null
) extends Error(msg, cause)

object ScalafmtDynamicError {
  sealed abstract class ConfigError(
      val configPath: Path,
      val msg: String,
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
      val version: String,
      cause: Throwable = null
  ) extends ConfigError(configPath, s"failed to download v=$version", cause)

  class CorruptedClassPath(
      configPath: Path,
      val version: String,
      val urls: Seq[URL],
      cause: Throwable
  ) extends ConfigError(configPath, s"corrupted class path v=$version", cause)

  case class UnknownError(cause: Throwable)
      extends ScalafmtDynamicError("unknown error", cause)
}
