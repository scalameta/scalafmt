package org.scalafmt.dynamic

import java.net.URL
import java.nio.file.Path

import org.scalafmt.dynamic.exceptions.ScalafmtConfigException

sealed trait ScalafmtDynamicError

object ScalafmtDynamicError {
  case class ConfigDoesNotExist(configPath: Path) extends ScalafmtDynamicError

  case class ConfigMissingVersion(configPath: Path) extends ScalafmtDynamicError

  case class ConfigParseError(configPath: Path, cause: ScalafmtConfigException)
      extends ScalafmtDynamicError

  case class CannotDownload(
      configPath: Path,
      version: String,
      cause: Option[Throwable]
  ) extends ScalafmtDynamicError

  case class CorruptedClassPath(
      configPath: Path,
      version: String,
      urls: Seq[URL],
      cause: Throwable
  ) extends ScalafmtDynamicError

  case class UnknownError(cause: Throwable) extends ScalafmtDynamicError
}
