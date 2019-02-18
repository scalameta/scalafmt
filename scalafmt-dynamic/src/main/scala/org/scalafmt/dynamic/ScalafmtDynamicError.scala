package org.scalafmt.dynamic

import java.nio.file.Path

import org.scalafmt.dynamic.exceptions.ScalafmtConfigException

sealed trait ScalafmtDynamicError

object ScalafmtDynamicError {
  case class ConfigDoesNotExist(configPath: Path) extends ScalafmtDynamicError
  case class ConfigMissingVersion(configPath: Path) extends ScalafmtDynamicError
  case class ConfigParseError(configPath: Path, cause: ScalafmtConfigException) extends ScalafmtDynamicError
  case class CannotDownload(configPath: Path, version: String, cause: Option[Throwable]) extends ScalafmtDynamicError
  case class UnknownError(message: String, cause: Option[Throwable]) extends ScalafmtDynamicError

  object UnknownError {
    def apply(cause: Throwable): UnknownError =
      new UnknownError(cause.getMessage, Option(cause))
  }
}
