package org.scalafmt.dynamic

import java.nio.file.Path

import org.scalafmt.dynamic.ScalafmtDynamicError._
import org.scalafmt.interfaces._

final case class ScalafmtProperties(
    reporter: ScalafmtReporter = ConsoleScalafmtReporter,
    repositories: Seq[String] = Nil,
    respectExcludeFilters: Boolean = true
) {

  def withReporter(value: ScalafmtReporter): ScalafmtProperties =
    copy(reporter = value)

  def withRespectProjectFilters(value: Boolean): ScalafmtProperties =
    copy(respectExcludeFilters = value)

  def withMavenRepositories(value: Seq[String]): ScalafmtProperties =
    copy(repositories = value)

  def reportError(file: Path, error: ScalafmtDynamicError): Unit =
    error match {
      case e: ConfigMissingVersion =>
        reporter.missingVersion(e.configPath, BuildInfo.stable)
      case e: ConfigError =>
        Option(e.getCause) match {
          case Some(cause) => reporter.error(e.configPath, e.getMessage, cause)
          case None => reporter.error(e.configPath, e.getMessage)
        }
      case UnknownError(cause) =>
        reporter.error(file, cause)
    }

}
