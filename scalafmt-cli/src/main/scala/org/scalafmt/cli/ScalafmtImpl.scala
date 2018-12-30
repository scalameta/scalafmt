package org.scalafmt.cli

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.attribute.FileTime
import java.time.Instant
import java.util.concurrent.TimeUnit
import metaconfig.Configured
import org.scalafmt.Formatted
import org.scalafmt.Scalafmt
import org.scalafmt.config.Config
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.interfaces
import org.scalafmt.util.OsSpecific

final case class ScalafmtImpl(
    configPath: Option[Path] = None,
    reporter: interfaces.ScalafmtReporter = ScalafmtReporterImpl,
    respectExcludeFilters: Boolean = true
) extends interfaces.Scalafmt {
  private val configFile = configPath.getOrElse {
    Files.createTempFile("scalafmt", ".scalafmt.conf")
  }
  private var lastTimestamp = FileTime.fromMillis(0)
  private var config = ScalafmtConfig.default
  override def format(file: Path, code: String): String = {
    val currentTimestamp = Files.getLastModifiedTime(configFile)
    val parsedConfig: Option[ScalafmtConfig] =
      if (currentTimestamp.compareTo(lastTimestamp) != 0) {
        val text =
          new String(Files.readAllBytes(configFile), StandardCharsets.UTF_8)
        Config.fromHoconString(text) match {
          case Configured.Ok(value) =>
            lastTimestamp = currentTimestamp
            config = value
            reporter.parsedConfig(configFile)
            Some(config)
          case Configured.NotOk(error) =>
            reporter.error(configFile, error.msg)
            None
        }
      } else {
        Some(config)
      }
    parsedConfig match {
      case None =>
        code
      case Some(baseConfig) =>
        val filename = OsSpecific.fixSeparatorsInPathPattern(file.toString)
        if (respectExcludeFilters &&
          !baseConfig.project.matcher.matches(filename)) {
          reporter.excluded(filename)
          code
        } else {
          val finalConfig =
            if (filename.endsWith(".sc") || filename.endsWith(".sbt")) {
              baseConfig.forSbt
            } else {
              baseConfig
            }
          val result = Scalafmt.format(code, finalConfig, Set.empty, filename)
          result match {
            case Formatted.Success(formattedCode) =>
              formattedCode
            case Formatted.Failure(e) =>
              reporter.error(e)
              code
          }
        }
    }
  }
}
