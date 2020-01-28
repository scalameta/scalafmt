package org.scalafmt.cli

import scala.util.matching.Regex

import java.io.File

import metaconfig._, Configured._
import org.scalafmt.Error.InvalidScalafmtConfiguration
import org.scalafmt.Formatted
import org.scalafmt.Scalafmt
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.config.{ScalafmtRunner => SRunner}
import org.scalafmt.util.{FileOps, StyleCache}
import org.scalafmt.util.LoggerOps._

/**
  * Classload ScalaFmt210 to run ScalaFmt from Scala 2.10, for example sbt
  * plugin.
  */
class Scalafmt210 {
  val oldConfig: Regex = "--".r
  def main(args: Array[String]): Unit = Cli.main(args)

  // The rest is for scalafmtIncremental.
  def format(code: String, configFile: String, filename: String): String = {
    StyleCache.getStyleForFileOrError(configFile) match {
      case NotOk(throwable) => {
        if (oldConfig.findFirstIn(FileOps.readFile(configFile)).nonEmpty) {
          logger.debug(
            "You seem to use the <0.4 configuration, for instructions on how to migrate: https://olafurpg.github.io/scalafmt/#0.4.x"
          )
        }
        throw new IllegalArgumentException(throwable.toString())
      }
      case Ok(style) =>
        format(code, style, filename)
    }
  }

  def format(code: String, filename: String): String =
    format(code, ScalafmtConfig.default, filename)

  private def format(
      code: String,
      scalafmtStyle: ScalafmtConfig,
      filename: String
  ): String = {
    val currentPath = new File("").getAbsolutePath + "/"
    val relativePath = filename.stripPrefix(currentPath)
    val runner = // DRY please, same login in CLI
      if (filename.endsWith(".sbt")) SRunner.sbt
      else SRunner.default
    val style = scalafmtStyle.copy(runner = runner)
    Scalafmt.format(code, style) match {
      case Formatted.Success(formattedCode) => formattedCode
      case error =>
        error match {
          case Formatted.Failure(e) =>
            logger.debug(
              s"Failed to format file $relativePath. Cause: ${e.getMessage}."
            )
          case _ =>
        }
        code
    }
  }
}
