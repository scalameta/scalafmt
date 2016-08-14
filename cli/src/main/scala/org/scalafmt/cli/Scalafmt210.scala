package org.scalafmt.cli

import java.io.File

import org.scalafmt.Error.SearchStateExploded
import org.scalafmt.util.LoggerOps._
import org.scalafmt.Error.InvalidScalafmtConfiguration
import org.scalafmt.FormatResult
import org.scalafmt.Scalafmt
import org.scalafmt.ScalafmtStyle

/**
  * Classload ScalaFmt210 to run ScalaFmt from Scala 2.10, for example sbt
  * plugin.
  */
class Scalafmt210 {

  def format(code: String, configFile: String, filename: String): String = {
    val style = StyleCache
      .getStyleForFile(configFile)
      .getOrElse(
        throw InvalidScalafmtConfiguration(new File(configFile))
      )
    format(code, style, filename)
  }

  def format(code: String, filename: String): String =
    format(code, ScalafmtStyle.default, filename)

  private def format(code: String,
                     scalafmtStyle: ScalafmtStyle,
                     filename: String): String = {
    val currentPath = new File("").getAbsolutePath + "/"
    val relativePath = filename.stripPrefix(currentPath)
    Scalafmt.format(code, style = scalafmtStyle) match {
      case FormatResult.Success(formattedCode) => formattedCode
      case error =>
        error match {
          case FormatResult.Failure(e) =>
            logger.warn(
              s"Failed to format file. Cause: ${e.getMessage}. Path: $relativePath")
          case _ =>
        }
        code
    }
  }
}
