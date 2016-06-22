package org.scalafmt.cli

import java.io.File

import org.scalafmt.Error.InvalidScalafmtConfiguration
import org.scalafmt.FormatResult
import org.scalafmt.Scalafmt
import org.scalafmt.ScalafmtStyle

/**
  * Classload ScalaFmt210 to run ScalaFmt from Scala 2.10, for example sbt
  * plugin.
  */
class Scalafmt210 {

  def format(code: String, configFile: String): String = {
    val style = StyleCache
      .getStyleForFile(configFile)
      .getOrElse(
          throw InvalidScalafmtConfiguration(new File(configFile))
      )
    format(code, style)
  }

  def format(code: String): String = format(code, ScalafmtStyle.default)

  private def format(code: String, scalafmtStyle: ScalafmtStyle): String = {
    Scalafmt.format(code, style = scalafmtStyle) match {
      case FormatResult.Success(formattedCode) => formattedCode
      case _ => code
    }
  }
}
