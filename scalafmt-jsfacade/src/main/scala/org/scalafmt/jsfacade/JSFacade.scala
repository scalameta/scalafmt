package org.scalafmt
package jsfacade

import scala.scalajs.js
import js.JSConverters._
import js.annotation._

import config.{ScalafmtConfig, Config}

object JSFacade {

  @JSExportTopLevel("format")
  def format(input: String, hoconConfig: js.UndefOr[String]): String = {
    (for {
      config <- Config.fromHoconString(hoconConfig.getOrElse("")).toEither.left.map(_.toString)
      formattedInput <- Scalafmt.format(input, config).toEither.left.map(_.toString)
    } yield formattedInput).fold(
      errorMessage => throw js.JavaScriptException(errorMessage),
      formattedInput => formattedInput
    )
  }

}
