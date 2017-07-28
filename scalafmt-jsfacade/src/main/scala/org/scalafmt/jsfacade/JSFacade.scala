package org.scalafmt
package jsfacade

import scala.util.Try

import scala.scalajs.js
import js.JSConverters._
import js.annotation._

import config.{ScalafmtConfig, Config}

object JSFacade {

  private[this] type Ranges = js.Array[js.Dictionary[Int]]

  private[this] def toRanges(
      ranges: js.UndefOr[Ranges]): Either[String, Set[Range]] =
    Try {
      ranges.toOption
        .map(_.map(r => Range(r("start"), r("end"))).toSet)
        .getOrElse(Set.empty)
    }.toEither.left.map(_.toString)

  @JSExportTopLevel("format")
  def format(
      input: String,
      hoconConfig: js.UndefOr[String],
      ranges: js.UndefOr[Ranges]): String = {
    (for {
      config <- Config
        .fromHoconString(hoconConfig.getOrElse(""))
        .toEither
        .left
        .map(_.toString)
      rr <- toRanges(ranges)
      formattedInput <- Scalafmt
        .format(input, config, rr)
        .toEither
        .left
        .map(_.toString)
    } yield formattedInput).fold(
      errorMessage => throw js.JavaScriptException(errorMessage),
      formattedInput => formattedInput
    )
  }

}
