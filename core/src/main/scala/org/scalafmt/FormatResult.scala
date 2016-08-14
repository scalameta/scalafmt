package org.scalafmt

import org.scalafmt.Error.CantFormatFile
import org.scalafmt.internal.Split

sealed abstract class FormatResult {
  def get: String = this match {
    case FormatResult.Success(code) => code
    case FormatResult.Failure(e) => throw e
  }
}

object FormatResult {
  case class Success(formattedCode: String) extends FormatResult
  case class Failure(e: Throwable) extends FormatResult
}
