package org.scalafmt

sealed abstract class FormatResult {
  def get: String = this match {
    case FormatResult.Success(code) => code
    case FormatResult.Failure(e) => throw e
  }
}

object FormatResult {
  case class Success(formattedCode: String) extends FormatResult
  // TODO(olafur) more precise info.
  case class Failure(e: Throwable) extends FormatResult
}
