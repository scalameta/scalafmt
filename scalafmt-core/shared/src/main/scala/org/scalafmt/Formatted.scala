package org.scalafmt

import org.scalafmt.config.ScalafmtConfig

sealed abstract class Formatted {

  def toEither: Either[Throwable, String] =
    this match {
      case Formatted.Success(s) => Right(s)
      case Formatted.Failure(e) => Left(e)
    }

  def get: String =
    this match {
      case Formatted.Success(code) => code
      case Formatted.Failure(e) => throw e
    }
}

object Formatted {
  case class Success(formattedCode: String) extends Formatted
  case class Failure(e: Throwable) extends Formatted

  private[scalafmt] case class Result(
      formatted: Formatted,
      config: ScalafmtConfig
  ) {
    def get: String = formatted.get
  }

}
