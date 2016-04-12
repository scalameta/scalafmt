package org.scalafmt.util

import scala.meta.parsers.ParseException

sealed abstract class ExperimentResult(scalaFile: ScalaFile) {
  def key: String

  override def toString: String =
    s"""${this.getClass.getSimpleName}($scalaFile)""".stripMargin
}

object ExperimentResult {

  case class Success(scalaFile: ScalaFile, nanos: Long)
      extends ExperimentResult(scalaFile) {

    override def key = "Success"
  }

  case class Timeout(scalaFile: ScalaFile)
      extends ExperimentResult(scalaFile) {

    override def key = "Formatter timed out"
  }

  case class Skipped(scalaFile: ScalaFile)
      extends ExperimentResult(scalaFile) {

    override def key = "Ignored, scalac won't parse"
  }

  case class UnknownFailure(scalaFile: ScalaFile, e: Throwable)
      extends ExperimentResult(scalaFile) {

    override def key: String = e.getClass.getName

    override def toString: String = s"$scalaFile $e"
  }

  case class ParseErr(scalaFile: ScalaFile, e: ParseException)
      extends ExperimentResult(scalaFile) {

    override def key: String =
      e.getClass.getName + ": " + e.getMessage.replaceAll(" at .*", "")

    def lineNumber = e.pos.point.line

    def content = s"cols:${e.pos.start.column}-${e.pos.end.column}"

    def urlWithLineHighlighted: String =
      s"${scalaFile.githubUrl}#L${e.pos.start.line + 1} $cols"

    def cols = s"cols:${e.pos.start.column}-${e.pos.end.column}"
  }
}
