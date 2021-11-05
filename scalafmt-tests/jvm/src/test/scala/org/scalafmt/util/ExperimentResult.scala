package org.scalafmt.util

import scala.meta.parsers.ParseException
import scala.meta.testkit._

import org.scalafmt.internal.State

sealed abstract class ExperimentResult(scalaFile: CorpusFile) {
  def key: String

  override def toString: String =
    s"""${this.getClass.getSimpleName}($scalaFile)""".stripMargin
}

object ExperimentResult {

  case class Success(scalaFile: CorpusFile, nanos: Long)
      extends ExperimentResult(scalaFile) {

    override def key = "Success"
  }

  case class Timeout(scalaFile: CorpusFile)
      extends ExperimentResult(scalaFile) {

    override def key = "Formatter timed out"
  }

  case class Skipped(scalaFile: CorpusFile)
      extends ExperimentResult(scalaFile) {

    override def key = "Ignored, scalac won't parse"
  }

  case class SearchStateExploded(scalaFile: CorpusFile, state: State)
      extends ExperimentResult(scalaFile) {

    override def key = s"Search state exploded"
  }

  case class UnknownFailure(scalaFile: CorpusFile, e: Throwable)
      extends ExperimentResult(scalaFile) {

    override def key: String = e.getClass.getName

    override def toString: String = s"$scalaFile $e"
  }

  case class ParseErr(scalaFile: CorpusFile, e: ParseException)
      extends ExperimentResult(scalaFile) {

    override def key: String =
      e.getClass.getName + ": " + e.getMessage.replaceAll(" at .*", "")

    def lineNumber = e.pos.startLine

    def content = s"cols:${e.pos.startColumn}-${e.pos.endColumn}"

    def urlWithLineHighlighted: String =
      s"${scalaFile.githubUrl}#L${e.pos.startLine + 1} $cols"

    def cols = s"cols:${e.pos.startColumn}-${e.pos.endColumn}"
  }
}
