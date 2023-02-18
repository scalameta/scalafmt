package org.scalafmt.dynamic

import scala.meta._

object PositionSyntax {

  def formatMessage(pos: Position, severity: String, message: String): String =
    pos match {
      case Position.None =>
        s"$severity: $message"
      case _ =>
        new java.lang.StringBuilder()
          .append(pos.lineInput)
          .append(if (severity.isEmpty) "" else " ")
          .append(severity)
          .append(
            if (message.isEmpty) ""
            else if (severity.isEmpty) " "
            else if (message.startsWith("\n")) ":"
            else ": "
          )
          .append(message)
          .append(pos.rangeText)
          .toString
    }

  implicit class XtensionPositionsScalafix(private val pos: Position)
      extends AnyVal {

    def contains(other: Position): Boolean = {
      pos.start <= other.start &&
      pos.end >= other.end
    }

    def formatMessage(severity: String, message: String): String =
      PositionSyntax.formatMessage(pos, severity, message)

    /** Returns a formatted string of this position including
      * filename/line/caret.
      */
    def lineInput: String =
      s"${pos.input.syntax}:${pos.startLine + 1}:${pos.startColumn + 1}:"

    def rangeNumber: String =
      s"${pos.startLine + 1}:${pos.startColumn + 1}..${pos.endLine + 1}:${pos.endColumn + 1}"

    def rangeText: String =
      pos match {
        case Position.None => ""
        case _ =>
          if (pos.startLine != pos.endLine) multilines
          else lineTextAndCaret
      }
    def lineTextAndCaret: String = {
      new StringBuilder()
        .append("\n")
        .append(lineContent)
        .append("\n")
        .append(pos.lineCaret)
        .toString()
    }
    def multilines: String = {
      var i = pos.startLine
      val sb = new StringBuilder()
      while (i <= pos.endLine) {
        val startColumn =
          if (i == pos.startLine) pos.startColumn
          else 0
        val endColumn =
          if (i == pos.endLine) pos.endColumn
          else Int.MaxValue
        sb.append("\n> ")
          .append(
            lineContent(
              i,
              startColumn = startColumn,
              endColumn = endColumn
            ).text
          )
        i += 1
      }
      sb.toString()
    }
    def lineCaret: String =
      pos match {
        case Position.None =>
          ""
        case _ =>
          val span = pos.end - pos.start
          val caret =
            if (span != 0 && pos.startLine == pos.endLine) "^" * span else "^"
          (" " * pos.startColumn) + caret
      }

    private def lineContent(
        line: Int,
        startColumn: Int = 0,
        endColumn: Int = Int.MaxValue
    ): Position =
      Position.Range(
        pos.input,
        startLine = line,
        startColumn = startColumn,
        endLine = line,
        endColumn = endColumn
      )

    private def lineContent: String =
      pos match {
        case Position.None => ""
        case range: Position.Range =>
          lineContent(range.startLine).text
      }
  }

}
