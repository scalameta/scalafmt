package org.scalafmt

import scala.meta.Case
import scala.meta.Tree
import scala.reflect.ClassTag
import scala.reflect.classTag
import java.io.File
import scala.meta.inputs.Position
import org.scalafmt.internal.Decision
import org.scalafmt.internal.FormatToken
import org.scalafmt.internal.State
import org.scalafmt.util.LoggerOps
import scala.meta.internal.inputs._
import scala.util.control.NoStackTrace

sealed abstract class Error(msg: String) extends Exception(msg)

object Error {
  import LoggerOps._

  def reportIssue: String =
    "Please file an issue on https://github.com/scalameta/scalafmt/issues"
  case object UnableToParseCliOptions
      extends Error("Failed to parse CLI options")

  case class Incomplete(formattedCode: String)
      extends Error("Unable to format file due to bug in scalafmt")

  case class PreciseIncomplete(pos: Position, formattedCode: String)
      extends Error(
        pos.formatMessage(
          "error",
          "Unable to format file due to bug in scalafmt"
        )
      )
  case class CantFindDefnToken(what: String, tree: Tree)
      extends Error(
        s"Expected keyword of type $what in tree $tree"
      )

  case class CaseMissingArrow(tree: Case)
      extends Error(s"Missing => in case: \n$tree")

  case class FormatterChangedAST(diff: String, output: String)
      extends Error(s"""Formatter changed AST
        |=====================
        |$diff
        |=====================
        |${output.linesIterator.toVector.take(10).mkString("\n")}
        |=====================
        |Formatter changed AST
      """.stripMargin)

  case class FormatterOutputDoesNotParse(msg: String, line: Int)
      extends Error("Formatter output does not parse:\n" + msg)

  case class UnexpectedTree[Expected <: Tree: ClassTag](obtained: Tree)
      extends Error(s"""Expected: ${classTag[Expected].runtimeClass.getClass}
        |Obtained: ${log(obtained)}""".stripMargin)

  case class CantFormatFile(msg: String)
      extends Error("scalafmt cannot format this file:\n" + msg)

  case class NoopDefaultPolicyApplied(decision: Decision)
      extends Error(s"Default policy run on $decision")

  case class UnknownStyle(style: String)
      extends Error(s"Don't understand style $style")

  case class UnableToFindStyle(filename: String, e: Throwable)
      extends Error(s"Unable to find style for file $filename. $e")

  case class MisformattedFile(file: File, customMessage: String)
      extends Error(s"${file.getPath} is mis-formatted. $customMessage")

  case class SearchStateExploded(
      deepestState: State,
      partialOutput: String,
      ft: FormatToken
  ) extends Error({
        val tok = LoggerOps.log2(ft)
        val line = ft.left.pos.endLine
        s"Search state exploded on '$tok', line $line"
      }) {
    def line: Int = ft.left.pos.endLine
  }

  case class InvalidScalafmtConfiguration(throwable: Throwable)
      extends Error(s"Failed to read configuration: $throwable")

  case object NoMatchingFiles
      extends Error(
        "No files formatted/tested. " +
          "Verify include/exclude filters and command line arguments."
      )
      with NoStackTrace

  case class InvalidOption(option: String)
      extends Error(s"Invalid option $option")

  case class FailedToParseOption(path: String, error: Throwable)
      extends Error(s"Failed to read option $path, error: $error")

  case class IdempotencyViolated(msg: String) extends Error(msg)

  case object MegaTestFailed extends Error("Mega test failed.")

}
