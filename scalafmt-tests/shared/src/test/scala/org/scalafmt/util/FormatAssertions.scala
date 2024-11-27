package org.scalafmt.util

import org.scalafmt.Error.FormatterChangedAST
import org.scalafmt.Error.FormatterOutputDoesNotParse
import org.scalafmt.Scalafmt
import org.scalafmt.config.ScalafmtRunner

import org.scalameta.logger
import scala.meta.Dialect
import scala.meta.Tree
import scala.meta.internal.inputs._
import scala.meta.parsers.Parse
import scala.meta.parsers.ParseException
import scala.meta.testkit.StructurallyEqual

import java.io.ByteArrayInputStream

import munit.diff.Diffs

trait FormatAssertions {

  def assertFormatPreservesAst(
      filename: String,
      original: String,
      obtained: String,
      runner: ScalafmtRunner,
  ): Unit = assertFormatPreservesAst(filename, original, obtained)(
    runner.getParser,
    runner.getDialectForParser,
  )

  def assertFormatPreservesAst[T <: Tree](
      filename: String,
      original: String,
      obtained: String,
  )(implicit ev: Parse[T], dialect: Dialect): Unit = {
    import scala.meta._
    def toInput(code: String) = Scalafmt.toInput(code, filename)
    toInput(original).parse[T] match {
      case Parsed.Error(pos, message, _) =>
        val msgWithPos = pos.formatMessage("error", message)
        val error = s"original does not parse ($msgWithPos) [$filename]"
        logger.debug(error)
        throw FormatterOutputDoesNotParse(error, pos.startLine)
      case Parsed.Success(originalParsed) => toInput(obtained).parse[T] match {
          case Parsed.Success(obtainedParsed) =>
            StructurallyEqual(originalParsed, obtainedParsed) match {
              case Right(_) => // OK
              case Left(diff) =>
                throw FormatterChangedAST(diff.toString, obtained)
            }
          case Parsed.Error(pos, _, details: ParseException) =>
            throw FormatterOutputDoesNotParse(
              parseException2Message(details, obtained),
              pos.startLine,
            )
          case _ =>
        }
      case _ =>
    }
  }

  def formatAst(ast: String): String = {
    import scala.sys.process._
    val input = new ByteArrayInputStream(ast.getBytes("UTF-8"))
    val command = List(
      "clang-format",
      "-style={ContinuationIndentWidth: 2, ColumnLimit: 120}",
    )
    (command #< input).!!.trim
  }

  /** Creates diff from structures. WARNING: slow for large asts.
    */
  def diffAsts(original: String, obtained: String): String = Diffs
    .unifiedDiff(original.replace("(", "\n("), obtained.replace("(", "\n("))
    .linesIterator.mkString("\n")

  // TODO(olafur) move this to scala.meta?

  def parseException2Message(e: ParseException, obtained: String): String = {
    val range = 3
    val startLine = e.pos.startLine
    val lines = obtained.linesIterator
    val linesBeforeCaret = lines.slice(startLine - range + 1, startLine + 1)
    val linesAfterCaret = lines.take(range)
    Seq(
      e.shortMessage,
      linesBeforeCaret.mkString("\n"),
      " " * e.pos.startColumn + "^", // arrow
      linesAfterCaret.mkString("\n"),
      "====== full result: ======",
      obtained.stripTrailing(),
    ).filter(_.nonEmpty).mkString("", "\n", "\n")
  }
}
