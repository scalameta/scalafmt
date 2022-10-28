package org.scalafmt.util

import java.io.ByteArrayInputStream

import munit.internal.difflib.Diffs
import org.scalafmt.Error.{FormatterChangedAST, FormatterOutputDoesNotParse}
import org.scalafmt.Scalafmt
import org.scalafmt.config.ScalafmtRunner
import org.scalameta.logger

import scala.meta.parsers.{Parse, ParseException}
import scala.meta.testkit.StructurallyEqual
import scala.meta.{Dialect, Tree}

trait FormatAssertions {

  def assertFormatPreservesAst(
      filename: String,
      original: String,
      obtained: String,
      runner: ScalafmtRunner
  ): Unit =
    assertFormatPreservesAst(filename, original, obtained)(
      runner.getParser,
      runner.getDialectForParser
    )

  def assertFormatPreservesAst[T <: Tree](
      filename: String,
      original: String,
      obtained: String
  )(implicit ev: Parse[T], dialect: Dialect): Unit = {
    import scala.meta._
    def toInput(code: String) = Scalafmt.toInput(code, filename)
    toInput(original).parse[T] match {
      case Parsed.Error(_, message, _) =>
        logger.debug(original)
        logger.debug(s"original does not parse $message")
      case Parsed.Success(originalParsed) =>
        toInput(obtained).parse[T] match {
          case Parsed.Success(obtainedParsed) =>
            StructurallyEqual(originalParsed, obtainedParsed) match {
              case Right(_) => // OK
              case Left(diff) =>
                throw FormatterChangedAST(diff.toString, obtained)
            }
          case Parsed.Error(pos, _, details: ParseException) =>
            throw FormatterOutputDoesNotParse(
              parseException2Message(details, obtained),
              pos.startLine
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
      "-style={ContinuationIndentWidth: 2, ColumnLimit: 120}"
    )
    (command #< input).!!.trim
  }

  /** Creates diff from structures. WARNING: slow for large asts.
    */
  def diffAsts(original: String, obtained: String): String = {
    Diffs
      .unifiedDiff(
        original.replace("(", "\n("),
        obtained.replace("(", "\n(")
      )
      .linesIterator
      .mkString("\n")
  }

  // TODO(olafur) move this to scala.meta?

  def parseException2Message(e: ParseException, obtained: String): String = {
    val range = 3
    val i = e.pos.startLine
    val lines = obtained.linesIterator.toVector
    val arrow = (" " * (e.pos.startColumn - 2)) + "^"
    s"""${lines.slice(i - range, i + 1).mkString("\n")}
      |$arrow
      |${e.getMessage}
      |${lines.slice(i + 1, i + range).mkString("\n")}
      |$obtained
      |""".stripMargin
  }
}
