package org.scalafmt.util

import scala.meta.Dialect
import scala.meta.Tree
import scala.meta.parsers.Parse
import scala.meta.parsers.ParseException
import scala.meta.testkit.StructurallyEqual

import java.io.ByteArrayInputStream

import org.scalafmt.Error.FormatterChangedAST
import org.scalafmt.Error.FormatterOutputDoesNotParse
import org.scalameta.logger
import org.scalatest.FunSuiteLike

trait FormatAssertions extends FunSuiteLike with DiffAssertions {

  def assertFormatPreservesAst[T <: Tree](
      original: String,
      obtained: String
  )(implicit ev: Parse[T], dialect: Dialect): Unit = {
    import scala.meta._
    original.parse[T] match {
      case Parsed.Error(pos, message, details) =>
        logger.debug(original)
        logger.debug(s"original does not parse $message")
      case Parsed.Success(originalParsed) =>
        dialect(obtained).parse[T] match {
          case Parsed.Success(obtainedParsed) =>
            StructurallyEqual(originalParsed, obtainedParsed) match {
              case Right(_) => // OK
              case Left(diff) =>
                throw FormatterChangedAST(diff.toString, obtained)
            }
          case Parsed.Error(pos, message, details: ParseException) =>
            throw FormatterOutputDoesNotParse(
              parseException2Message(details, obtained),
              pos.startLine
            )
          case _ =>
        }
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

  /**
    * Creates diff from structures.
    * WARNING: slow for large asts.
    */
  def diffAsts(original: String, obtained: String): String = {
//    compareContents(formatAst(original), formatAst(obtained))
    // Predef.augmentString = work around scala/bug#11125 on JDK 11
    augmentString(
      compareContents(
        original.replace("(", "\n("),
        obtained.replace("(", "\n(")
      )
    ).lines.mkString("\n")
  }

  // TODO(olafur) move this to scala.meta?

  def parseException2Message(e: ParseException, obtained: String): String = {
    val range = 3
    val i = e.pos.startLine
    // Predef.augmentString = work around scala/bug#11125 on JDK 11
    val lines = augmentString(obtained).lines.toVector
    val arrow = (" " * (e.pos.startColumn - 2)) + "^"
    s"""${lines.slice(i - range, i + 1).mkString("\n")}
       |$arrow
       |${e.getMessage}
       |${lines.slice(i + 1, i + range).mkString("\n")}
       |$obtained
       |""".stripMargin
  }
}
