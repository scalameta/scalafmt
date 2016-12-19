package org.scalafmt.util

import scala.meta.Tree
import scala.meta.parsers.Parse
import scala.meta.parsers.ParseException

import java.io.ByteArrayInputStream

import org.scalafmt.Error.FormatterChangedAST
import org.scalafmt.Error.FormatterOutputDoesNotParse
import org.scalatest.FunSuiteLike

trait FormatAssertions extends FunSuiteLike with DiffAssertions {

  def assertFormatPreservesAst[T <: Tree](original: String, obtained: String)(
      implicit ev: Parse[T]): Unit = {
    import scala.meta._
    original.parse[T] match {
      case Parsed.Error(pos, message, details) =>
        logger.warn(original)
        logger.warn(s"original does not parse $message")
      case Parsed.Success(originalParsed) =>
        obtained.parse[T] match {
          case Parsed.Success(obtainedParsed) =>
            val originalStructure = originalParsed.show[Structure]
            val obtainedStructure = obtainedParsed.show[Structure]
            if (originalStructure.trim != obtainedStructure.trim) {
              // TODO(olafur) Can produce false negatives, see
              // https://github.com/scalameta/scalameta/issues/342
              throw FormatterChangedAST(
                diffAsts(originalStructure, obtainedStructure),
                obtained)
            }
          case Parsed.Error(pos, message, details: ParseException) =>
            throw FormatterOutputDoesNotParse(
              parseException2Message(details, obtained))
          case _ =>
        }
    }
  }

  def formatAst(ast: String): String = {
    import scala.sys.process._
    val input = new ByteArrayInputStream(ast.getBytes("UTF-8"))
    val command = List("clang-format",
                       "-style={ContinuationIndentWidth: 2, ColumnLimit: 120}")
    (command #< input).!!.trim
  }

  /**
    * Creates diff from structures.
    * WARNING: slow for large asts.
    */
  def diffAsts(original: String, obtained: String): String = {
//    compareContents(formatAst(original), formatAst(obtained))
    compareContents(original.replace("(", "\n("), obtained.replace("(", "\n(")).lines
      .mkString("\n")
  }

  // TODO(olafur) move this to scala.meta?

  def parseException2Message(e: ParseException, obtained: String): String = {
    val range = 3
    val i = e.pos.start.line
    val lines = obtained.lines.toVector
    val arrow = (" " * (e.pos.start.column - 2)) + "^"
    s"""${lines.slice(i - range, i + 1).mkString("\n")}
       |$arrow
       |${e.getMessage}
       |${lines.slice(i + 1, i + range).mkString("\n")}
       |$obtained
       |""".stripMargin
  }
}
