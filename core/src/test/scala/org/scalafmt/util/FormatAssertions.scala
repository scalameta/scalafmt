package org.scalafmt.util

import java.io.ByteArrayInputStream

import org.scalafmt.internal.ScalaFmtLogger._
import org.scalafmt.Error.FormatterChangedAST
import org.scalafmt.Error.FormatterOutputDoesNotParse
import org.scalatest.FunSuiteLike

import scala.meta.Tree
import scala.meta.parsers.common.Parse
import scala.meta.parsers.common.ParseException
import scala.util.Failure
import scala.util.Success
import scala.util.Try

trait FormatAssertions extends FunSuiteLike with DiffAssertions {

  def assertFormatPreservesAst[T <: Tree](original: String, obtained: String)(
      implicit ev: Parse[T]): Unit = {
    import scala.meta._
    Try(original.parse[T]) match {
      case Failure(t) => // ignore
        logger.warn(s"original does not parse $t")
      case Success(originalParsed) =>
        Try(obtained.parse[T]) match {
          case Success(obtainedParsed) =>
            val originalStructure = originalParsed.show[Structure]
            val obtainedStructure = obtainedParsed.show[Structure]
            if (originalStructure.trim != obtainedStructure.trim) {
              // TODO(olafur) Can produce false negatives, see
              // https://github.com/scalameta/scalameta/issues/342
              throw FormatterChangedAST(
                  diffAsts(originalStructure, obtainedStructure),
                  obtained)
            }
          case Failure(e: ParseException) =>
            throw FormatterOutputDoesNotParse(
                parseException2Message(e, obtained))
          case Failure(e: TokenizeException) =>
            throw FormatterOutputDoesNotParse(s"""${e.getMessage}
                                                 |$obtained
                                                 |""".stripMargin)
          case Failure(e) =>
            logger.error(s"Unexpected error $e, $obtained")
            throw e
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
