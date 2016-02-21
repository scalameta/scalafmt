package org.scalafmt.util

import org.scalafmt.Error.FormatterChangedAST
import org.scalafmt.Error.FormatterOutputDoesNotParse
import org.scalatest.FunSuite
import org.scalatest.FunSuiteLike

import scala.meta.Tree
import scala.meta.parsers.common.Parse
import scala.meta.prettyprinters.Structure
import scala.util.Try

trait FormatAssertions extends FunSuiteLike with DiffAssertions {
  def assertFormatPreservesAst[T <: Tree](original: String,
                                          obtained: String)
                                         (implicit ev: Parse[T]): Unit = {
    parses(original) match {
      case None => // ignore
      case Some(originalParsed) =>
        parses(obtained) match {
          case Some(obtainedParsed) =>
            val originalStructure = originalParsed.show[Structure]
            val obtainedStructure = obtainedParsed.show[Structure]
            if (originalStructure != obtainedStructure) {
              throw FormatterChangedAST
            }
          case None =>
            throw FormatterOutputDoesNotParse(error2message(obtained, original))
        }
    }
  }
  def parses[T <: Tree](code: String)(implicit parse: Parse[T]): Option[Tree] = {
    import scala.meta._
    Try(code.parse[T]).toOption
  }
}
