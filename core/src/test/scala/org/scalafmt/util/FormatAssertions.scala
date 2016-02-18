package org.scalafmt.util

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
            assertNoDiff(originalStructure, obtainedStructure,
              "formatter changed AST!")
          case None =>
            fail("Formatter output does not parse!\n" +
              error2message(obtained, original))
        }
    }
  }
  def parses[T <: Tree](code: String)(implicit parse: Parse[T]): Option[Tree] = {
    import scala.meta._
    Try(code.parse[T]).toOption
  }
}
