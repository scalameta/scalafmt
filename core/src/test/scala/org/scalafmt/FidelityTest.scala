package org.scalafmt

import org.scalafmt.util.FileOps
import org.scalafmt.util.FormatAssertions
import org.scalatest.FunSuite

/**
  * Asserts formatter does not alter original source file's AST.
  *
  * Will maybe use scalacheck someday.
  */
class FidelityTest extends FunSuite with FormatAssertions {

  case class Test(filename: String, code: String)

  object Test {

    def apply(filename: String): Test =
      Test(filename, FileOps.readFile(filename))
  }

  val files = FileOps
    .listFiles(".")
    .filter(_.endsWith(".scala"))
    .filterNot(_.contains("/target/"))
    .filterNot(_.contains("/resources/"))

  val examples = files.map(Test.apply)

  examples.foreach { example =>
    test(example.filename) {
      val formatted =
        Scalafmt.format(example.code, ScalafmtConfig.unitTest80).get
      assertFormatPreservesAst(example.code, formatted)(scala.meta.parsers.Parse.parseSource)
      println(example.filename)
    }
  }
}
