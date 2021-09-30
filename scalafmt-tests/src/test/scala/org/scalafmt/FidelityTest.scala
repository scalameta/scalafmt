package org.scalafmt

import scala.meta.dialects.Scala211

import java.io.File

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.FileOps
import org.scalafmt.util.FormatAssertions
import munit.FunSuite

/** Asserts formatter does not alter original source file's AST.
  *
  * Will maybe use scalacheck someday.
  */
class FidelityTest extends FunSuite with FormatAssertions {

  case class TestCase(filename: String, code: String)

  object TestCase {

    def apply(filename: String): TestCase =
      TestCase(filename, FileOps.readFile(filename))
  }

  val files = FileOps
    .listFiles(".")
    .filter(_.endsWith(".scala"))
    .filterNot(x =>
      Set(
        "ConfigReader.scala",
        "BuildTime.scala",
        "GitCommit.scala",
        "/target/",
        "/resources/",
        "/gh-pages/"
      ).map(_.replace("/", File.separator)).exists(x.contains)
    )

  val examples = files.map(TestCase.apply)

  examples.foreach { example =>
    test(example.filename) {
      val formatted = Scalafmt.formatCode(
        example.code,
        ScalafmtConfig.default,
        filename = example.filename
      )
      assertFormatPreservesAst(example.code, formatted.get)(
        scala.meta.parsers.Parse.parseSource,
        Scala211
      )
    }
  }
}
