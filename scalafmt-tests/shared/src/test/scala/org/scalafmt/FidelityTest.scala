package org.scalafmt

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.sysops.FileOps
import org.scalafmt.util.FormatAssertions

import scala.meta.dialects.Scala213

import java.io.File
import java.nio.file.Path

import munit.FunSuite

/** Asserts formatter does not alter original source file's AST.
  *
  * Will maybe use scalacheck someday.
  */
class FidelityTest extends FunSuite with FormatAssertions {

  case class TestCase(path: Path, code: String) {
    def filename = path.toString
  }

  val examples = {
    val denyList = Set(
      "ConfigReader.scala",
      "BuildTime.scala",
      "GitCommit.scala",
      "/target/",
      "/resources/",
      "/gh-pages/",
    ).map(_.replace("/", File.separator))
    FileOps.listFiles(".").filter { x =>
      val filename = x.toString
      filename.endsWith(".scala") && !denyList.exists(filename.contains)
    }.map(x => TestCase(x, FileOps.readFile(x)))
  }

  examples.foreach(example =>
    test(example.filename) {
      val formatted = Scalafmt.formatCode(
        example.code,
        ScalafmtConfig.default,
        filename = example.filename,
      )
      assertFormatPreservesAst(example.filename, example.code, formatted.get)(
        scala.meta.parsers.Parse.parseSource,
        Scala213,
      )
    },
  )
}
