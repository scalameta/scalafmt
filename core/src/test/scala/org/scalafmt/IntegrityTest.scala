package org.scalafmt

import org.scalafmt.internal.ScalaFmtLogger
import org.scalafmt.util.FilesUtil
import org.scalafmt.util.FormatAssertions
import org.scalatest.FunSuite

/**
  * Asserts formatter maintains integrity of original source file.
  *
  * Will maybe use scalacheck someday.
  */
class IntegrityTest extends FunSuite with FormatAssertions with ScalaFmtLogger {
  case class Test(filename: String, code: String)
  object Test {
    def apply(filename: String): Test =
      Test(filename, FilesUtil.readFile(filename))
  }

  val files =
    Seq(
      "benchmarks/src/resources/scalafmt/Basic.scala",
      "benchmarks/src/resources/scala-js/SourceMapWriter.scala",
      "benchmarks/src/resources/scala-js/Division.scala",
      "benchmarks/src/resources/scala-js/BaseLinker.scala",
      "benchmarks/src/resources/scala-js/JSDependency.scala")

  val examples = files.map(Test.apply)

  examples.foreach { example =>
    test(example.filename) {
      println(example.filename)
      val formatted = ScalaFmt.format_!(
        example.code, ScalaStyle.ManualTest)(scala.meta.parseSource)
      assertFormatPreservesAst(example.code, formatted)(scala.meta.parseSource)
    }
  }

}
