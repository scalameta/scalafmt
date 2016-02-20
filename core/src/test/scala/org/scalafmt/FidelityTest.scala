package org.scalafmt

import org.scalafmt.internal.ScalaFmtLogger
import org.scalafmt.util.FilesUtil
import org.scalafmt.util.FormatAssertions
import org.scalatest.FunSuite

/**
  * Asserts formatter does not alter original source file's AST.
  *
  * Will maybe use scalacheck someday.
  */
class FidelityTest extends FunSuite with FormatAssertions with ScalaFmtLogger {
  case class Test(filename: String, code: String)
  object Test {
    def apply(filename: String): Test =
      Test(filename, FilesUtil.readFile(filename))
  }


  // TODO(olafur) append to [[files]]
  val thisProject = FilesUtil.listFiles(".")
    .filter(_.endsWith(".scala"))
    .filterNot(_.contains("/target/"))
    .filterNot(_.contains("/resources/"))

  val files =
    Seq(
      "core/src/test/resources/standard/TestingClassNameDoNotEdit.scalafmt",
      "benchmarks/src/resources/scalafmt/Basic.scala",
      "benchmarks/src/resources/scala-js/SourceMapWriter.scala",
      "benchmarks/src/resources/scala-js/Division.scala",
      "benchmarks/src/resources/scala-js/BaseLinker.scala",
      "benchmarks/src/resources/scala-js/JSDependency.scala")


  val examples = files.map(Test.apply)

  examples.foreach { example =>
    test(example.filename) {
      val formatted = ScalaFmt.format_!(
        example.code, ScalaStyle.UnitTest80)(scala.meta.parseSource)
      assertFormatPreservesAst(example.code, formatted)(scala.meta.parseSource)
      println(example.filename)
    }
  }

}
