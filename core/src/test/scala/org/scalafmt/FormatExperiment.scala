package org.scalafmt

import org.scalafmt.util.ExperimentResult
import org.scalafmt.util.ExperimentResult.Skipped
import org.scalafmt.util.ExperimentResult.Success
import org.scalafmt.util.ExperimentResult.Timeout
import org.scalafmt.util.FormatAssertions
import org.scalafmt.util.LoggerOps
import org.scalafmt.util.ScalaFile
import org.scalafmt.util.ScalaProjectsExperiment
import org.scalafmt.util.ScalacParser
import org.scalatest.FunSuite
import scala.collection.JavaConversions._
import scala.meta._

trait FormatExperiment extends ScalaProjectsExperiment with FormatAssertions {
  import LoggerOps._
  override val verbose = false

  val okRepos = Set(
      "goose",
      "scala-js",
      "fastparse",
      "scalding",
      "spark",
      "I wan't trailing commas!!!"
  )
  val badRepos = Set(
      "kafka"
  )
  def okScalaFile(scalaFile: ScalaFile): Boolean = {
    okRepos(scalaFile.repo) && !badFile(scalaFile.filename)
  }

  def badFile(filename: String): Boolean =
//  !Seq(
//    "mllib/src/main/scala/org/apache/spark/ml/param/shared/SharedParamsCodeGen.scala",
//    "project/MimaExcludes.scala",
//    "sql/catalyst/src/test/scala/org/apache/spark/sql/catalyst/ScalaReflectionSuite.scala",
//    "sql/core/src/test/scala/org/apache/spark/sql/JoinSuite.scala",
//    "sql/core/src/test/scala/org/apache/spark/sql/execution/datasources/json/JsonSuite.scala",
//    "sql/core/src/test/scala/org/apache/spark/sql/sources/TableScanSuite.scala"
//  ).exists(filename.contains)
//
//    !filename.contains(
//
//  "catalyst/src/main/scala/org/apache/spark/sql/catalyst"
//       yarn/src/test/scala/org/apache/spark/network/
//      "sql/hive/src/test/scala/org/apache/spark/sql"
//     ) ||
    Seq(
        // These format fine when run individually, but hog when run together with other files.
        "core/src/main/scala/org/apache/spark/deploy/SparkSubmit.scala",
        "sql/hive/src/test/scala/org/apache/spark/sql/hive/execution/WindowQuerySuite.scala",
        "core/src/main/scala/org/apache/spark/SparkConf.scala",
        // Formats OK, but contains huge function calls which
        // would definitely be excluded from automatic formatting.
        "javalanglib/src/main/scala/java/lang/Character.scala",
        // Duplicate file, both in scala.js and fastparse.
        "jvm/src/test/resources/scalaparse/GenJSCode.scala",
        // Auto generated files
        "scalding-core/src/main/scala/com/twitter/scalding/macros/impl/TypeDescriptorProviderImpl.scala",
        "scalding/serialization/macros/impl/ordered_serialization/providers/ProductOrderedBuf.scala",
        "scalding-core/src/main/scala/com/twitter/scalding/typed/GeneratedFlattenGroup.scala",
        "emitter/JSDesugaring.scala",
        "js/ThisFunction.scala",
        "js/Any.scala"
    ).exists(filename.contains)

  override def runOn(scalaFile: ScalaFile): ExperimentResult = {
    val code = scalaFile.read
    if (!ScalacParser.checkParseFails(code)) {
      val startTime = System.nanoTime()
      val formatted =
        ScalaFmt.format_![Source](code, ScalaStyle.NoIndentStripMargin)
      assertFormatPreservesAst[Source](code, formatted)
      print("+")
      Success(scalaFile, System.nanoTime() - startTime)
    } else {
      Skipped(scalaFile)
    }
  }

  def scalaFiles = ScalaFile.getAll.filter(okScalaFile)
}

// TODO(olafur) integration test?

class FormatExperimentTest extends FunSuite with FormatExperiment {

  def validate(result: ExperimentResult): Unit = result match {
    case _: Success | _: Timeout | _: Skipped =>
    case failure => fail(s"""Unexpected failure:
                            |$failure""".stripMargin)
  }

  test(s"scalafmt formats a bunch of OSS projects") {
    runExperiment(scalaFiles)
    results.toIterable.foreach(validate)
    printResults()
  }
}

object FormatExperimentApp extends FormatExperiment with App {
  runExperiment(scalaFiles)
  printResults()
}
