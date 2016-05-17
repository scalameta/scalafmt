package org.scalafmt

import org.scalafmt.util.ExperimentResult
import org.scalafmt.util.ExperimentResult.ParseErr
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
      "akka",
      "intellij-scala",
      "I wan't trailing commas!!!"
  )
  val badRepos = Set(
      "kafka"
  )
  def okScalaFile(scalaFile: ScalaFile): Boolean = {
    okRepos(scalaFile.repo) && !badFile(scalaFile.filename)
  }

  def badFile(filename: String): Boolean =
    Seq(
        // TODO(olafur) include this file after fixing #140
        "project/MimaExcludes.scala",
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
      val formatted = Scalafmt
        .format(
            code, ScalafmtStyle.default.copy(alignStripMarginStrings = false))
        .get
      val elapsed = System.nanoTime() - startTime
      assertFormatPreservesAst[Source](code, formatted)
      val formattedSecondTime = Scalafmt
        .format(
            code, ScalafmtStyle.default.copy(alignStripMarginStrings = false))
        .get
      assertNoDiff(formattedSecondTime, formatted, "Idempotency")
      Success(scalaFile, elapsed)
    } else {
      Skipped(scalaFile)
    }
  }

  def scalaFiles = ScalaFile.getAll.filter(okScalaFile)
}

// TODO(olafur) integration test?
class FormatExperimentTest extends FunSuite with FormatExperiment {

  def validate(result: ExperimentResult): Unit = result match {
    case _: Success | _: Timeout | _: Skipped | _: ParseErr =>
    case failure => fail(s"""Unexpected failure:
                            |$failure""".stripMargin)
  }

  // Java 7 times out on Travis.
  if (!sys.env.contains("TRAVIS") ||
      sys.props("java.specification.version") == "1.8") {
    test(s"scalafmt formats a bunch of OSS projects") {
      runExperiment(scalaFiles)
      results.toIterable.foreach(validate)
      printResults()
    }
  }
}

object FormatExperimentApp extends FormatExperiment with App {
  runExperiment(scalaFiles)
  printResults()
}
