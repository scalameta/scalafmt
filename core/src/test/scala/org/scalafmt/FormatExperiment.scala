package org.scalafmt

import org.scalafmt.internal.ScalaFmtLogger._
import org.scalafmt.util.ExperimentResult
import org.scalafmt.util.ExperimentResult.Skipped
import org.scalafmt.util.ExperimentResult.Success
import org.scalafmt.util.ExperimentResult.Timeout
import org.scalafmt.util.FilesUtil
import org.scalafmt.util.FormatAssertions
import org.scalafmt.util.ScalaFile
import org.scalafmt.util.ScalaProjectsExperiment
import org.scalafmt.util.ScalacParser
import org.scalatest.FunSuite

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.meta._

import scala.collection.JavaConversions._
import scalaz.concurrent.Task

trait FormatExperiment extends ScalaProjectsExperiment with FormatAssertions {
  override val verbose = false

  val okRepos = Set(
      "goose",
      "scala-js",
      "fastparse",
//      "scalding",
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
        "emitter/JSDesugaring.scala",
        "js/ThisFunction.scala", // Computer generated.
        "js/Any.scala" // Computer generated.
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
    case _: Success | _: Timeout =>
    case failure => fail(s"""Unexpected failure:
                            |$failure""".stripMargin)
  }

  test("scalafmt formats all files in Scala.js") {
    runExperiment(scalaFiles)
    results.toIterable.foreach(validate)
    printResults()
  }
}

object FormatExperimentApp extends FormatExperiment with App {
  runExperiment(scalaFiles)
  printResults()
}
