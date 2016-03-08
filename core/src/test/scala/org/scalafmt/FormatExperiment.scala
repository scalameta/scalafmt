package org.scalafmt

import org.scalafmt.util.ExperimentResult
import org.scalafmt.util.ExperimentResult.Skipped
import org.scalafmt.util.ExperimentResult.Success
import org.scalafmt.util.ExperimentResult.Timeout
import org.scalafmt.util.FilesUtil
import org.scalafmt.util.FormatAssertions
import org.scalafmt.util.ScalaProjectsExperiment
import org.scalafmt.util.ScalacParser
import org.scalatest.FunSuite

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.meta._

import scala.collection.JavaConversions._

trait FormatExperiment extends ScalaProjectsExperiment with FormatAssertions {
  override val verbose = false
  override val skipProject: String => Boolean =
    !_.contains("scala-js/scala-js")

  val awaitMaxDuration =
    // Can't guarantee performance on Travis
    if (sys.env.contains("TRAVIS")) Duration(1, "min")
    // Max on @olafurpg's machine is 5.9s, 2,5 GHz Intel Core i7 Macbook Pro.
    else Duration(20, "s")

  def ignore(filename: String): Boolean =
    Seq(
        "emitter/JSDesugaring.scala",
        "js/ThisFunction.scala", // Computer generated.
        "js/Any.scala" // Computer generated.
    ).exists(filename.contains)

  override def runOn(filename: String): Boolean = {
    if (!ignore(filename)) {
      val code = FilesUtil.readFile(filename)
      if (!ScalacParser.checkParseFails(code)) {
        val startTime = System.nanoTime()
        val f = Future(ScalaFmt.format_![Source](code, ScalaStyle.Default))
        val formatted = Await.result(f, awaitMaxDuration)
        assertFormatPreservesAst[Source](code, formatted)
        print("+")
        results.add(Success(filename, System.nanoTime() - startTime))
      } else {
        results.add(Skipped(filename))
      }
    } else {
      false
    }
  }
}

// TODO(olafur) integration test?

class FormatExperimentTest extends FunSuite with FormatExperiment {

  def validate(result: ExperimentResult): Unit =
    result match {
      case _: Success | _: Timeout =>
      case failure => fail(s"""Unexpected failure:
            |$failure""".stripMargin)
    }

  test("scalafmt formats all files in Scala.js") {
    runExperiment()
    results.toIterable.foreach(validate)
    printResults()
  }
}

object FormatExperimentApp extends FormatExperiment with App {
  runExperiment()
  printResults()
}
