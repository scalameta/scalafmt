package org.scalafmt

import org.scalafmt.util.ExperimentResult.Skipped
import org.scalafmt.util.ExperimentResult.Success
import org.scalafmt.util.FilesUtil
import org.scalafmt.util.FormatAssertions
import org.scalafmt.util.ScalaProjectsExperiment
import org.scalafmt.util.ScalacParser

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.meta._


object ParseExperiment extends App with ScalaProjectsExperiment with FormatAssertions {
  override val verbose = false

//  override val skipProject: String => Boolean = !_.contains("intelli")
  override def runOn(filename: String): Boolean = {
    val code = FilesUtil.readFile(filename)
    if (!ScalacParser.checkParseFails(code)) {
      val startTime = System.nanoTime()
      code.parse[Source]
      print("+")
      results.add(Success(filename, System.nanoTime() - startTime))
    } else {
      results.add(Skipped(filename))
    }
  }

  runExperiment()
  printResults()
}
