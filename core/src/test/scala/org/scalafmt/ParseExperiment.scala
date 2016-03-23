package org.scalafmt

import org.scalafmt.util.ExperimentResult
import org.scalafmt.util.ExperimentResult.Skipped
import org.scalafmt.util.ExperimentResult.Success
import org.scalafmt.util.FormatAssertions
import org.scalafmt.util.ScalaFile
import org.scalafmt.util.ScalaProjectsExperiment
import org.scalafmt.util.ScalacParser
import scala.meta._

object ParseExperiment
    extends App with ScalaProjectsExperiment with FormatAssertions {
  override val verbose = false

  override def runOn(scalaFile: ScalaFile): ExperimentResult = {
    val code = scalaFile.read
    if (!ScalacParser.checkParseFails(code)) {
      val startTime = System.nanoTime()
      code.parse[Source]
      print("+")
      Success(scalaFile, System.nanoTime() - startTime)
    } else {
      Skipped(scalaFile)
    }
  }

  runExperiment(ScalaFile.getAll)
  printResults()
}
