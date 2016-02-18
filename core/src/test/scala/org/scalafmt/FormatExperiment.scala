package org.scalafmt

import org.scalafmt.util.FilesUtil
import org.scalafmt.util.FormatAssertions
import org.scalafmt.util.ScalaProjectsExperiment
import org.scalafmt.util.ScalacParser

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.meta._


object FormatExperiment extends App with ScalaProjectsExperiment with FormatAssertions {
  override val verbose = false
  override val globalFilter: String => Boolean = !_.contains("scala-js/scala-js")

  override def runOn(filename: String): Boolean = {
    val code = FilesUtil.readFile(filename)
    if (!ScalacParser.checkParseFails(code)) {
      val startTime = System.nanoTime()
      val f = Future(ScalaFmt.format_![Source](code, ScalaStyle.Standard))
      val formatted = Await.result(f, ScalaStyle.Standard.maxDuration)
      assertFormatPreservesAst[Source](code, formatted)
      print("+")
      formatSuccesses.add(FormatSuccess(filename, System.nanoTime() - startTime))
    } else {
      skipped.add(filename)
    }
  }

  runExperiment()
  printResults()
}
