package org.scalafmt

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.meta._


object FormatExperiment extends App with GithubExperiment {
  override val verbose = false
  override val globalFilter: String => Boolean = !_.contains("scala-js/scala-js")
  val fmt = new ScalaFmt(Standard)

  override def runOn(filename: String): Boolean = {
    val code = FilesUtil.readFile(filename)
    if (!ScalacParser.checkParseFails(code)) {
      val t = Stopwatch()
      val f = Future(fmt.format_![Source](code))
      Await.result(f, Standard.maxDuration)
      print("+")
      formatSuccesses.add(FormatSuccess(filename, t.elapsedNs))
    } else {
      skipped.add(filename)
    }
  }
  runExperiment()
  printResults()
}
