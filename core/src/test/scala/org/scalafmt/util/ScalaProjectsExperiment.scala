package org.scalafmt.util

import java.nio.file.Files
import java.nio.file.Paths
import java.text.NumberFormat
import java.util.Locale
import java.util.concurrent.CopyOnWriteArrayList

import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics
import org.scalafmt.internal.ScalaFmtLogger
import org.scalafmt.util.ExperimentResult.ParseErr
import org.scalafmt.util.ExperimentResult.Success
import org.scalafmt.util.ExperimentResult.Timeout
import org.scalafmt.util.ExperimentResult.UnknownFailure

import scala.collection.JavaConversions._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.meta._
import scala.util.control.NonFatal

/**
  * Mostly borrowed from
  * https://github.com/lihaoyi/fastparse/blob/0d67eca8f9264bfaff68e5cbb227045ceac4a15f/scalaparse/jvm/src/test/scala/scalaparse/ProjectTests.scala
  */
trait ScalaProjectsExperiment extends ScalaFmtLogger {
  val results: java.util.List[ExperimentResult] = new CopyOnWriteArrayList[ExperimentResult]()
  val verbose = false
  val globalFilter: String => Boolean = _ => true
  val colLength = 10
  val numberFormat = {
    // I prefer . as thousand separator.
    val format = NumberFormat.getNumberInstance(Locale.GERMAN)
    format.setMaximumFractionDigits(3)
    format
  }
  private val pathRoot = "target/repos/"

  /**
    * Implement this method.
    *
    * @param filename
    */
  def runOn(filename: String): Boolean

  def runExperiment(): Unit = {
    checkRepo("https://github.com/GravityLabs/goose")
    checkRepo("https://github.com/JetBrains/intellij-scala")
    checkRepo("https://github.com/PredictionIO/PredictionIO")
    checkRepo("https://github.com/akka/akka")
    checkRepo("https://github.com/apache/kafka")
    checkRepo("https://github.com/apache/spark")
    checkRepo("https://github.com/ensime/ensime-server")
    checkRepo("https://github.com/lift/framework")
    checkRepo("https://github.com/lihaoyi/fastparse")
    checkRepo("https://github.com/mesosphere/marathon")
    checkRepo("https://github.com/milessabin/shapeless")
    checkRepo("https://github.com/non/cats")
    checkRepo("https://github.com/non/spire")
    checkRepo("https://github.com/ornicar/lila", lilaIgnore)
    checkRepo("https://github.com/playframework/playframework")
    checkRepo("https://github.com/pocorall/scaloid")
    checkRepo("https://github.com/precog/platform")
    checkRepo("https://github.com/saddle/saddle")
    checkRepo("https://github.com/sbt/sbt", sbtIgnore)
    checkRepo("https://github.com/scala-ide/scala-ide")
    checkRepo("https://github.com/scala-js/scala-js")
    checkRepo("https://github.com/scala/pickling")
    checkRepo("https://github.com/scala/scala", scalaIgnore)
    checkRepo("https://github.com/scalafx/scalafx")
    checkRepo("https://github.com/scalafx/scalafx-ensemble")
    checkRepo("https://github.com/scalanlp/breeze")
    checkRepo("https://github.com/scalatra/scalatra")
    checkRepo("https://github.com/scalaz/scalaz")
    checkRepo("https://github.com/slick/slick")
    checkRepo("https://github.com/takezoe/gitbucket")
    checkRepo("https://github.com/twitter/finagle")
    checkRepo("https://github.com/twitter/scalding")
    checkRepo("https://github.com/twitter/summingbird")
    checkRepo("https://github.com/twitter/util")
  }

  private def checkRepo(url: String, filter: String => Boolean = _ => true): Unit = {
    if (globalFilter(url)) return
    import FilesUtil._

    import sys.process._
    val name = repoName(url)
    val path = pathRoot + name
    println("CLONING?")
    if (!Files.exists(Paths.get("target", "repos", name))) {
      println("CLONING")
      val exit =
        List("git", "clone", url, "target/repos/" + name, "--depth", "1").!
    }
    val branch = Seq("git", s"--git-dir=$path/.git", "rev-parse", "HEAD").!!
    println("Checking project " + name)
    val files = listFiles(path).withFilter(x => filter(x) &&
      x.endsWith(".scala")).map(filename => {
      val fileUrl = s"$url/blob/$branch${filename.stripPrefix(path)}"
      run(filename).recover(recoverError(fileUrl))
    })
    files.foreach(Await.result(_, files.length.seconds))
    println("")
  }

  def recoverError(fileUrl: String): PartialFunction[Throwable, Boolean] = {
    case e: ParseException => results.add(ParseErr(fileUrl, e))
    case e: java.util.concurrent.TimeoutException =>
      print("-")
      results.add(Timeout(fileUrl))
    case NonFatal(e) =>
      print("X")
      results.add(UnknownFailure(fileUrl, e))
  }

  private def repoName(url: String): String = url.split("/").last

  private def run(filename: String): Future[Boolean] = Future(runOn(filename))

  private def sbtIgnore: String => Boolean =
    x => !Seq(
      // Unicode escapes in weird places
      "target/repos/sbt/main/settings/src/main/scala/sbt/std/InputWrapper.scala",
      // uses a package called `macro`
      "target/repos/sbt/sbt/src/sbt-test/source-dependencies/inherited-macros",
      "target/repos/sbt/sbt/src/sbt-test/source-dependencies/macro")
      .exists(x.startsWith)

  private def scalaIgnore: String => Boolean =
    x => !Seq(
      // This fella seems to make the scalac parser hang (???)
      "target/repos/scala/test/files/neg/t5510.scala",
      // Unicode escapes in weird places
      "target/repos/scala/test/files/neg/t8015-ffb.scala",
      "target/repos/scala/test/files/pos/t389.scala",
      "target/repos/scala/test/files/run/literals.scala",
      "target/repos/scala/test/files/run/t3835.scala",
      // Scalac parser seems to accept this, though it blows up later
      "target/repos/scala/test/files/neg/t8266-invalid-interp.scala",
      "target/repos/scala/test/disabled/",
      "target/repos/scala/test/files/neg/",
      // trailing . after number
      "target/repos/scala/test/files/presentation/infix-completion/src/Snippet.scala")
      .exists(x.startsWith)

  private def lilaIgnore: String => Boolean =
    x => !Seq(
      "target/repos/lila/modules/lobby/src/main/SocketHandler.scala")
      .exists(x.startsWith)

  def printResults(): Unit = {
    println(header("Summary:"))
    results.groupBy(_.key).foreach {
      case (categoryName, categoryResults) =>
        println(s"$categoryName: ${categoryResults.length}")
    }
    val formatStats = new DescriptiveStatistics()
    results.foreach {
      case x: Success =>
        formatStats.addValue(x.nanos)
      case _ =>
    }
    println(s"Total: ${results.length}")
    println(s"${summarize(formatStats)}")
  }

  private def header(msg: String) = s"\n\n## $msg\n"

  private def summarize(stats: DescriptiveStatistics): String =
    Tabulator.format(Seq(
      Seq("Max", "Min", "Sum", "Mean", "Q1", "Q2", "Q3"),
      Seq(stats.getMax, stats.getMin,
        stats.getSum, stats.getMean,
        stats.getPercentile(25),
        stats.getPercentile(50),
        stats.getPercentile(75)).map(formatNumber)
    ))

  private def formatNumber(x: Any): String = x match {
    case d: Double =>
      // TODO(olafur) no better lib to do this?
      val ms = d / Math.pow(10, 6)
      numberFormat.format(ms) + " ms"
    case _ => x.toString
  }

  private def col(strings: Any*): String = strings.map { s =>
    val x = s match {
      case d: Double => numberFormat.format(d)
      case _ => s
    }
    x.toString.slice(0, colLength - 2).padTo(colLength - 1, " ").mkString
  }.mkString(" ")

}
