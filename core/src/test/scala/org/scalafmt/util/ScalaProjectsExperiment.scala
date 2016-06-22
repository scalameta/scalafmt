package org.scalafmt.util

import scala.concurrent.Await
import scala.concurrent.Future
import scala.util.Try

import java.io.File
import java.text.NumberFormat
import java.util.Locale
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.atomic.AtomicInteger

import org.apache.commons.math3.stat.descriptive.DescriptiveStatistics
import org.scalafmt.Error
import org.scalafmt.util.ExperimentResult.ParseErr
import org.scalafmt.util.ExperimentResult.Skipped
import org.scalafmt.util.ExperimentResult.Success
import org.scalafmt.util.ExperimentResult.Timeout
import org.scalafmt.util.ExperimentResult.UnknownFailure
import scala.annotation.tailrec
import scala.collection.JavaConversions._
import scala.concurrent.duration.Duration
import scala.meta._
import scala.util.control.NonFatal
import scalaz.concurrent.Task

/**
  * Mostly borrowed from
  * https://github.com/lihaoyi/fastparse/blob/0d67eca8f9264bfaff68e5cbb227045ceac4a15f/scalaparse/jvm/src/test/scala/scalaparse/ProjectTests.scala
  */
trait ScalaProjectsExperiment {
  val awaitMaxDuration =
    // Can't guarantee performance on Travis
    if (sys.env.contains("TRAVIS")) Duration(60, "s")
    // Max on @olafurpg's machine is 5.9s, 2,5 GHz Intel Core i7 Macbook Pro.
    else Duration(10, "s")

  val results: java.util.List[ExperimentResult] =
    new CopyOnWriteArrayList[ExperimentResult]()
  var totalNanos: Long = _
  val verbose = false
  val colLength = 10
  val numberFormat = {
    // I prefer . as thousand separator.
    val format = NumberFormat.getNumberInstance(Locale.GERMAN)
    format.setMaximumFractionDigits(0)
    format
  }

  val separator = File.pathSeparator

  def runOn(scalaFile: ScalaFile): ExperimentResult

  def runExperiment(scalaFiles: Seq[ScalaFile]): Unit = {
    val start = System.nanoTime()
    val counter = new AtomicInteger()
    val N = scalaFiles.length
    scalaFiles.toArray.par.foreach { file =>
      val result = Try(runOn(file)).recover(recoverError(file)).get
      results.add(result)
      val count = counter.incrementAndGet()
      if (count % 100 == 0) {
        println(s"$count processed, ${N - count} remaining...")
      }
    }
    totalNanos = System.nanoTime() - start
  }

  def recoverError(
      scalaFile: ScalaFile): PartialFunction[Throwable, ExperimentResult] = {
    case e: ParseException => ParseErr(scalaFile, e)
    case e: Error.SearchStateExploded =>
      ExperimentResult.SearchStateExploded(scalaFile, e.deepestState)
    case e: java.util.concurrent.TimeoutException =>
      println(s"- $scalaFile")
      Timeout(scalaFile)
    case NonFatal(e) =>
      print(s"X ${e.getClass} $scalaFile")
      UnknownFailure(scalaFile, e)
  }

  private def repoName(url: String): String = url.split("/").last

  private def lilaIgnore: String => Boolean = x => !Seq().exists(x.startsWith)

  def printResults(): Unit = {
    println(header("Summary:"))
    results.groupBy(_.key).foreach {
      case (categoryName, categoryResults) =>
        println(s"$categoryName: ${categoryResults.length}")
        if (categoryResults.nonEmpty) {
          println()
        }
        categoryResults.foreach {
          case _: Success | _: Skipped =>
          case e => println(e)
        }
        if (categoryResults.nonEmpty) {
          println()
        }
    }
    val formatStats = new DescriptiveStatistics()
    results.foreach {
      case x: Success => formatStats.addValue(x.nanos)
      case _ =>
    }
    println(s"Total: ${results.length}")
    println(s"${summarize(formatStats)}")
  }

  private def header(msg: String) = s"\n\n## $msg\n"

  private def summarize(stats: DescriptiveStatistics): String =
    Tabulator.format(
        Seq(
            Seq("Total time",
                "Max",
                "Min",
                "Mean",
                "Q2",
                "Q3",
                "90th",
                "95th",
                "99th"),
            Seq(totalNanos,
                stats.getMax,
                stats.getMin,
                stats.getMean,
                stats.getPercentile(50),
                stats.getPercentile(75),
                stats.getPercentile(90),
                stats.getPercentile(95),
                stats.getPercentile(99)).map(formatNumber)
        ))

  private def formatNumber(x: Any): String = x match {
    case d: Double =>
      // TODO(olafur) no better lib to do this?
      val ms = d / Math.pow(10, 6)
      numberFormat.format(ms) + " ms"
      Math.round(ms).toString
    case _ => x.toString
  }

  private def col(strings: Any*): String =
    strings.map { s =>
      val x = s match {
        case d: Double => numberFormat.format(d)
        case _ => s
      }
      x.toString.slice(0, colLength - 2).padTo(colLength - 1, " ").mkString
    }.mkString(" ")
}
