package org.scalafmt

import scala.util.Random
import scala.util.Try
import scalariform.formatter.ScalaFormatter
import scalariform.formatter.preferences.FormattingPreferences
import scalariform.formatter.preferences.IndentSpaces

import java.nio.file.Files
import java.nio.file.Paths
import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.atomic.AtomicInteger

import org.scalafmt.Error.IdempotencyViolated
import org.scalafmt.Error.MegaTestFailed
import org.scalafmt.util.ExperimentResult
import org.scalafmt.util.ExperimentResult.ParseErr
import org.scalafmt.util.ExperimentResult.SearchStateExploded
import org.scalafmt.util.ExperimentResult.Skipped
import org.scalafmt.util.ExperimentResult.Success
import org.scalafmt.util.ExperimentResult.Timeout
import org.scalafmt.util.ExperimentResult.UnknownFailure
import org.scalafmt.util.FormatAssertions
import org.scalafmt.util.LoggerOps
import org.scalafmt.util.ScalaFile
import org.scalafmt.util.ScalaProjectsExperiment
import org.scalafmt.util.ScalacParser
import org.scalatest.FunSuite
import scala.collection.JavaConversions._
import scala.meta._

import org.scalatest.exceptions.TestFailedException

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
    Seq().exists(filename.contains)

  override def runOn(scalaFile: ScalaFile): ExperimentResult = {
    val code = scalaFile.read

    if (!ScalacParser.checkParseFails(code)) {
      val startTime = System.nanoTime()
      Scalafmt.format(code, ScalafmtStyle.default) match {
        case FormatResult.Success(formatted) =>
          val elapsed = System.nanoTime() - startTime
          assertFormatPreservesAst[Source](code, formatted)
          val formattedSecondTime = Scalafmt
            .format(formatted,
                    ScalafmtStyle.default.copy(
                      assumeStandardLibraryStripMargin = false))
            .get
          try {
            assertNoDiff(formattedSecondTime, formatted, "Idempotency")
          } catch {
            case e: DiffFailure =>
              throw IdempotencyViolated(e.diff)
          }
          Success(scalaFile, elapsed)
        case e => e.get; ???
      }
    } else {
      Skipped(scalaFile)
    }
  }

  def scalaFiles = ScalaFile.getAll.filter(okScalaFile)
}

object LinePerMsBenchmark extends FormatExperiment with App {
  case class Result(formatter: String, lineCount: Int, ns: Long) {
    def toCsv: String = s"$formatter, $lineCount, $ns\n"
  }

  val csv = new CopyOnWriteArrayList[Result]()

  def time[T](f: => T): Long = {
    val startTime = System.nanoTime()
    f
    System.nanoTime() - startTime
  }
  val counter = new AtomicInteger()

  scalaFiles.par.foreach { scalaFile =>
    val code = scalaFile.read
    val lineCount = code.lines.length
    Try(Result("scalafmt", lineCount, time(Scalafmt.format(code))))
      .foreach(csv.add)
    Try(Result("scalariform", lineCount, time(ScalaFormatter.format(code))))
      .foreach(csv.add)
    val c = counter.incrementAndGet()
    if (c % 1000 == 0) {
      println(c)
    }
  }

  val csvText = {
    val sb = new StringBuilder
    sb.append(s"Formatter, LOC, ns")
    csv.foreach(x => sb.append(x.toCsv))
    sb.toString()
  }

  Files.write(Paths.get("target", "macro.csv"), csvText.getBytes)
}

object FormatExperimentApp extends FormatExperiment with App {
  def valid(result: ExperimentResult): Boolean = result match {
    case _: Success | _: Timeout | _: Skipped | _: ParseErr |
        _: SearchStateExploded =>
      true
    case UnknownFailure(_, _: IdempotencyViolated) =>
      true // TODO(olafur) remove after #339
    case failure => false
  }

  val onTravis = sys.env.contains("TRAVIS")

  val filesToRun: Seq[ScalaFile] = {
    // running everything on my machine takes <4 minutes but it can take
    // over an hour on Travis. We shuffle since all files should work.
    if (onTravis) Random.shuffle(scalaFiles).take(1000)
    else scalaFiles
  }
  runExperiment(filesToRun)
  printResults()
  val nonValidResults = results.filterNot(valid)
  nonValidResults.foreach(println)
  if (nonValidResults.nonEmpty) {
    throw MegaTestFailed
  }
}
