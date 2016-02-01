package org.scalafmt

import java.util.concurrent.TimeUnit

import org.scalafmt.DiffUtil._
import org.scalafmt.stats.TestStats
import org.scalatest.BeforeAndAfterAll
import org.scalatest.ConfigMap
import org.scalatest.FunSuite
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.SpanSugar._

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.postfixOps
import scala.meta.Tree
import scala.meta.parsers.common.Parse
import scala.meta.parsers.common.ParseException
import scala.util.Try

case class DiffTest(spec: String,
                    name: String,
                    filename: String,
                    original: String,
                    expected: String,
                    skip: Boolean,
                    only: Boolean,
                    style: ScalaStyle) {
  val fullName = s"$spec: $name"
}

case class FormatOutput(token: String, whitespace: String, visits: Int)

case class Result(test: DiffTest,
                  obtained: String,
                  obtainedHtml: String,
                  tokens: Seq[FormatOutput],
                  maxVisitsOnSingleToken: Int,
                  visitedStates: Int,
                  timeNs: Long) {

  def title = f"${test.name} (${timeMs}ms, $visitedStates states)"

  def statesPerMs: Long = visitedStates / timeMs

  def timeMs = TimeUnit.MILLISECONDS.convert(timeNs, TimeUnit.NANOSECONDS)
}

trait HasTests {

  val testDir = "src/test/resources"

  def isOnly(name: String) = name.startsWith("ONLY ")

  def isSkip(name: String) = name.startsWith("SKIP ")

  def stripPrefix(name: String) =
    name.stripPrefix("SKIP ").stripPrefix("ONLY ").trim

  def filename2parse(filename: String): Option[Parse[_ <: Tree]] =
    extension(filename) match {
      case "source" | "scala" => Some(scala.meta.parsers.parseSource)
      case "stat" => Some(scala.meta.parsers.parseStat)
      case _ => None
    }

  def extension(filename: String): String = filename.replaceAll(".*\\.", "")

  def tests: Seq[DiffTest]
}

class FormatTest
  extends FunSuite with Timeouts with ScalaFmtLogger
  with BeforeAndAfterAll with HasTests {

  lazy val onlyOne = tests.exists(_.only)

  lazy val debugResults = mutable.ArrayBuilder.make[Result]

  override val tests = UnitTests.tests ++ ManualTests.tests

  tests
    .sortWith(bySpecThenName)
    .withFilter(testShouldRun)
    .foreach(runTest)

  def testShouldRun(t: DiffTest): Boolean = !onlyOne || t.only

  def bySpecThenName(left: DiffTest, right: DiffTest): Boolean = {
    import scala.math.Ordered.orderingToOrdered
    (left.spec, left.name).compare(right.spec -> right.name) < 0
  }

  def runTest(t: DiffTest): Unit = {
    val paddedName = f"${t.fullName}%-70s|"
    if (t.skip) {
      ignore(paddedName) {}
    }
    else {
      val fmt = new ScalaFmt(t.style)
      test(paddedName) {
        Debug.newTest()
        failAfter(10 seconds) {
          filename2parse(t.filename) match {
            case Some(parse) =>
              val obtained = fmt.format(t.original)(parse)
              saveResult(t, obtained)
              assertParses(obtained, t.original)(parse)
              assertNoDiff(obtained, t.expected)
            case None =>
              logger.warn(s"Found no parse for filename ${t.filename}")
          }
        }
      }
    }
  }

  def saveResult(t: DiffTest, obtained: String): Unit = {
    val visitedStates = Debug.exploredInTest
    logger.debug(f"$visitedStates%-4s ${t.fullName}")
    val output = getFormatOutput(t.style)
    val obtainedHtml = Report.mkHtml(output)
    debugResults += Result(t,
      obtained,
      obtainedHtml,
      output,
      Debug.maxVisitedToken,
      visitedStates,
      Debug.timer.elapsedNs)
  }

  def getFormatOutput(style: ScalaStyle): Seq[FormatOutput] = {
    val path = State.reconstructPath(Debug.tokens,
      Debug.state.splits, style)
    val output = path.map {
      case (token, whitespace) =>
        FormatOutput(token.left.code,
          whitespace, Debug.formatTokenExplored(token))
    }
    output
  }

  def parses[T <: Tree](code: String)(implicit parse: Parse[T]): Boolean = {
    import scala.meta._
    Try(code.parse[T]).map(_ => true).getOrElse(false)
  }

  def assertParses[T <: Tree](obtained: String,
                              original: String)(implicit parse: Parse[T]): Unit = {
    if (!parses(obtained) && parses(original)) {
      fail(
        s"""Formatter output does not parse!
            |${header("Obtained")}
            |$obtained
            |
            |${header("Original")}
            |$original
           """.stripMargin)
    }
  }

  override def afterAll(configMap: ConfigMap): Unit = {
    logger.debug(s"Total explored: ${Debug.explored}")
    val results = debugResults.result()
    val stats = TestStats(results)
    // TODO(olafur) don't block printing out test results.
    // I don't want to deal with scalaz's Tasks :'(
    val k = for {
      _ <- Future(Speed.submitStats(stats))
      _ <- Future(Speed.writeComparisonReport(stats, "master"))
      _ <- Future(FilesUtil.writeFile("target/index.html", Report.heatmap(results)))
    } yield ()
    // Travis can take more than 10 seconds.
    Await.ready(k, 10 seconds)
  }
}
