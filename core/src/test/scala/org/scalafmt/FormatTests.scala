package org.scalafmt

import org.scalafmt.internal.Debug
import org.scalafmt.internal.ScalaFmtLogger
import org.scalafmt.internal.State
import org.scalafmt.stats.TestStats
import org.scalafmt.util.DiffTest
import org.scalafmt.util.DiffAssertions
import org.scalafmt.util.FilesUtil
import org.scalafmt.util.FormatOutput
import org.scalafmt.util.FormatAssertions
import org.scalafmt.util.HasTests
import org.scalafmt.util.Report
import org.scalafmt.util.Result
import org.scalafmt.util.Speed
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
import scala.meta.parsers.common.ParseException

// TODO(olafur) property test: same solution without optimization or timeout.

class FormatTests
    extends FunSuite with Timeouts with ScalaFmtLogger with BeforeAndAfterAll
    with HasTests with FormatAssertions with DiffAssertions {
  lazy val onlyUnit = UnitTests.tests.exists(_.only)
  lazy val onlyManual = ManualTests.tests.exists(_.only)
  lazy val onlyOne = tests.exists(_.only)
  lazy val debugResults = mutable.ArrayBuilder.make[Result]

  def ignore(t: DiffTest): Boolean = {
    false
  }

  override val tests = {
    if (onlyManual && !onlyUnit) ManualTests.tests
    else UnitTests.tests
  }

  tests.sortWith(bySpecThenName).withFilter(testShouldRun).foreach(runTest)

  def testShouldRun(t: DiffTest): Boolean = !onlyOne || t.only

  def bySpecThenName(left: DiffTest, right: DiffTest): Boolean = {
    import scala.math.Ordered.orderingToOrdered
    (left.spec, left.name).compare(right.spec -> right.name) < 0
  }

  def runTest(t: DiffTest): Unit = {
    val paddedName = f"${t.fullName}%-70s|"

    if (ignore(t)) {
      // Not even ignore(t), save console space.
    } else if (t.skip) {
      ignore(paddedName) {}
    } else {
      test(paddedName) {
        Debug.newTest()
        filename2parse(t.filename) match {
          case Some(parse) =>
            try {
              val obtained = ScalaFmt.format_!(t.original, t.style)(parse)
              saveResult(t, obtained)
              assertFormatPreservesAst(t.original, obtained)(parse)
              assertNoDiff(obtained, t.expected)
            } catch {
              case e: ParseException =>
                fail("test does not parse" + parseException2Message(
                    e, t.original))
            }

          case None =>
            logger.warn(s"Found no parse for filename ${t.filename}")
        }
      }
    }
  }

  def saveResult(t: DiffTest, obtained: String): Unit = {
    val visitedStates = Debug.exploredInTest
    //    logger.debug(f"$visitedStates%-4s ${t.fullName}")
    val output = getFormatOutput(t.style)
    val obtainedHtml = Report.mkHtml(output, t.style)
    debugResults += Result(t,
                           obtained,
                           obtainedHtml,
                           output,
                           Debug.maxVisitedToken,
                           visitedStates,
                           Debug.elapsedNs)
  }

  def getFormatOutput(style: ScalaStyle): Seq[FormatOutput] = {
    val path = State.reconstructPath(
        Debug.tokens, Debug.state.splits, style, debug = onlyOne)
    val output = path.map {
      case (token, whitespace) =>
        FormatOutput(
            token.left.code, whitespace, Debug.formatTokenExplored(token))
    }
    output
  }

  override def afterAll(configMap: ConfigMap): Unit = {
    val splits =
      Debug.enqueuedSplits.groupBy(_.line.value).toVector.sortBy(-_._2.size)
        .map(x => s"Split(line=${x._1}, count=${x._2.size})").take(3)
    logger.debug(splits.mkString(", "))
    logger.debug(s"Total explored: ${Debug.explored}")
    val results = debugResults.result()
    val stats = TestStats(results)
    // TODO(olafur) don't block printing out test results.
    // I don't want to deal with scalaz's Tasks :'(
    val k = for {
      _ <- Future(
          FilesUtil.writeFile("target/index.html", Report.heatmap(results)))
      _ <- Future(Speed.submitStats(stats)) if !onlyOne
      _ <- Future(Speed.writeComparisonReport(stats, "master"))
    } yield ()
    // Travis exits right after running tests.
    if (sys.env.contains("TRAVIS"))
      Await.ready(k, 20 seconds)
  }
}
