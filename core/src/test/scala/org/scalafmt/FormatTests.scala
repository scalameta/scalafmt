package org.scalafmt

import scala.language.postfixOps

import org.scalafmt.FormatEvent.CompleteFormat
import org.scalafmt.FormatEvent.Enqueue
import org.scalafmt.FormatEvent.Explored
import org.scalafmt.FormatEvent.VisitToken
import org.scalafmt.stats.TestStats
import org.scalafmt.util.DiffAssertions
import org.scalafmt.util.DiffTest
import org.scalafmt.util.FileOps
import org.scalafmt.util.FormatAssertions
import org.scalafmt.util.HasTests
import org.scalafmt.util.LoggerOps
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
import scala.meta.Tree
import scala.meta.parsers.Parse

// TODO(olafur) property test: same solution without optimization or timeout.

class FormatTests
    extends FunSuite
    with Timeouts
    with BeforeAndAfterAll
    with HasTests
    with FormatAssertions
    with DiffAssertions {
  import LoggerOps._
  lazy val onlyUnit = UnitTests.tests.exists(_.only)
  lazy val onlyManual = !onlyUnit && ManualTests.tests.exists(_.only)
  lazy val onlyOne = tests.exists(_.only)
  lazy val debugResults = mutable.ArrayBuilder.make[Result]

  override def ignore(t: DiffTest): Boolean = false

  override val tests = {
    if (onlyManual) ManualTests.tests
    else UnitTests.tests
  }

  tests
    .sortWith(bySpecThenName)
    .withFilter(testShouldRun)
    .foreach(runTest(run))

  def run(t: DiffTest, parse: Parse[_ <: Tree]): Unit = {
    val runner = scalafmtRunner.withParser(parse)
    val obtained = Scalafmt.format(t.original, t.style, runner) match {
      case FormatResult.Incomplete(code) =>
        code
      case x => x.get
    }
    debugResults += saveResult(t, obtained, onlyOne)
    assertFormatPreservesAst(t.original, obtained)(parse)
    if (!onlyManual) {
      assertNoDiff(obtained, t.expected)
      Debug.newTest()
      val formattedAgain = Scalafmt.format(obtained, t.style, runner).get
//      getFormatOutput(t.style, true) // uncomment to debug
      assertNoDiff(formattedAgain, obtained, "Idempotency violated")
    }
  }

  def testShouldRun(t: DiffTest): Boolean = !onlyOne || t.only

  def bySpecThenName(left: DiffTest, right: DiffTest): Boolean = {
    import scala.math.Ordered.orderingToOrdered
    (left.spec, left.name).compare(right.spec -> right.name) < 0
  }

  override def afterAll(configMap: ConfigMap): Unit = {
    val splits = Debug.enqueuedSplits
      .groupBy(_.line.value)
      .toVector
      .sortBy(-_._2.size)
      .map(x => s"Split(line=${x._1}, count=${x._2.size})")
      .take(3)
    logger.debug(splits.mkString(", "))
    logger.debug(s"Total explored: ${Debug.explored}")
    val results = debugResults.result()
    val stats = TestStats(results)
    // TODO(olafur) don't block printing out test results.
    // I don't want to deal with scalaz's Tasks :'(
    val k = for {
      _ <- Future(
              FileOps.writeFile("target/index.html", Report.heatmap(results)))
    } yield ()
    // Travis exits right after running tests.
    if (sys.env.contains("TRAVIS")) Await.ready(k, 20 seconds)
  }
}
