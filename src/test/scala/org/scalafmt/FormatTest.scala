package org.scalafmt

import java.util.concurrent.TimeUnit

import org.scalafmt.DiffUtil._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.ConfigMap
import org.scalatest.FunSuite
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.SpanSugar._

import scala.collection.mutable
import scala.meta.parsers.common.ParseException

case class DiffTest(spec: String, name: String, original: String, expected: String) {
  val fullName = s"$spec: $name"
}

case class FormatOutput(token: String, whitespace: String, visits: Int)

case class Result(test: DiffTest,
                  obtained: String,
                  obtainedHtml: String,
                  tokens: Seq[FormatOutput],
                  maxVisitsOnSingleToken: Int,
                  visitedStates: Int,
                  // minStates
                  timeNs: Long) {

  def timeMs = TimeUnit.MILLISECONDS.convert(timeNs, TimeUnit.NANOSECONDS)

  def title = f"${test.name} (${timeMs}ms, $visitedStates states)"

  def statesPerMs: Long = visitedStates / timeMs
}

trait FormatTest
  extends FunSuite with Timeouts with ScalaFmtLogger with BeforeAndAfterAll {

  lazy val onlyOne = tests.exists(_.name.startsWith("ONLY"))
  val testDir = "src/test/resources"
  val fmt = new ScalaFmt(style)
  val reports = mutable.ArrayBuilder.make[Result]

  def tests: Seq[DiffTest]

  def style: ScalaStyle = UnitTestStyle

  def assertParses(code: String): Unit = {
    try {
      import scala.meta._
      code.parse[Source]
    } catch {
      case e: ParseException =>
        fail(e)
    }
  }

  def maxVisitedToken: Int = {
    val maxTok = Debug.toks.maxBy(x => Debug.formatTokenExplored(x))
    Debug.formatTokenExplored(maxTok)
  }

  def getFormatOutput: Seq[FormatOutput] = {
    val path = State.reconstructPath(Debug.toks,
      Debug.state.splits, style)
    val output = path.map {
      case (token, whitespace) =>
        FormatOutput(token.left.code,
          whitespace, Debug.formatTokenExplored(token))
    }
    output
  }

  tests.sortWith {
    case (left, right) =>
      import scala.math.Ordered.orderingToOrdered
      if (left.name == "Warmup") true
      else (left.spec, left.name).compare(right.spec -> right.name) < 0
  }.withFilter { t =>
    !t.name.startsWith("SKIP") &&
      (!onlyOne || t.name.startsWith("ONLY"))
  }.foreach {
    case t@DiffTest(spec, name, original, expected) =>
      test(f"${t.fullName}%-50s|") {
        failAfter(10 seconds) {
          Debug.clear()
          val before = Debug.explored
          val timer = Stopwatch()
          val obtained = fmt.format(original)
          assertParses(obtained)
          val visitedStates = Debug.explored - before
          logger.debug(f"$visitedStates%-4s ${t.fullName}")
          val output = getFormatOutput
          val obtainedHtml = Report.mkHtml(output)
          reports += Result(t,
            obtained,
            obtainedHtml,
            output,
            maxVisitedToken,
            visitedStates,
            timer.elapsedNs)
          assert(obtained diff expected)
        }
      }
  }

  override def afterAll(configMap: ConfigMap): Unit = {
    logger.debug(s"Total explored: ${Debug.explored}")
    val result = reports.result()
    Speed.submit(result, suiteName)
  }
}
