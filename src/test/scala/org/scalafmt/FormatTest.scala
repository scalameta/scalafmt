package org.scalafmt

import org.scalafmt.DiffUtil._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.ConfigMap
import org.scalatest.FunSuite
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.SpanSugar._

import scala.collection.mutable
import scala.meta.parsers.common.ParseException

case class DiffTest(spec: String, name: String, original: String, expected: String)

case class Result(test: DiffTest,
                  obtained: String,
                  obtainedHtml: String,
                  redness: Int,
                  time: Long)

trait FormatTest
  extends FunSuite with Timeouts with ScalaFmtLogger with BeforeAndAfterAll {

  def tests: Seq[DiffTest]

  val testDir = "src/test/resources"

  def style: ScalaStyle = UnitTestStyle

  val fmt = new ScalaFmt(style)

  lazy val onlyOne = tests.exists(_.name.startsWith("ONLY"))
  val reports = mutable.ArrayBuilder.make[Result]

  def red(token: FormatToken): Int = {
    val max = 10
    val i = Math.min(max, Debug.formatTokenExplored(token))
    val k = (i.toDouble / max.toDouble * 256).toInt
    Math.min(256, 270 - k)
  }

  def assertParses(code: String): Unit = {
    try {
      import scala.meta._
      code.parse[Source]
    } catch {
      case e: ParseException =>
        fail(e)
    }
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
      val testName = s"$spec: $name"
      test(f"$testName%-50s|") {
        failAfter(10 seconds) {
          Debug.clear()
          val before = Debug.explored
          val start = System.currentTimeMillis()
          val obtained = fmt.format(original)
          assertParses(obtained)
          logger.debug(f"${Debug.explored - before}%-4s $testName")
          var maxTok = 0
          val obtainedHtml =
            Debug.state.reconstructPath(Debug.toks, style, { tok =>
              import scalatags.Text.all._
              val color = red(tok)
              maxTok = Math.max(Debug.formatTokenExplored(tok), maxTok)
              span(background := s"rgb(256, $color, $color)", tok.left.code).render
            })
          reports += Result(t,
            obtained,
            obtainedHtml,
            maxTok,
            System.currentTimeMillis() - start)
          assert(obtained diff expected)
        }
      }
  }

  override def afterAll(configMap: ConfigMap): Unit = {
    logger.debug(s"Total explored: ${Debug.explored}")
    val report = Report.generate(reports.result())
    val filename = "target/index.html"
    val alternativeFilename = s"target/$suiteName.html"
    FilesUtil.writeFile(filename, report)
    FilesUtil.writeFile(alternativeFilename, report)
  }
}
