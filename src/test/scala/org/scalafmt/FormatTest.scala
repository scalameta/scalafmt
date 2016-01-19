package org.scalafmt

import org.scalafmt.DiffUtil._
import org.scalatest.FunSuite
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.SpanSugar._

case class DiffTest(spec: String, name: String, original: String, expected: String)

trait FormatTest extends FunSuite with Timeouts with ScalaFmtLogger {

  def tests: Seq[DiffTest]

  val testDir = "src/test/resources"

  def style: ScalaStyle = UnitTestStyle

  val fmt = new ScalaFmt(style)

  lazy val onlyOne = tests.exists(_.name.startsWith("ONLY"))

  tests.sortWith {
    case (left, right) =>
      import scala.math.Ordered.orderingToOrdered
      if (left.name == "Warmup") true
      else (left.spec, left.name).compare(right.spec -> right.name) < 0
  }.withFilter { t =>
    !t.name.startsWith("SKIP") &&
      (!onlyOne || t.name.startsWith("ONLY"))
  }.foreach {
    case DiffTest(spec, name, original, expected) =>
      val testName = s"$spec: $name"
      test(f"$testName%-50s|") {
        failAfter(10 seconds) {
          assert(fmt.format(original) diff expected)
        }
      }
  }

}
