package org.scalafmt.util

import org.scalatest.FunSuite
import scala.meta._

import LoggerOps._
import org.scalafmt.ScalafmtRunner
import org.scalafmt.ScalafmtStyle
import org.scalafmt.internal.FormatOps

class StyleMapTest extends FunSuite {
  test("basic") {
    val code =
      """object a {
        |  // scalafmt: { maxColumn = 100 }
        |  println(1)
        |  // scalafmt: { maxColumn = 110 }
        |}
      """.stripMargin.parse[Source].get
    val m = new FormatOps(code, ScalafmtStyle.default, ScalafmtRunner.default)
    assert(
      m.styleMap
        .at(m.tokens.head)
        .maxColumn == ScalafmtStyle.default.maxColumn)
    assert(
      m.styleMap
        .at(m.tokens.find(_.left.syntax == "println").get)
        .maxColumn == 100)
    assert(m.styleMap.at(m.tokens.last).maxColumn == 110)
  }

}
