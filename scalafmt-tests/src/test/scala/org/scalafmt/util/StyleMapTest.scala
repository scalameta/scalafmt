package org.scalafmt.util

import scala.meta._

import org.scalafmt.config.ScalafmtRunner
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.FormatOps
import org.scalatest.FunSuite

class StyleMapTest extends FunSuite {
  test("basic") {
    val code =
      """object a {
        |  // scalafmt: { maxColumn = 100 }
        |  println(1)
        |  // scalafmt: { maxColumn = 110 }
        |}
      """.stripMargin.parse[Source].get
    val m = new FormatOps(code, ScalafmtConfig.default)
    assert(
      m.styleMap
        .at(m.tokens.head)
        .maxColumn == ScalafmtConfig.default.maxColumn
    )
    assert(
      m.styleMap
        .at(m.tokens.find(_.left.syntax == "println").get)
        .maxColumn == 100
    )
    assert(m.styleMap.at(m.tokens.last).maxColumn == 110)
  }

}
