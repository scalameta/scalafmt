package org.scalafmt.util

import scala.meta._

import org.scalafmt.config.Newlines
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.FormatOps
import org.scalatest.funsuite.AnyFunSuite

class StyleMapTest extends AnyFunSuite {
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

  test("align.tokens.add") {
    val code =
      """object a {
        |  // scalafmt: { align.tokens.add = ["="] }
        |  println(1)
        |}
      """.stripMargin.parse[Source].get
    val formatOps = new FormatOps(code, ScalafmtConfig.defaultWithAlign)
    val headToken = formatOps.tokens.head
    val printlnToken = formatOps.tokens.find(_.left.syntax == "println").get

    val defaultEquals = "(Enumerator.Val|Defn.(Va(l|r)|Def|Type))"
    val overrideEquals = ".*"

    val headConfig = formatOps.styleMap.at(headToken)
    val printlnConfig = formatOps.styleMap.at(printlnToken)
    val newFormatOps = new FormatOps(code, printlnConfig)

    val newHeadConfig = newFormatOps.styleMap.at(headToken)
    assert(headConfig.alignMap("=").regex == defaultEquals)
    assert(newHeadConfig.alignMap("=").regex == overrideEquals)

    val newPrintlnConfig = newFormatOps.styleMap.at(printlnToken)
    assert(printlnConfig.alignMap("=").regex == overrideEquals)
    assert(newPrintlnConfig.alignMap("=").regex == overrideEquals)
  }

  test("newlines.implicitParamListModifierForce") {
    val code =
      """object a {
        |  // scalafmt: { newlines.implicitParamListModifierForce = [after] }
        |  println(1)
        |}
      """.stripMargin.parse[Source].get
    val formatOps = new FormatOps(
      code,
      ScalafmtConfig(newlines =
        Newlines(implicitParamListModifierForce = List(Newlines.before))
      )
    )
    val token1 = formatOps.tokens.head
    val token2 = formatOps.tokens.find(_.left.syntax == "println").get

    val defaultValue = List(Newlines.before)
    val overrideValue = List(Newlines.after)

    val style1 = formatOps.styleMap.at(token1)
    val style2 = formatOps.styleMap.at(token2)
    val formatOps2 = new FormatOps(code, style2)

    val newStyle1 = formatOps2.styleMap.at(token1)
    assert(style1.newlines.implicitParamListModifierForce == defaultValue)
    assert(newStyle1.newlines.implicitParamListModifierForce == overrideValue)

    val newStyle2 = formatOps2.styleMap.at(token2)
    assert(style2.newlines.implicitParamListModifierForce == overrideValue)
    assert(newStyle2.newlines.implicitParamListModifierForce == overrideValue)
  }

}
