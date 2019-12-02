package org.scalafmt

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.DiffAssertions
import org.scalatest.funsuite.AnyFunSuite

class RangeTest extends AnyFunSuite with DiffAssertions {
  test("range preserves indent") {
    val original = """object a {
                     |val x = 1
                     |val y = 2
                     |}
      """.stripMargin
    val expected = """object a {
                     |val x = 1
                     |  val y = 2
                     |}
      """.stripMargin
    val obtained = Scalafmt
      .format(
        original,
        ScalafmtConfig.unitTest40,
        range = Set(Range(2, 2).inclusive)
      )
      .get
    assertNoDiff(obtained, expected)
  }
}
