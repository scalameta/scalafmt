package org.scalafmt

import org.scalafmt.util.DiffAssertions
import org.scalatest.FunSuite

class RangeTest extends FunSuite with DiffAssertions {
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
    val obtained =
      ScalaFmt.format_!(original,
                        ScalaStyle.UnitTest40,
                        Set(Range(2, 2).inclusive))(scala.meta.parseSource)
    assertNoDiff(obtained, expected)
  }
}
