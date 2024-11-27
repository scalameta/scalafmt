package org.scalafmt

import org.scalafmt.util.HasTests

import munit.FunSuite

class RangeTest extends FunSuite {
  test("range preserves indent") {
    val original = """|object a {
                      |val x = 1
                      |val y = 2
                      |}
                      |      """
      .stripMargin
    val expected = """|object a {
                      |val x = 1
                      |  val y = 2
                      |}
                      |      """
      .stripMargin
    val obtained = Scalafmt
      .format(original, HasTests.unitTest40, range = Set(Range(2, 2).inclusive))
      .get
    assertNoDiff(obtained, expected)
  }
}
