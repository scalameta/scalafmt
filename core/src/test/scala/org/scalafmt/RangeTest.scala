package org.scalafmt

import org.scalafmt.util.DiffUtil
import org.scalatest.FunSuite

class RangeTest extends FunSuite {
  test("range preserves indent") {
    val original =
      """object a {
        |val x = 1
        |val y = 2
        |}
      """.stripMargin
    val expected =
      """object a {
        |val x = 1
        |  val y = 2
        |}
      """.stripMargin
    val obtained = ScalaFmt.format(original,
      ScalaStyle.UnitTest40, Some(Range(1, 2).inclusive))
    DiffUtil.assertNoDiff(obtained, expected)
  }

}
