package org.scalafmt

import org.scalatest.FunSuite

class RangeTest extends FunSuite {

  test("range preserves indent") {
    val original =
      """{
        |val x = 1
        |val y = 2
        |}
      """.stripMargin
    val expected =
      """{
        |val x = 1
        |  val y = 2
        |}
      """.stripMargin
    val obtained = ScalaFmt.format(original, Standard, Some(Range(2, 2)))
    DiffUtil.assertNoDiff(obtained, expected)
  }

}
