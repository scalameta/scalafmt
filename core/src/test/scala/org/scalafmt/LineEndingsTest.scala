package org.scalafmt

import org.scalafmt.util.DiffAssertions
import org.scalatest.FunSuite

class LineEndingsTest extends FunSuite with DiffAssertions {

  test("code with windows line endings after formatting have the same endings") {
    val original = "@ Singleton\r\nobject a {\r\nval y = 2\r\n}"
    val expected = "@Singleton\r\nobject a {\r\n  val y = 2\r\n}"
    val obtained = Scalafmt.format(original).get
    assertNoDiff(obtained, expected)
  }

  test("code with unix line endings after formatting have the same endings") {
    val original = "@ Singleton\nobject a {\nval y = 2\n}"
    val expected = "@Singleton\nobject a {\n  val y = 2\n}"
    val obtained = Scalafmt.format(original).get
    assertNoDiff(obtained, expected)
  }
}
