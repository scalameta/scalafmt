package org.scalafmt

import org.scalafmt.util.DiffAssertions
import org.scalatest.FunSuite

class CommentTest extends FunSuite with DiffAssertions {
  val javadocStyle = ScalafmtStyle.default.copy(scalaDocs = false)

  test("javadoc docstrings are correct") {
    val original = """object a {
                     |/**
                     |   * Y is cool
                     |   */
                     |val y = 2
                     |}
      """.stripMargin
    val expected = """object a {
                     |
                     |  /**
                     |   * Y is cool
                     |   */
                     |  val y = 2
                     |}
      """.stripMargin
    val obtained = Scalafmt.format(original, javadocStyle).get
    assertNoDiff(obtained, expected)
  }
  test("remove trailing space in comments") {
    val trailingSpace = "   "
    val original = s"""object a {
                     |  // inline comment$trailingSpace
                     |/**$trailingSpace
                     |  * Y is cool$trailingSpace
                     |  */
                     |/*$trailingSpace
                     |  * X is cool$trailingSpace
                     |  */
                     |val y = 2
                     |}
                   """.stripMargin
    val expected = """object a {
                     |  // inline comment
                     |  /**
                     |   * Y is cool
                     |   */
                     |  /*
                     |   * X is cool
                     |   */
                     |  val y = 2
                     |}
                   """.stripMargin
    val obtained = Scalafmt.format(original, javadocStyle).get
    assertNoDiff(obtained, expected)
  }
}
