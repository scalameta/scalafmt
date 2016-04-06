package org.scalafmt

import org.scalafmt.ScalaStyle.CustomStyleBecauseIDontLikeTheProvidedStyles
import org.scalafmt.util.DiffAssertions
import org.scalatest.FunSuite

class CommentTest extends FunSuite with DiffAssertions {
  val javadocStyle = ScalafmtConfig.default.copy(scalaDocs = false)

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
}
