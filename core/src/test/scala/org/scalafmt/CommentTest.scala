package org.scalafmt

import org.scalafmt.ScalaStyle.CustomStyleBecauseIDontLikeTheProvidedStyles
import org.scalafmt.util.DiffAssertions
import org.scalatest.FunSuite

class CommentTest extends FunSuite with DiffAssertions {
  val javadocStyle = CustomStyleBecauseIDontLikeTheProvidedStyles(
      scalaDocs = false)
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
    val obtained =
      ScalaFmt.format_!(original, javadocStyle)(scala.meta.parseSource)
    assertNoDiff(obtained, expected)
  }
}
