package org.scalafmt

import org.scalafmt.config.Docstrings
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.DiffAssertions
import org.scalatest.FunSuite

class CommentTest extends FunSuite with DiffAssertions {
  val javadocStyle: ScalafmtConfig =
    ScalafmtConfig.default.copy(docstrings = Docstrings.JavaDoc)
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
                      |/*
                      |    I have blank lines.
                      |
                      |    Please preserve them.
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
                     |  /*
                     |    I have blank lines.
                     |
                     |    Please preserve them.
                     |   */
                     |  val y = 2
                     |}
                     |""".stripMargin
    val obtained = Scalafmt.format(original, javadocStyle).get
    assertNoDiff(obtained, expected)
  }
}
