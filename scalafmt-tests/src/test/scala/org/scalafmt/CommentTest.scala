package org.scalafmt

import org.scalafmt.config.Docstrings
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.DiffAssertions
import org.scalatest.FunSuite

class CommentTest extends FunSuite with DiffAssertions {
  val javadocStyle: ScalafmtConfig =
    ScalafmtConfig.default.copy(docstrings = Docstrings.JavaDoc)
  val scaladocStyle: ScalafmtConfig =
    ScalafmtConfig.default.copy(docstrings = Docstrings.ScalaDoc)
  test("remove trailing space in comments with JavaDoc style") {
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

  test("remove trailing space in comments with ScalaDoc style") {
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
                      |    I have blank lines with JavaStyle, do nothing
                      |
                      |    Please preserve them.
                      |  */
                      |/**
                      |    I have blank lines with ScalaStyle, move line up
                      |
                      |    Please preserve them.
                      | */
                      |val y = 2
                      |}
                   """.stripMargin
    val expected = """object a {
                     |  // inline comment
                     |  /** Y is cool
                     |    */
                     |  /*
                     |   * X is cool
                     |   */
                     |  /*
                     |    I have blank lines with JavaStyle, do nothing
                     |
                     |    Please preserve them.
                     |   */
                     |  /** I have blank lines with ScalaStyle, move line up
                     |
                     |    Please preserve them.
                     |    */
                     |  val y = 2
                     |}
                     |""".stripMargin
    val obtained = Scalafmt.format(original, scaladocStyle).get
    assertNoDiff(obtained, expected)
  }
}
