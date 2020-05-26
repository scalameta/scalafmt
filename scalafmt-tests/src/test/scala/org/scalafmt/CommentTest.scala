package org.scalafmt

import org.scalatest.funsuite.AnyFunSuite
import org.scalafmt.config.{Docstrings, ScalafmtConfig}
import org.scalafmt.util.DiffAssertions

class CommentTest extends AnyFunSuite with DiffAssertions {

  private val javadocStyle: ScalafmtConfig =
    ScalafmtConfig.default.copy(docstrings =
      ScalafmtConfig.default.docstrings.copy(style = Some(Docstrings.Asterisk))
    )

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

  test("remove trailing tabs in comments") {
    val trailingSpace = "\t \t"
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

  test("remove various trailing Unicode whitespace in comments") {
    val trailingSpace = "   　" // U+00A0, U+2000, U+1680, U+3000
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
