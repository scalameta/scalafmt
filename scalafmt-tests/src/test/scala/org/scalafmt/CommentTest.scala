package org.scalafmt

import munit.FunSuite
import org.scalafmt.config.{Docstrings, ScalafmtConfig}

class CommentTest extends FunSuite {

  private val javadocStyle: ScalafmtConfig =
    ScalafmtConfig.default.copy(docstrings =
      ScalafmtConfig.default.docstrings
        .copy(style = Docstrings.Asterisk, wrap = Docstrings.Wrap.no)
    )

  private val wrapStyle: ScalafmtConfig =
    ScalafmtConfig.default.copy(
      maxColumn = 20,
      docstrings = ScalafmtConfig.default.docstrings.copy(
        wrap = Docstrings.Wrap.yes
      )
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

  test("preserve wikidoc headings when docstring wrap is enabled") {
    val original = """
      |/** =Heading1=
      |  *
      |  * I exceed the configured column count in a docstring.
      |  *
      |  * ==Heading2 - Long headings will not get wrapped==
      |  *
      |  * ===Heading3 also not getting wrapped===
      |  */
      |""".stripMargin

    val expected = """
      |/** =Heading1=
      |  *
      |  * I exceed the
      |  * configured
      |  * column count in
      |  * a docstring.
      |  *
      |  * ==Heading2 - Long headings will not get wrapped==
      |  *
      |  * ===Heading3 also not getting wrapped===
      |  */
      |""".stripMargin

    val obtained = Scalafmt.format(original, wrapStyle).get
    assertNoDiff(obtained, expected)
  }
}
