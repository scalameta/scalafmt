package org.scalafmt

import org.scalafmt.config.LineEndings._
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.DiffAssertions
import org.scalatest.FunSuite

class LineEndingsTest extends FunSuite with DiffAssertions {

  test(
    "code with windows line endings after formatting with line endings preserve setting should have the same endings"
  ) {
    val original = "@ Singleton\r\nobject a {\r\nval y = 2\r\n}"
    val expected = "@Singleton\r\nobject a {\r\n  val y = 2\r\n}\r\n"
    val obtained = Scalafmt
      .format(original, ScalafmtConfig.default.copy(lineEndings = preserve))
      .get
    assertNoDiff(obtained, expected)
  }

  test(
    "code with unix line endings after formatting with line endings preserve setting should have the same endings"
  ) {
    val original = "@ Singleton\nobject a {\nval y = 2\n}"
    val expected = "@Singleton\nobject a {\n  val y = 2\n}\n"
    val obtained = Scalafmt
      .format(original, ScalafmtConfig.default.copy(lineEndings = preserve))
      .get
    assertNoDiff(obtained, expected)
  }

  test(
    "code with windows line endings after formatting with line endings windows setting should have windows endings"
  ) {
    val original = "@ Singleton\r\nobject a {\r\nval y = 2\r\n}"
    val expected = "@Singleton\r\nobject a {\r\n  val y = 2\r\n}\r\n"
    val obtained = Scalafmt
      .format(original, ScalafmtConfig.default.copy(lineEndings = windows))
      .get
    assertNoDiff(obtained, expected)
  }

  test(
    "code with unix line endings after formatting with line endings windows setting should have windows endings"
  ) {
    val original = "@ Singleton\nobject a {\nval y = 2\n}"
    val expected = "@Singleton\r\nobject a {\r\n  val y = 2\r\n}\r\n"
    val obtained = Scalafmt
      .format(original, ScalafmtConfig.default.copy(lineEndings = windows))
      .get
    assertNoDiff(obtained, expected)
  }

  test(
    "code with windows line endings after formatting with line endings unix setting should have unix endings"
  ) {
    val original = "@ Singleton\r\nobject a {\r\nval y = 2\r\n}"
    val expected = "@Singleton\nobject a {\n  val y = 2\n}\n"
    val obtained = Scalafmt
      .format(original, ScalafmtConfig.default.copy(lineEndings = unix))
      .get
    assertNoDiff(obtained, expected)
  }

  test(
    "code with unix line endings after formatting with line endings unix setting should have unix endings"
  ) {
    val original = "@ Singleton\nobject a {\nval y = 2\n}"
    val expected = "@Singleton\nobject a {\n  val y = 2\n}\n"
    val obtained = Scalafmt
      .format(original, ScalafmtConfig.default.copy(lineEndings = unix))
      .get
    assertNoDiff(obtained, expected)
  }
}
