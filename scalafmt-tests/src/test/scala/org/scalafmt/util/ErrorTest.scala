package org.scalafmt.util

import org.scalafmt.Formatted
import org.scalafmt.Scalafmt
import munit.FunSuite

class ErrorTest extends FunSuite {
  test("errors are caught") {
    val nonSourceFile = Seq(
      "class A {",
      "val x = 1",
      "println(1)"
    )
    nonSourceFile.foreach { original =>
      Scalafmt.format(original, HasTests.unitTest40) match {
        case _: Formatted.Success => fail("expected failure, got success")
        case _ =>
      }
    }
  }
}
