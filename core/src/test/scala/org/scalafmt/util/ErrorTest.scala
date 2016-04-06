package org.scalafmt.util

import org.scalafmt.FormatResult
import org.scalafmt.Scalafmt
import org.scalafmt.ScalafmtConfig
import org.scalatest.FunSuite

class ErrorTest extends FunSuite with DiffAssertions {
  test("errors are caught") {
    val nonSourceFile = Seq(
        "class A {",
        "val x = 1",
        "println(1)"
    )
    nonSourceFile.foreach { original =>
      Scalafmt.format(original, ScalafmtConfig.unitTest40) match {
        case _: FormatResult.Success => fail("expected failure, got success")
        case _ =>
      }
    }
  }
}
