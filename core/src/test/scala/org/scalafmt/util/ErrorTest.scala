package org.scalafmt.util

import org.scalafmt.Formatted
import org.scalafmt.Scalafmt
import org.scalafmt.config.ScalafmtConfig
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
        case _: Formatted.Success => fail("expected failure, got success")
        case _ =>
      }
    }
  }
}
