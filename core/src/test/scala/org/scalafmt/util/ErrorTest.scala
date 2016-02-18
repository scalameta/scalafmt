package org.scalafmt.util

import org.scalafmt.ScalaFmt
import org.scalafmt.ScalaStyle
import org.scalatest.FunSuite

class ErrorTest extends FunSuite with DiffAssertions {
  test("errors are caught") {
    val nonSourceFile = Seq(
      "class A {",
      "val x = 1",
      "println(1)"
    )
    nonSourceFile.foreach { original =>
      val obtained = ScalaFmt.format(original, ScalaStyle.UnitTest40)
      assertNoDiff(obtained, original)
    }
  }
}
