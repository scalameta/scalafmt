package org.scalafmt

import java.lang.System.lineSeparator

import org.scalatest.funsuite.AnyFunSuite
import org.scalafmt.util.DiffAssertions

class EmptyFileTest extends AnyFunSuite with DiffAssertions {
  test("empty tree formats to newline") {
    Seq("", lineSeparator, "", s"   $lineSeparator  ").foreach { original =>
      val expected = "\n"
      val obtained = Scalafmt.format(original).get
      assertNoDiff(obtained, expected)
    }
  }
}
