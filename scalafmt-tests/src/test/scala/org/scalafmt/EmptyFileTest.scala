package org.scalafmt

import java.lang.System.lineSeparator

import munit.FunSuite

class EmptyFileTest extends FunSuite {

  test("empty tree formats to newline") {
    Seq("", lineSeparator, "", s"   $lineSeparator  ").foreach { original =>
      val expected = "\n"
      val obtained = Scalafmt.format(original).get
      assertNoDiff(obtained, expected)
    }
  }
}
