package org.scalafmt

import java.lang.System.lineSeparator

import org.scalatest.funsuite.AnyFunSuite

class EmptyFileTest extends AnyFunSuite {
  test("empty tree formats to newline") {
    Seq("", lineSeparator, "", s"   $lineSeparator  ").foreach { original =>
      val expected = lineSeparator
      val obtained = Scalafmt.format(original).get
      assert(obtained == expected)
    }
  }
}
