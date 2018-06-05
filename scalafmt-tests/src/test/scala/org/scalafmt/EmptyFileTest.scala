package org.scalafmt

import org.scalafmt.util.DiffAssertions
import java.lang.System.lineSeparator
import org.scalatest.FunSuite

class EmptyFileTest extends FunSuite with DiffAssertions {
  test("empty tree formats to newline") {
    Seq("", lineSeparator, "", s"   $lineSeparator  ").foreach { original =>
      val expected = lineSeparator
      val obtained = Scalafmt.format(original).get
      assert(obtained == expected)
    }
  }
}
