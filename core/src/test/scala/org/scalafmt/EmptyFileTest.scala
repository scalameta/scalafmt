package org.scalafmt

import org.scalafmt.util.DiffAssertions
import org.scalatest.FunSuite

class EmptyFileTest extends FunSuite with DiffAssertions {
  test("empty tree formats to newline") {
    Seq("", "\n", "", "   \n  ").foreach { original =>
      val expected = "\n"
      val obtained = Scalafmt.format(original).get
      assert(obtained == expected)
    }
  }
}
