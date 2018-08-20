package website

import org.scalatest.FunSuite
import scala.meta.testkit._

class WebsiteSuite extends FunSuite with DiffAssertions {
  def check(name: String, original: String, expected: String): Unit = {
    test(name) {
      val obtained = website.preProcess(original)
      assertNoDiff(obtained, expected.replaceAllLiterally("'''", "\"\"\""))
    }
  }

  check(
    "basic",
    """
      '''|
      #
      # a
    """,
    """
  '''|
        |
        | a
"""
  )
}
