package org.scalafmt

import metaconfig.Conf
import org.scalatest.funsuite.AnyFunSuite
import org.scalafmt.config.Edition
import org.scalafmt.util.DiffAssertions

class EditionTest extends AnyFunSuite with DiffAssertions {
  def check(original: String, expected: Edition, expectedStr: String): Unit = {
    test(original) {
      val conf = Conf.Str(original)
      val obtained = Edition.decoder.read(conf).get
      assertNoDiff(
        Edition.encoder.write(obtained).toString(),
        Edition.encoder.write(expected).toString()
      )
      assertNoDiff(expected.toString, expectedStr)
    }
  }
  def checkActiveFor(less: String, more: String): Unit = {
    test(s"$less <= $more") {
      assert(Edition(less) <= Edition(more))
    }
  }
  def checkError(original: String, expectedError: String): Unit = {
    test(original) {
      Edition.decoder.read(Conf.Str(original)).toEither match {
        case Left(error) =>
          assertNoDiff(error.toString(), expectedError)
        case Right(value) =>
          fail(s"expected error, obtained value $value")
      }
    }
  }
  check("2019-09", Edition(2019, 9), "2019-09")
  check("2019-9", Edition(2019, 9), "2019-09")
  check("2019-10", Edition(2019, 10), "2019-10")
  checkError(
    "2019-invalid",
    """|Type mismatch;
       |  found    : String (value: "2019-invalid")
       |  expected : '$year-$month', for example '2019-08'
       |""".stripMargin.trim
  )
  checkActiveFor("2019-09", "2019-09")
  checkActiveFor("2019-09", "2019-10")
  checkActiveFor("2019-08", "2019-09")
}
