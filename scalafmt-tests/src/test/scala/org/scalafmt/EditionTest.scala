package org.scalafmt

import org.scalatest.FunSuite
import org.scalafmt.util.DiffAssertions
import org.scalafmt.config.Edition
import metaconfig.Conf

class EditionTest extends FunSuite with DiffAssertions {
  def check(original: String, expected: Edition): Unit = {
    test(original) {
      val conf = Conf.Str(original)
      val obtained = Edition.decoder.read(conf).get
      assertNoDiff(
        Edition.encoder.write(obtained).toString(),
        Edition.encoder.write(expected).toString()
      )
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
  check("2019-09", Edition(2019, 9))
  check("2019-9", Edition(2019, 9))
  check("2019-10", Edition(2019, 10))
  checkError(
    "2019-invalid",
    """|Type mismatch;
       |  found    : String (value: "2019-invalid")
       |  expected : '$year-$month', for example '2019-08'
       |""".stripMargin.trim
  )
}
