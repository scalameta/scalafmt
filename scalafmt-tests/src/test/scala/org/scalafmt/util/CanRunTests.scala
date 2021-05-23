package org.scalafmt.util

import munit.FunSuite
import munit.Location
import scala.meta.parsers.ParseException

trait CanRunTests extends FunSuite with HasTests {
  def runTest(run: DiffTest => Unit)(t: DiffTest): Unit = {
    implicit val loc: Location = t.loc
    val paddedName = f"${t.fullName}%-70s|"

    if (ignore(t)) {
      // Not even ignore(t), save console space.
    } else if (t.skip) {
      test(paddedName.ignore) {}
    } else {
      test(paddedName) {
        try {
          run(t)
        } catch {
          case FormatException(e: ParseException, code) =>
            fail(
              "test does not parse\n" +
                parseException2Message(e, code),
              e
            )
        }
      }
    }
  }

  def runTestsDefault(): Unit = {
    testsToRun.foreach(runTest(defaultRun))
  }

}
