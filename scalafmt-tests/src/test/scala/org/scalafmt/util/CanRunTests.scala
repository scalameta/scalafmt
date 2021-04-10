package org.scalafmt.util

import munit.FunSuite
import scala.meta.Tree
import scala.meta.parsers.Parse
import munit.Location
import scala.meta.parsers.ParseException

trait CanRunTests extends FunSuite with HasTests {
  def runTest(run: (DiffTest, Parse[_ <: Tree]) => Unit)(t: DiffTest): Unit = {
    implicit val loc: Location = t.loc
    val filename = loc.path
    val paddedName = f"${t.fullName}%-70s|"

    if (ignore(t)) {
      // Not even ignore(t), save console space.
    } else if (t.skip) {
      test(paddedName.ignore) {}
    } else {
      test(paddedName) {
        filename2parse(filename) match {
          case Some(parse) =>
            try {
              run.apply(t, parse)
            } catch {
              case FormatException(e: ParseException, code) =>
                fail(
                  "test does not parse\n" +
                    parseException2Message(e, code),
                  e
                )
            }
          case None => fail(s"Found no parse for filename $filename")
        }
      }
    }
  }

  def runTestsDefault(): Unit = {
    testsToRun.foreach(runTest(defaultRun))
  }

}
