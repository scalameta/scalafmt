package org.scalafmt.util

import org.scalatest.funsuite.AnyFunSuite
import org.scalafmt.Debug

import scala.meta.Tree
import scala.meta.parsers.{Parse, ParseException}

trait CanRunTests extends AnyFunSuite with HasTests {
  def runTest(run: (DiffTest, Parse[_ <: Tree]) => Unit)(t: DiffTest): Unit = {
    val paddedName = f"${t.fullName}%-70s|"

    if (ignore(t)) {
      // Not even ignore(t), save console space.
    } else if (t.skip) {
      ignore(paddedName) {}
    } else {
      test(paddedName) {
        Debug.newTest()
        filename2parse(t.filename) match {
          case Some(parse) =>
            try {
              run.apply(t, parse)
            } catch {
              case e: ParseException =>
                fail(
                  "test does not parse" +
                    parseException2Message(e, t.original)
                )
            }
          case None => fail(s"Found no parse for filename ${t.filename}")
        }
      }
    }
  }

  def runTestsDefault(): Unit = {
    testsToRun.foreach(runTest(defaultRun))
  }

}
