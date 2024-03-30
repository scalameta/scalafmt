package org.scalafmt

import scala.meta._
import org.scalafmt.config._
import munit.FunSuite

class ScalafmtRunnerTest extends FunSuite {
  test("sbt dialect supports trailing commas") {
    ScalafmtRunner.sbt.getDialect(
      """
        |lazy
        |val x = project(
        |  a,
        |
        |
        |  b,
        |)
        """.stripMargin
    ).parse[Source].get
  }
}
