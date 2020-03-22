package org.scalafmt

import scala.meta._
import org.scalafmt.config._
import org.scalatest.funsuite.AnyFunSuite

class ScalafmtRunnerTest extends AnyFunSuite {
  test("sbt dialect supports trailing commas") {
    ScalafmtRunner.sbt
      .dialect(
        """
          |lazy
          |val x = project(
          |  a,
          |
          |
          |  b,
          |)
        """.stripMargin
      )
      .parse[Source]
      .get
  }
}
