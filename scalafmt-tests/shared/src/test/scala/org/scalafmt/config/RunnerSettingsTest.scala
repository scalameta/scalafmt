package org.scalafmt.config

import scala.meta._

import munit.FunSuite

class RunnerSettingsTest extends FunSuite {
  test("sbt dialect supports trailing commas")(
    RunnerSettings.sbt.getDialect(
      """|
         |lazy
         |val x = project(
         |  a,
         |
         |
         |  b,
         |)
         |        """.stripMargin,
    ).parse[Source].get,
  )
}
