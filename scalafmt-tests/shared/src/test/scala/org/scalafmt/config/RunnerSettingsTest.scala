package org.scalafmt
package config

import scala.meta._

class RunnerSettingsTest extends SharedFunSuiteBase {
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
