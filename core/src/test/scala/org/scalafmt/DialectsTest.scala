package org.scalafmt

import org.scalafmt.util.DiffAssertions
import org.scalatest.FunSuite

class DialectsTest extends FunSuite with DiffAssertions {
  val javadocStyle = ScalafmtStyle.default.copy(scalaDocs = false)

  test("sbt") {
    val original =
      """
        |lazy val noPublish = Seq(
        |  publish := {},
        |  publishLocal := {}
        |)
        |
        |lazy val allSettings = commonSettings ++ buildSettings ++ publishSettings
        |
        |lazy val root = project
        |  .in(file("."))
        |  .settings(moduleName := "scalafmt")
        |  .settings(allSettings)
        |  .settings(noPublish)
        |  .aggregate(core, cli, benchmarks, scalafmtSbt, macros, readme)
        |  .dependsOn(core)
        |""".stripMargin
    val expected = original
    val obtained =
      Scalafmt.format(original, runner = ScalafmtRunner.sbt).get
    assertNoDiff(obtained, expected)
  }
}
