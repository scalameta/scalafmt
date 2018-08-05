package org.scalafmt

class ScalafmtConfigTest extends org.scalatest.FunSuite {

  test("project.matcher") {
    val config = Scalafmt
      .parseHoconConfig(
        """
          |project.excludeFilters = [
          |  "scalafmt-benchmarks/src/resources"
          |  "sbt-test"
          |  "bin/issue"
          |]
      """.stripMargin
      )
      .get
    assert(config.project.matcher.matches("qux/Kazbar.scala"))
    assert(!config.project.matcher.matches("sbt-test/src/main"))
  }

}
