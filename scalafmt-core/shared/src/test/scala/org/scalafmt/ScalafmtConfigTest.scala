package org.scalafmt

import org.scalatest.funsuite.AnyFunSuite

class ScalafmtConfigTest extends AnyFunSuite {

  test("project.matcher") {
    val config = Scalafmt
      .parseHoconConfig(
        """
          |project.excludeFilters = [
          |  "scalafmt-benchmarks/src/resources"
          |  "/sbt-test/"
          |  "bin/issue"
          |]
      """.stripMargin
      )
      .get
    assert(config.project.matcher.matches("qux/Kazbar.scala"))
    assert(!config.project.matcher.matches("foo/sbt-test/src/main"))
  }

}
