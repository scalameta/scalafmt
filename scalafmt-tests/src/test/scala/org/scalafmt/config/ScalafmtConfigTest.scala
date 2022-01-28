package org.scalafmt.config

import munit.FunSuite

class ScalafmtConfigTest extends FunSuite {

  test("project.matcher") {
    val config = Config
      .fromHoconString(
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

  test("file overrides") {
    val config = Config
      .fromHoconString(
        """
          |newlines.source = fold
          |newlines.topLevelBodyIfMinStatements = [before,after]
          |fileOverride {
          |  "glob:**/src/test/scala/**" {
          |    newlines.source = unfold
          |    newlines.topLevelBodyIfMinStatements = []
          |  }
          |}
      """.stripMargin
      )
      .get
    val nlCfg1 = config.getConfigFor("/x/src/main/scala/foo.scala").get.newlines
    val nlCfg2 = config.getConfigFor("/x/src/test/scala/bar.scala").get.newlines
    assertEquals(nlCfg1.source, Newlines.fold)
    assertEquals(nlCfg2.source, Newlines.unfold)
    assertEquals(
      nlCfg1.topLevelBodyIfMinStatements,
      Seq(Newlines.before, Newlines.after)
    )
    assertEquals(nlCfg2.topLevelBodyIfMinStatements, Seq.empty)
  }

  test("align preset no override") {
    val config = Config
      .fromHoconString("""
        |align = none
        |align.stripMargin = true
      """.stripMargin)
      .get
    // none was ignored
    assertEquals(config.align, Align(stripMargin = true))
  }

  test("align preset with override") {
    val config = Config
      .fromHoconString("""
        |align.preset = none
        |align.stripMargin = true
      """.stripMargin)
      .get
    assertEquals(config.align, Align.none.copy(stripMargin = true))
  }

}
