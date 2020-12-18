package org.scalafmt

import org.scalafmt.config.Align
import org.scalafmt.config.Newlines
import munit.FunSuite

class ScalafmtConfigTest extends FunSuite {

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

  test("file overrides") {
    val config = Scalafmt
      .parseHoconConfig(
        """
          |newlines.source = fold
          |newlines.topLevelStatements = [before,after]
          |fileOverride {
          |  "glob:**/src/test/scala/**" {
          |    newlines.source = unfold
          |    newlines.topLevelStatements = []
          |  }
          |}
      """.stripMargin
      )
      .get
    val nlCfg1 = config.getConfigFor("/x/src/main/scala/foo.scala").newlines
    val nlCfg2 = config.getConfigFor("/x/src/test/scala/bar.scala").newlines
    assert(nlCfg1.source == Newlines.fold)
    assert(nlCfg2.source == Newlines.unfold)
    assert(nlCfg1.topLevelStatements == Seq(Newlines.before, Newlines.after))
    assert(nlCfg2.topLevelStatements == Seq.empty)
  }

  test("align preset no override") {
    val config = Scalafmt
      .parseHoconConfig("""
        |align = none
        |align.stripMargin = true
      """.stripMargin)
      .get
    // none was ignored
    assert(config.align == Align(stripMargin = true))
  }

  test("align preset with override") {
    val config = Scalafmt
      .parseHoconConfig("""
        |align.preset = none
        |align.stripMargin = true
      """.stripMargin)
      .get
    assert(config.align == Align.none.copy(stripMargin = true))
  }

}
