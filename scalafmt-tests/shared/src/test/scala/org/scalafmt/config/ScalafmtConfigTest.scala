package org.scalafmt.config

import metaconfig.{Conf, Configured}
import munit.FunSuite

class ScalafmtConfigTest extends FunSuite {

  test("project.matcher") {
    val config = ScalafmtConfig.fromHoconString(
      """|
         |project.excludeFilters = [
         |  "scalafmt-benchmarks/src/resources"
         |  "/sbt-test/"
         |  "bin/issue"
         |]
         |      """.stripMargin,
    ).get
    assert(config.project.matcher.matches("qux/Kazbar.scala"))
    assert(!config.project.matcher.matches("foo/sbt-test/src/main"))
  }

  test("file overrides") {
    val config = ScalafmtConfig.fromHoconString {
      """
        |newlines.source = fold
        |newlines.topLevelBodyIfMinStatements = [before,after]
        |fileOverride {
        |  "glob:**/src/test/scala/**" {
        |    newlines.source = unfold
        |    newlines.topLevelBodyIfMinStatements = []
        |  }
        |}
        |      """.stripMargin
    }.get
    val nlCfg1 = config.getConfigFor("/x/src/main/scala/foo.scala").get.newlines
    val nlCfg2 = config.getConfigFor("/x/src/test/scala/bar.scala").get.newlines
    assertEquals(nlCfg1.source, Newlines.fold)
    assertEquals(nlCfg2.source, Newlines.unfold)
    assertEquals(
      nlCfg1.topLevelBodyIfMinStatements,
      Seq(Newlines.before, Newlines.after),
    )
    assertEquals(nlCfg2.topLevelBodyIfMinStatements, Seq.empty)
  }

  test("align preset no override") {
    val config = ScalafmtConfig.fromHoconString(
      """|
         |align = none
         |align.stripMargin = true
         |      """.stripMargin,
    ).get
    // none was ignored
    assertEquals(config.align, Align(stripMargin = true))
  }

  test("align preset with override") {
    val config = ScalafmtConfig.fromHoconString(
      """
        |align.preset = none
        |align.stripMargin = true
      """.stripMargin,
    ).get
    assertEquals(config.align, Align.none.copy(stripMargin = true))
  }

  test("dialect override") {
    val config1 = ScalafmtConfig.fromHoconString(
      """|
         |runner.dialect = scala213
         |""".stripMargin,
    ).get
    assert(!config1.runner.getDialect.allowToplevelTerms)
    val config2 = ScalafmtConfig.fromHoconString(
      """|
         |runner.dialectOverride.allowToplevelTerms = true
         |runner.dialect = scala213
         |""".stripMargin,
    ).get
    assert(config2.runner.getDialect.allowToplevelTerms)
  }

  test("hasRewriteRules-and-withoutRewriteRules trailingCommas") {
    val config1 = ScalafmtConfig.fromHoconString(
      """|
         |runner.dialect = scala213
         |rewrite.trailingCommas = never
         |""".stripMargin,
    ).get
    assert(config1.hasRewrites)
    val config2 = config1.withoutRewrites
    assert(!config2.hasRewrites)
  }

  test("hasRewriteRules-and-withoutRewriteRules docstrings") {
    val config1 = ScalafmtConfig.fromHoconString(
      """|
         |runner.dialect = scala213
         |rewrite.trailingCommas = keep
         |docstrings.removeEmpty = true
         |""".stripMargin,
    ).get
    assert(config1.hasRewrites)
    val config2 = config1.withoutRewrites
    assert(!config2.hasRewrites)
  }

  test("convert") {
    ConfParsed.fromString {
      """|binPack.unsafeCallSite = false # callSite = never
         |indentOperator.topLevelOnly = true # exemptScope = oldTopLevel
         |newlines.alwaysBeforeMultilineDef = true # forceBeforeMultilineAssign = def
         |importSelectors = false # binPack.importSelectors = unfold
         |fileOverride {
         |  "pat1": "scala213"
         |  "pat2" {
         |    binPack.unsafeCallSite = true # callSite = always
         |  }
         |  "pat3" {
         |    importSelectors = true # binPack.importSelectors = fold
         |  }
         |}
         |""".stripMargin
    }.conf match {
      case Configured.Ok(value) => assertEquals(
          Conf.printHocon(ScalafmtConfig.decoder.convert(value)),
          """|binPack.callSite = Never
             |binPack.importSelectors = unfold
             |fileOverride.pat1 = "scala213"
             |fileOverride.pat2.binPack.callSite = Always
             |fileOverride.pat3.binPack.importSelectors = fold
             |indent.infix = [
             |  {
             |    exemptScope = [
             |      oldTopLevel
             |    ]
             |  }
             |]
             |newlines.forceBeforeMultilineAssign = def""".stripMargin,
        )
      case Configured.NotOk(err) => fail(err.msg)
    }
  }

}
