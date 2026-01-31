package org.scalafmt.config

import scala.meta.Dialect
import scala.meta.dialects.Scala213

import metaconfig.{ConfError, Configured}
import munit.FunSuite

class ConfigDialectOverrideTest extends FunSuite {
  private val generatedMap = DialectMacro.dialectMap

  test("throws on an non-existent setting") {
    val res =
      try ScalafmtConfig.fromHoconString(
          """|
             |runner.dialectOverride.fooBar = true
             |runner.dialect = scala213
             |""".stripMargin,
        )
      catch { case ex: Throwable => Configured.NotOk(ConfError.exception(ex)) }
    res match {
      case Configured.NotOk(err: ConfError) =>
        assert(err.msg.contains("key not found: withFooBar"), err.msg)
      case _ => fail("should have failed")
    }
  }

  def testBooleanFlag(
      methodName: String,
      getter: Dialect => Boolean,
      testDirectly: Boolean,
  ): Unit = {
    def makeBooleanConfig(setting: String, value: Boolean) = ScalafmtConfig
      .fromHoconString(
        s"""|
            |runner.dialectOverride.$setting = $value
            |runner.dialect = scala213
            |""".stripMargin,
      ).get
    Seq(true, false).foreach(flag =>
      test(s"boolean flag: $methodName($flag)") {
        if (testDirectly)
          assertEquals(getter(generatedMap(methodName)(Scala213, flag)), flag)
        assertEquals(
          getter(RunnerSettings.overrideDialect(Scala213, methodName, flag)),
          flag,
        )
        assertEquals(
          getter(makeBooleanConfig(methodName, flag).runner.getDialect),
          flag,
        )
      },
    )
  }

  testBooleanFlag("allowFewerBraces", _.allowFewerBraces, testDirectly = false)
  testBooleanFlag(
    "withAllowFewerBraces",
    _.allowFewerBraces,
    testDirectly = true,
  )
  testBooleanFlag(
    "useInfixTypePrecedence",
    _.useInfixTypePrecedence,
    testDirectly = false,
  )
  testBooleanFlag(
    "withUseInfixTypePrecedence",
    _.useInfixTypePrecedence,
    testDirectly = true,
  )
  testBooleanFlag(
    "allowImplicitByNameParameters",
    _.allowImplicitByNameParameters,
    testDirectly = false,
  )
  testBooleanFlag(
    "withAllowImplicitByNameParameters",
    _.allowImplicitByNameParameters,
    testDirectly = true,
  )
  testBooleanFlag(
    "allowSignificantIndentation",
    _.allowSignificantIndentation,
    testDirectly = false,
  )
  testBooleanFlag(
    "withAllowSignificantIndentation",
    _.allowSignificantIndentation,
    testDirectly = true,
  )

  test("applying generated boolean map elements does not result in errors") {
    val omittedMethods = Set(
      "withToplevelSeparator", // non-boolean
      "withAllowMultilinePrograms", // unimplemented in scalameta (???)
      "withAllowTermUnquotes", // unimplemented in scalameta (???)
      "withAllowPatUnquotes", // unimplemented in scalameta (???)
    )
    val baseDialect = Dialect.empty
    generatedMap.keys.filter(!omittedMethods.contains(_))
      .foreach(key => generatedMap(key)(baseDialect, true))
  }
}
