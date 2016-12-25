package org.scalafmt.cli

import scala.meta.dialects.Paradise211
import scala.meta.parsers.Parse

import org.scalafmt
import org.scalafmt.config.AlignToken
import org.scalafmt.config.ImportSelectors
import org.scalafmt.config.IndentOperator
import org.scalafmt.config.ScalafmtConfig
import org.scalatest.FunSuite

class CliOptionsTest extends FunSuite {

  test("style = ...") {
    import org.scalafmt.config.Config
    val Left(err) = Config.fromHocon("style = foobar")
    assert(
      "Unknown style name foobar. Expected one of: Scala.js, IntelliJ, default, defaultWithAlign" == err.getMessage)

    val overrideOne = Config.fromHocon("""|style = defaultWithAlign
                                          |maxColumn = 100
                                          |""".stripMargin)
    assert(
      Right(ScalafmtConfig.defaultWithAlign.copy(maxColumn = 100)) == overrideOne)
    assert(
      Right(ScalafmtConfig.intellij) == Config.fromHocon("style = intellij"))
    assert(
      Right(ScalafmtConfig.scalaJs) == Config.fromHocon("style = Scala.js"))
    assert(
      Right(ScalafmtConfig.defaultWithAlign) == Config.fromHocon(
        "style = defaultWithAlign"))
  }

  test("hocon2class") {
    val config =
      """
        |style = intellij
        |assumeStandardLibraryStripMargin = true
        |importSelectors = binPack
        |danglingParentheses = true
        |optIn: {
        |  configStyleArguments = true
        |  breakChainOnFirstMethodDot = true
        |  noNewlineAfterAnnotation = true
        |}
        |maxColumn = 4000
        |poorMansTrailingCommasInConfigStyle = true
        |unindentTopLevelOperators = true
        |docstrings = JavaDoc
        |binPack: {
        |  defnSite = true
        |  callSite = true
        |  parentConstructors = true
        |}
        |continuationIndent: {
        |  callSite = 3
        |  defnSite = 5
        |}
        |spaces: {
        |  inImportCurlyBraces = true
        |  afterTripleEquals = true
        |  beforeContextBoundColon = true
        |}
        |rewriteTokens: {
        |  "⇒" = "=>"
        |  "true" = "false"
        |}
        |align: {
        |  tokens = [
        |    {code: "=>", owner: "Function"},
        |    {code: "//"},
        |    "%%",
        |    "%"
        |  ]
        |  arrowEnumeratorGenerator = true
        |  ifWhileOpenParen = true
        |  openParenCallSite = true
        |  openParenDefnSite = true
        |  mixedOwners = true
        |}
        |newlines: {
        |  alwaysBeforeCurlyBraceLambdaParams = true
        |  neverBeforeJsNative = true
        |  sometimesBeforeColonInMethodReturnType = true
        |}
        |indentOperator: {
        |  "include" = inc
        |  exclude = exclude
        |}
        |
        |runner: {
        |  optimizer.acceptOptimalAtHints = false
        |  parser = parseCase
        |  dialect = Paradise211
        |  eventCallback = bar
        |}
      """.stripMargin
    scalafmt.config.Config.fromHocon(config) match {
      case Left(e) => throw e
      case Right(obtained) =>
        assert(obtained.maxColumn == 4000)
        assert(
          obtained.rewriteTokens == Map(
            "⇒" -> "=>",
            "true" -> "false"
          ))
        assert(obtained.runner.parser == Parse.parseCase)
        assert(obtained.runner.dialect == Paradise211)
        assert(!obtained.runner.optimizer.acceptOptimalAtHints)
        assert(obtained.assumeStandardLibraryStripMargin)
        assert(obtained.reformatDocstrings)
        assert(!obtained.scalaDocs)
        assert(obtained.binPack.callSite)
        assert(obtained.binPack.defnSite)
        assert(obtained.configStyleArguments)
        assert(obtained.neverBeforeJsNative)
        assert(obtained.danglingParentheses)
        assert(obtained.align.openParenCallSite)
        assert(obtained.align.openParenDefnSite)
        assert(obtained.continuationIndent.callSite == 3)
        assert(obtained.continuationIndent.defnSite == 5)
        assert(obtained.align.mixedOwners)
        assert(obtained.importSelectors == ImportSelectors.binPack)
        assert(obtained.spaces.inImportCurlyBraces)
        assert(obtained.poorMansTrailingCommasInConfigStyle)
        assert(obtained.sometimesBeforeColonInMethodReturnType)
        assert(obtained.binPackParentConstructors)
        assert(obtained.spaces.afterTripleEquals)
        assert(obtained.unindentTopLevelOperators)
        assert(obtained.align.arrowEnumeratorGenerator)
        assert(obtained.align.ifWhileOpenParen)
        assert(obtained.spaces.beforeContextBoundColon)
        assert(obtained.breakChainOnFirstMethodDot)
        assert(obtained.noNewlineAfterAnnotation)
        assert(obtained.alwaysBeforeCurlyBraceLambdaParams)
        assert(
          obtained.align.tokens ==
            Set(
              AlignToken("//", ".*"),
              AlignToken("=>", "Function"),
              AlignToken("%%", ".*"),
              AlignToken("%", ".*")
            ))
        assert(obtained.indentOperator == IndentOperator("inc", "exclude"))
    }
  }

}
