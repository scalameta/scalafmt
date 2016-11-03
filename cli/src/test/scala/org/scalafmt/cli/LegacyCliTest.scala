package org.scalafmt.cli

import org.scalafmt.config
import org.scalafmt.util.DiffAssertions
import org.scalatest.FunSuite

class LegacyCliTest extends FunSuite with DiffAssertions {
  test("migrate") {
    val result = LegacyCli.migrate(
      """
        |--indentOperatorsIncludeFilter a
        |--indentOperatorsExcludeFilter b
        |--maxColumn 100 # comment
        |--alignTokens %;Infix,%%;Infix
        |--alignTokens "a;b,c;d,e;.*" # comment
        |--reformatComments false
        |--scalaDocs false
        |--scalaDocs true
        |--alignStripMarginStrings true
        |--binPackArguments true
        |--binPackParameters true
        |--binPackParentConstructors true
        |--configStyleArguments false
        |--noNewlinesBeforeJsNative false
        |--allowNewlineBeforeColonInMassiveReturnTypes true
        |--alignByOpenParenCallSite false
        |--alignByOpenParenDefnSite false
        |--continuationIndentCallSite 3
        |--continuationIndentDefnSite 3
        |--alignMixedOwners false
        |--binPackImportSelectors true
        |--spacesInImportCurlyBraces true
        |--spaceAfterTripleEquals true
        |--spaceBeforeContextBoundColon true
        |--unindentTopLevelOperators false
        |--bestEffortInDeeplyNestedCode
        |--rewriteTokens ⇒;=>,←;<-
      """.stripMargin
    )
    val expected =
      """
        |indentOperator.include = a
        |indentOperator.exclude = b
        |maxColumn = 100 # comment
        |align.tokens = [
        |  { code = "%", owner = "Infix" }
        |  { code = "%%", owner = "Infix" }
        |]
        |align.tokens = [ # comment
        |  { code = "a", owner = "b" }
        |  { code = "c", owner = "d" }
        |  "e"
        |]
        |docstrings = preserve
        |docstrings = JavaDoc
        |docstrings = ScalaDoc
        |assumeStandardLibraryStripMargin = true
        |binPack.callSite = true
        |binPack.defnSite = true
        |binPack.parentConstructors = true
        |optIn.configStyleArguments = false
        |newlines.neverBeforeJsNative = false
        |newlines.sometimesBeforeColonInMethodReturnType = true
        |align.openParenCallSite = false
        |align.openParenDefnSite = false
        |continuationIndent.callSite = 3
        |continuationIndent.defnSite = 3
        |align.mixedOwners = false
        |binPackImportSelectors = true
        |spaces.inImportCurlyBraces = true
        |spaces.afterTripleEquals = true
        |spaces.beforeContextBoundColon = true
        |unindentTopLevelOperators = false
        |bestEffortInDeeplyNestedCode = true
        |rewriteTokens: {
        |  "⇒" = "=>"
        |  "←" = "<-"
        |}
      """.stripMargin
    assertNoDiff(result, expected)
    val Right(_) = config.Config.fromHocon(result)
  }

}
