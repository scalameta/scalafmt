package org.scalafmt.cli

object LegacyCli {
  private def gimmeStrPairs(tokens: Seq[String]): Seq[(String, String)] = {
    tokens.map { token =>
      val splitted = token.split(";", 2)
      if (splitted.length != 2)
        throw new IllegalArgumentException("pair must contain ;")
      (splitted(0), splitted(1))
    }
  }
  def migrate(contents: String): String = {
    val regexp: Seq[String => String] = Seq(
      "--bestEffortInDeeplyNestedCode" -> "bestEffortInDeeplyNestedCode = true",
      "--scalaDocs true" -> "docstrings = ScalaDoc",
      "--indentOperators false" -> "indentOperator = spray",
      "--indentOperators true" -> "",
      "--scalaDocs false" -> "docstrings = JavaDoc",
      "--reformatComments true" -> "",
      "--reformatComments false" -> "docstrings = preserve",
      "--(\\w+) (.*)" -> "$1 = $2",
      "indentOperatorsIncludeFilter" -> "indentOperator.include",
      "indentOperatorsExcludeFilter" -> "indentOperator.exclude",
      "alignTokens" -> "align.tokens",
      "noNewlinesBeforeJsNative" -> "newlines.neverBeforeJsNative",
      "allowNewlineBeforeColonInMassiveReturnTypes" -> "newlines.sometimesBeforeColonInMethodReturnType",
      "configStyleArguments" -> "optIn.configStyleArguments",
      "alignStripMarginStrings" -> "assumeStandardLibraryStripMargin",
      "binPackArguments" -> "binPack.callSite",
      "binPackParameters" -> "binPack.defnSite",
      "binPackParentConstructors" -> "binPack.parentConstructors",
      "alignByOpenParenCallSite" -> "align.openParenCallSite",
      "alignByOpenParenDefnSite" -> "align.openParenDefnSite",
      "continuationIndentCallSite" -> "continuationIndent.callSite",
      "continuationIndentDefnSite" -> "continuationIndent.defnSite",
      "alignMixedOwners" -> "align.mixedOwners",
      "spacesInImportCurlyBraces" -> "spaces.inImportCurlyBraces",
      "spaceAfterTripleEquals" -> "spaces.afterTripleEquals",
      "spaceBeforeContextBoundColon" -> "spaces.beforeContextBoundColon"
    ).map {
      case (from, to) =>
        (x: String) =>
          x.replaceAll(from, to)
    }
    val alignR = "(align.tokens = )\"?([^#\"]*)\"?(.*)$".r
    val rewriteR = "(rewriteTokens = )\"?([^#\"]*)\"?(.*)$".r
    val custom = Seq[String => String](
      x =>
        x.lines.map {
          case rewriteR(lhs, rhs, comments) =>
            val arr = gimmeStrPairs(rhs.split(",").toSeq).map {
              case (l, r) => s"""  "$l" = "$r""""
            }.mkString("\n")
            s"""rewriteTokens: {$comments
               |$arr
               |}""".stripMargin
          case alignR(lhs, rhs, comments) =>
            val arr = gimmeStrPairs(rhs.split(",").toSeq).map {
              case (l, r) if r == ".*" => s""""$l""""
              case (l, r) => s"""{ code = "$l", owner = "$r" }"""
            }.mkString("\n  ")
            s"""|$lhs[$comments
                |  $arr
                |]""".stripMargin
          case y => y
        }.mkString("\n")
    )

    (regexp ++ custom).foldLeft(contents) {
      case (curr, f) => f(curr)
    }
  }

}
