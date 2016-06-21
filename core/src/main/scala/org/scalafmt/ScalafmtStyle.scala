package org.scalafmt

import scala.util.matching.Regex

import org.scalafmt.util.ValidationOps
import sourcecode.Text

/** Configuration options for scalafmt.
  *
  * @param maxColumn Column limit, any formatting exceeding this field is
  *                  penalized heavily.
  * @param scalaDocs Use scaladoc style docstring, otherwise use javadoc style.
  * @param alignStripMarginStrings If true, the margin character | is treated
  *                                as the new indentation in multiline strings
  *                                ending with `.stripMargin`.
  * @param binPackArguments If true, will fit as many arguments on each line,
  *                          only breaking at commas. If false, a function
  *                          call's arguments will either be all on the same
  *                          line or will have one line each.
  * @param binPackParameters Same as [[binPackArguments]], except for def/class
  *                          definition parameters.
  * @param configStyleArguments Call-sites where there is a newline after
  *                             opening ( and newline before closing ).
  *                             If true, preserves the newlines and keeps one
  *                             line per argument.
  * @param binPackDotChains If true, will fit as many arguments on each line,
  *                         only breaking at dots. If false, a either all selects
  *                         go on the same line or will have one line each.
  * @param noNewlinesBeforeJsNative If true, a newline will never be placed in
  *                                 front of js.native.
  * @param continuationIndentCallSite Indent width for line continuation at
  *                                   call site.
  * @param continuationIndentDefnSite Indent width for line continuation at
  *                                   definition/declaration site.
  * @param allowNewlineBeforeColonInMassiveReturnTypes If true, scalafmt
  *                                                    may choose to put a newline
  *                                                    before colon : at defs.
  * @param binPackParentConstructors Parent constructors are C and D in
  *                                  "class A extends B with C and D". If true,
  *                                  scalafmt will fit as many parent constructors
  *                                  on a single line. If false, each parent
  *                                  constructor gets its own line.
  * @param spaceAfterTripleEquals If true, formats ===( as === (
  * @param alignByArrowEnumeratorGenerator If true, aligns by <- in for comprehensions.
  * @param alignByIfWhileOpenParen If true, aligns by ( in if/while/for. If false,
  *                                indents by [[continuationIndentCallSite]].
  */
case class ScalafmtStyle(
    maxColumn: Int,
    scalaDocs: Boolean,
    alignStripMarginStrings: Boolean,
    binPackArguments: Boolean,
    binPackParameters: Boolean,
    configStyleArguments: Boolean,
    binPackDotChains: Boolean,
    noNewlinesBeforeJsNative: Boolean,
    continuationIndentCallSite: Int,
    continuationIndentDefnSite: Int,
    alignTokens: Set[AlignToken],
    spacesInImportCurlyBraces: Boolean,
    allowNewlineBeforeColonInMassiveReturnTypes: Boolean,
    binPackParentConstructors: Boolean,
    spaceAfterTripleEquals: Boolean,
    alignByArrowEnumeratorGenerator: Boolean,
    alignByIfWhileOpenParen: Boolean
) {
  lazy val alignMap: Map[String, Regex] =
    alignTokens.map(x => x.code -> x.owner.r).toMap
  ValidationOps.assertNonNegative(
      continuationIndentCallSite,
      continuationIndentDefnSite
  )
}

object ScalafmtStyle {
  val default = ScalafmtStyle(
      maxColumn = 80,
      scalaDocs = true,
      alignStripMarginStrings = false,
      binPackArguments = false,
      binPackParameters = false,
      configStyleArguments = true,
      binPackDotChains = false,
      noNewlinesBeforeJsNative = false,
      continuationIndentCallSite = 4,
      continuationIndentDefnSite = 4,
      alignTokens = Set.empty[AlignToken],
      spacesInImportCurlyBraces = false,
      allowNewlineBeforeColonInMassiveReturnTypes = true,
      binPackParentConstructors = false,
      spaceAfterTripleEquals = false,
      alignByArrowEnumeratorGenerator = true,
      alignByIfWhileOpenParen = true
  )

  val defaultWithAlign = default.copy(alignTokens = AlignToken.default)

  val default40 = default.copy(maxColumn = 40)
  val default120 = default.copy(maxColumn = 120)

  /**
    * Experimental implementation of:
    * https://github.com/scala-js/scala-js/blob/master/CODINGSTYLE.md
    */
  val scalaJs = default.copy(
      noNewlinesBeforeJsNative = true,
      binPackArguments = true,
      binPackParameters = true,
      allowNewlineBeforeColonInMassiveReturnTypes = false,
      scalaDocs = false,
      binPackParentConstructors = true,
      alignByArrowEnumeratorGenerator = false,
      alignTokens = Set(AlignToken.caseArrow),
      alignByIfWhileOpenParen = false
  )

  // TODO(olafur) parameterize
  private def name2style(
      styles: Text[ScalafmtStyle]*): Map[String, ScalafmtStyle] =
    styles.map(x => x.source -> x.value).toMap

  /**
    * Ready styles provided by scalafmt.
    */
  val activeStyles =
    Map(
        "Scala.js" -> scalaJs
    ) ++ name2style(
        default,
        defaultWithAlign
    )
  val availableStyles = {
    activeStyles ++ name2style(
        scalaJs
    )
  }.map { case (k, v) => k.toLowerCase -> v }

  // TODO(olafur) move these elsewhere.
  val testing = default.copy(alignStripMarginStrings = false)
  val unitTest80 = testing.copy(maxColumn = 80)
  val unitTest40 = testing.copy(maxColumn = 40)
}
