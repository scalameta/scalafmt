package org.scalafmt.config

import scala.util.matching.Regex

import metaconfig.ConfigReader
import metaconfig.Reader
import org.scalafmt.util.ValidationOps

/** Configuration options for scalafmt.
  *
  * @param maxColumn Column limit, any formatting exceeding this field is
  *                  penalized heavily.
  * @param docstrings If true, reformats docstrings according to @scalaDocs.
  * @param scalaDocs Only used if @reformatDocstrings is true. If true,
  *                  reformats docstrings to use scaladoc style docstring,
  *                  otherwise use javadoc style.
  * @param assumeStandardLibraryStripMargin If true, the margin character | is treated
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
  * @param neverBeforeJsNative If true, a newline will never be placed in
  *                                 front of js.native.
  * @param danglingParentheses If true
  *                            AND @binPackArguments is true
  *                            AND @configStyleArguments is false, then this
  *
  *                            function(
  *                                longerArg1,
  *                                longerArg3)
  *
  *                            is formatted like this
  *
  *                            function(
  *                                longerArg1,
  *                                longerArg3
  *                            )
  * @param alignByOpenParenCallSite If true AND bin-packing is true, then call-site
  *                                 arguments won't be aligned by the opening
  *                                 parenthesis. For example, this output
  *                                 will be disallowed
  *
  *                                 function(a,
  *                                          b,
  *                                          c)
  * @param continuationIndentCallSite Indent width for line continuation at
  *                                   call site.
  * @param continuationIndentDefnSite Indent width for line continuation at
  *                                   definition/declaration site.
  * @param sometimesBeforeColonInMethodReturnType If true, scalafmt
  *                                                    may choose to put a newline
  *                                                    before colon : at defs.
  * @param binPackParentConstructors Parent constructors are C and D in
  *                                  "class A extends B with C and D". If true,
  *                                  scalafmt will fit as many parent constructors
  *                                  on a single line. If false, each parent
  *                                  constructor gets its own line.
  * @param unindentTopLevelOperators If true, allows no indentation on infix operators
  *                                  in non-top-level functions. For example,
  *
  *                                  function(
  *                                      a &&
  *                                      b
  *                                  )
  *
  *                                  If false, only allows 0 space indentation for
  *                                  top-level statements
  *
  *                                  a &&
  *                                  b
  *                                  function(
  *                                      a &&
  *                                        b
  *                                  )
  *
  *                                  Context: https://github.com/scala-js/scala-js/blob/master/CODINGSTYLE.md#long-expressions-with-binary-operators
  * @param indentOperatorsIncludeFilter Regexp for which infix operators should
  *                                     indent by 2 spaces. For example, .*=
  *                                     produces this output
  *
  *                                     a &&
  *                                     b
  *
  *                                     a +=
  *                                       b
  * @param indentOperatorsExcludeFilter Regexp for which infix operators should
  *                                     not indent by 2 spaces. For example, when
  *                                     [[indentOperatorsIncludeFilter]] is .* and
  *                                     [[indentOperatorsExcludeFilter]] is &&
  *
  *                                     a &&
  *                                     b
  *
  *                                     a ||
  *                                       b
  *
  *                                     a +=
  *                                       b
  * @param spaceAfterTripleEquals If true, formats ===( as === (
  * @param alignByArrowEnumeratorGenerator If true, aligns by <- in for comprehensions.
  * @param alignByIfWhileOpenParen If true, aligns by ( in if/while/for. If false,
  *                                indents by [[continuationIndentCallSite]].
  * @param rewriteTokens Map of tokens to rewrite. For example, Map("⇒" -> "=>")
  *                      will rewrite unicode arrows to regular ascii arrows.
  * @param spaceBeforeContextBoundColon formats [A: T] as [A : T]
  * @param alignMixedOwners If true, aligns `=` for val/var/def and
  *                         `extends` for def/trait/object.
  * @param alignTokens Documented in scalafmt --help page.
  * @param spacesInImportCurlyBraces If true, formats `import a.b.{ c, d }`.
  *                                  If false, formats `import a.b.{c, d}`.
  * @param binPackImportSelectors If true, breaks up import selectors if
  *                                   it overflows [[maxColumn]]. For example
  *                                   import a.b.{
  *                                     c,
  *                                     d
  *                                   }. If false, import selectors stay on a
  *                                   single line.
  * @param poorMansTrailingCommasInConfigStyle (experimental, may be removed)
  *                                      If true, formats config style like this:
  *
  *                                      function(
  *                                        a
  *                                        , b
  *                                        , c
  *                                      )
  * @param breakChainOnFirstMethodDot If true, keep line breaks for chained method calls
  *                                  If false, format line breaks for chained method calls
  *                                   For example
  *                                   example(foo)
  *                                     .methodCall(bar)
  *                                   will be kept as it is if this is true, while if false
  *                                   it will be formatted as
  *                                   example(foo).methodCall(bar)
  * @param alwaysBeforeCurlyBraceLambdaParams If true, puts a newline after the open brace
  *                                      and the parameters list of an anonymous function.
  *                                      For example
  *                                      something.map {
  *                                        n =>
  *                                          consume(n)
  *                                      }
  * @param lineEndings If [[org.scalafmt.LineEndings.unix]], output will include only unix line endings
  *                    If [[org.scalafmt.LineEndings.windows]], output will include only windows line endings
  *                    If [[org.scalafmt.LineEndings.preserve]], output will include endings included in original
  *                    file (windows if there was at least one windows line ending, unix if there
  *                    was zero occurrences of windows line endings)
  *
  */
@ConfigReader
case class ScalafmtConfig(
    maxColumn: Int = 80,
    docstrings: Docstrings = Docstrings.ScalaDoc,
    optIn: OptIn = OptIn(),
    binPack: BinPack = BinPack(),
    continuationIndent: ContinuationIndent = ContinuationIndent(),
    align: Align = Align(),
    spaces: Spaces = Spaces(),
    lineEndings: LineEndings = LineEndings.unix,
    rewriteTokens: Map[String, String] = Map.empty[String, String],
    rewrite: RewriteSettings = RewriteSettings(),
    indentOperator: IndentOperator = IndentOperator(),
    newlines: Newlines = Newlines(),
    runner: ScalafmtRunner = ScalafmtRunner.default,
    // Settings which belong to no group
    binPackImportSelectors: Boolean = false, // TODO(olafur) bundle into `importSelectors: oneOf(singleLine, binPack, default)`
    unindentTopLevelOperators: Boolean = false,
    assumeStandardLibraryStripMargin: Boolean = false,
    danglingParentheses: Boolean = false,
    poorMansTrailingCommasInConfigStyle: Boolean = false,
    bestEffortInDeeplyNestedCode: Boolean = false
) {

  // TODO(olafur): Remove these when I have time.
  def neverBeforeJsNative: Boolean = newlines.neverBeforeJsNative
  def sometimesBeforeColonInMethodReturnType: Boolean =
    newlines.sometimesBeforeColonInMethodReturnType
  def alwaysBeforeCurlyBraceLambdaParams: Boolean =
    newlines.alwaysBeforeCurlyBraceLambdaParams

  def configStyleArguments: Boolean = optIn.configStyleArguments
  def breakChainOnFirstMethodDot: Boolean = optIn.breakChainOnFirstMethodDot
  def reformatDocstrings: Boolean = docstrings != Docstrings.preserve
  def scalaDocs: Boolean = docstrings == Docstrings.ScalaDoc
  def binPackParentConstructors: Boolean = binPack.parentConstructors

  implicit val runnerReader: Reader[ScalafmtRunner] =
    runner.reader
  implicit val contIndentReader: Reader[ContinuationIndent] =
    continuationIndent.reader
  implicit val indentReader: Reader[IndentOperator] = indentOperator.reader
  implicit val binPackReader: Reader[BinPack] = binPack.reader
  implicit val alignReader: Reader[Align] = align.reader
  implicit val lineEndingReader: Reader[LineEndings] = LineEndings.reader
  implicit val spacesReader: Reader[Spaces] = spaces.reader
  implicit val docstringsReader: Reader[Docstrings] = Docstrings.reader
  implicit val rewriteReader: Reader[RewriteSettings] = rewrite.reader
  implicit val optInReader: Reader[OptIn] = optIn.reader
  implicit val newlinesReader: Reader[Newlines] = newlines.reader

  lazy val alignMap: Map[String, Regex] =
    align.tokens.map(x => x.code -> x.owner.r).toMap
  ValidationOps.assertNonNegative(
    continuationIndent.callSite,
    continuationIndent.defnSite
  )
}

object ScalafmtConfig extends Settings
