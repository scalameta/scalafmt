package org.scalafmt.config

import scala.io.Codec
import scala.meta.Dialect
import scala.util.matching.Regex
import metaconfig.annotation._
import metaconfig._
import metaconfig.Configured._
import org.scalafmt.util.LoggerOps
import org.scalafmt.util.ValidationOps

/** Configuration options for scalafmt.
  *
  * @param version The version of scalafmt to use for this project. Currently not used,
  *                the plan is to use this field for the IntelliJ+sbt integrations.
  * @param maxColumn Column limit, any formatting exceeding this field is
  *                  penalized heavily.
  * @param docstrings Several options:
  *                   - ScalaDoc: format as Scala docs
  *                   - JavaDocs: format as Java docs
  *                   - preserve: keep existing formatting
  * @param assumeStandardLibraryStripMargin If true, the margin character | is treated
  *                                as the new indentation in multiline strings
  *                                ending with `.stripMargin`.
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
  * @param rewriteTokens Map of tokens to rewrite. For example, Map("⇒" -> "=>")
  *                      will rewrite unicode arrows to regular ascii arrows.
  * @param importSelectors Controls formatting of import selectors with multiple names from the
  *                        same package;
  *                        If [[org.scalafmt.config.ImportSelectors.binPack]], import selectors are
  *                        arranged to fit within the maximum line width
  *                        If [[org.scalafmt.config.ImportSelectors.noBinPack]], import selectors
  *                        are broken to one per line
  *                        If [[org.scalafmt.config.ImportSelectors.singleLine]], import selectors
  *                        are kept on a single line
  *                        The default setting is currently `noBinPack`.
  * @param poorMansTrailingCommasInConfigStyle (experimental, may be removed)
  *                                      If true, formats config style like this:
  *
  *                                      function(
  *                                        a
  *                                        , b
  *                                        , c
  *                                      )
  * @param trailingCommas If [[org.scalafmt.config.TrailingCommas.always]], trailing
  *                       commas are added everywhere a newline is followed by a right parens, brace
  *                       or bracket.
  *
  *                       If [[org.scalafmt.config.TrailingCommas.never]], trailing
  *                       commas are removed whenever they appear.
  *
  *                       If [[org.scalafmt.config.TrailingCommas.preserve]], existing
  *                       trailing commas will be preserved, and no new ones will be added.
  *
  * @param indentYieldKeyword If true, indents `yield` by two spaces
  *                           for (i <- j)
  *                             yield banana
  *                           If false, treats `yield` like `else`
  *                           for (i <- j)
  *                           yield banana
  * @param lineEndings If [[LineEndings.unix]], output will include only unix line endings
  *                    If [[LineEndings.windows]], output will include only windows line endings
  *                    If [[LineEndings.preserve]], output will include endings included in original
  *                    file (windows if there was at least one windows line ending, unix if there
  *                    was zero occurrences of windows line endings)
  * @param includeCurlyBraceInSelectChains
  *  If true, includes curly brace applications in select chains/pipelines.
  *  {{{
  *    // If true
  *    List(1)
  *      .map { x =>
  *        x + 2
  *      }
  *      .filter(_ > 2)
  *    // If false
  *    List(1).map { x =>
  *        x + 2
  *    }.filter(_ > 2)
  *  }}}
  * @param includeNoParensInSelectChains
  *  If true, includes applications without parens in select chains/pipelines.
  *  {{{
  *    // If true
  *    List(1)
  *      .toIterator
  *      .buffered
  *      .map(_ + 2)
  *      .filter(_ > 2)
  *    // If false
  *    List(1).toIterator.buffered
  *      .map(_ + 2)
  *      .filter(_ > 2)
  *  }}}
  */
case class ScalafmtConfig(
    version: String = org.scalafmt.Versions.stable,
    maxColumn: Int = 80,
    docstrings: Docstrings = Docstrings.ScalaDoc,
    optIn: OptIn = OptIn(),
    binPack: BinPack = BinPack(),
    continuationIndent: ContinuationIndent = ContinuationIndent(),
    align: Align = Align(),
    spaces: Spaces = Spaces(),
    literals: Literals = Literals(),
    lineEndings: LineEndings = LineEndings.unix,
    rewriteTokens: Map[String, String] = Map.empty[String, String],
    rewrite: RewriteSettings = RewriteSettings(),
    indentOperator: IndentOperator = IndentOperator(),
    newlines: Newlines = Newlines(),
    runner: ScalafmtRunner = ScalafmtRunner.default,
    // Settings which belong to no group
    indentYieldKeyword: Boolean = true,
    @ExtraName("binPackImportSelectors")
    importSelectors: ImportSelectors = ImportSelectors.noBinPack,
    unindentTopLevelOperators: Boolean = false,
    includeCurlyBraceInSelectChains: Boolean = true,
    includeNoParensInSelectChains: Boolean = false,
    assumeStandardLibraryStripMargin: Boolean = false,
    danglingParentheses: DanglingParentheses = DanglingParentheses(true, true),
    poorMansTrailingCommasInConfigStyle: Boolean = false,
    trailingCommas: TrailingCommas = TrailingCommas.never,
    @deprecated("Use VerticalMultiline.atDefnSite instead", "1.6.0")
    verticalMultilineAtDefinitionSite: Boolean = false,
    @deprecated("Use VerticalMultiline.arityThreshold instead", "1.6.0")
    verticalMultilineAtDefinitionSiteArityThreshold: Int = 100,
    verticalMultiline: VerticalMultiline = VerticalMultiline(),
    verticalAlignMultilineOperators: Boolean = false,
    onTestFailure: String = "",
    encoding: Codec = "UTF-8",
    project: ProjectFiles = ProjectFiles(),
    edition: Edition = Edition.Latest
) {
  private implicit val runnerReader = runner.reader
  private implicit val projectReader = project.reader
  private implicit val rewriteReader = rewrite.reader
  private implicit val spacesReader = spaces.reader
  private implicit val literalsReader = literals.reader
  private implicit val continuationIndentReader = continuationIndent.reader
  private implicit val binpackReader = binPack.reader
  private implicit val newlinesReader = newlines.reader
  private implicit val optInReader = optIn.reader
  private implicit val verticalMultilineReader = verticalMultiline.reader
  implicit val alignDecoder: ConfDecoder[Align] =
    ScalafmtConfig.alignReader(align.reader)
  lazy val alignMap: Map[String, Regex] =
    align.tokens.map(x => x.code -> x.owner.r).toMap
  def reader: ConfDecoder[ScalafmtConfig] =
    generic.deriveDecoder(this).noTypos.noTypos

  def withDialect(dialect: Dialect): ScalafmtConfig =
    copy(runner = runner.copy(dialect = dialect))

  def forSbt: ScalafmtConfig = copy(runner = runner.forSbt)

  def reformatDocstrings: Boolean = docstrings != Docstrings.preserve
  def scalaDocs: Boolean = docstrings == Docstrings.ScalaDoc
  ValidationOps.assertNonNegative(
    continuationIndent.callSite,
    continuationIndent.defnSite
  )

  // Edition-specific settings below
  def activeFor(edition: Edition): Boolean = this.edition >= edition

  // Edition 2019-11
  val activeForEdition_2019_11: Boolean = activeFor(Edition(2019, 11))
  val newlinesBeforeSingleArgParenLambdaParams: Boolean =
    newlines.alwaysBeforeCurlyBraceLambdaParams || !activeForEdition_2019_11
  val newlinesBetweenCurlyAndCatchFinally: Boolean =
    newlines.alwaysBeforeElseAfterCurlyIf && activeForEdition_2019_11

  // Edition 2020-01
  val activeForEdition_2020_01: Boolean = activeFor(Edition(2020, 1))
}

object ScalafmtConfig {
  implicit lazy val surface: generic.Surface[ScalafmtConfig] =
    generic.deriveSurface
  implicit lazy val encoder: ConfEncoder[ScalafmtConfig] =
    generic.deriveEncoder[ScalafmtConfig]

  implicit lazy val codecEncoder: ConfEncoder[Codec] =
    ConfEncoder.StringEncoder.contramap(_.name)

  val indentOperatorsIncludeAkka = "^.*=$"
  val indentOperatorsExcludeAkka = "^$"

  val default = ScalafmtConfig()

  val intellij: ScalafmtConfig = default.copy(
    continuationIndent = ContinuationIndent(2, 2),
    align = default.align.copy(openParenCallSite = false),
    optIn = default.optIn.copy(
      configStyleArguments = false
    ),
    danglingParentheses = DanglingParentheses(true, true)
  )

  def addAlign(style: ScalafmtConfig): ScalafmtConfig = style.copy(
    align = style.align.copy(
      tokens = AlignToken.default
    )
  )

  val defaultWithAlign: ScalafmtConfig = addAlign(default)

  val default40: ScalafmtConfig = default.copy(maxColumn = 40)
  val default120: ScalafmtConfig = default.copy(maxColumn = 120)

  /**
    * Experimental implementation of:
    * https://github.com/scala-js/scala-js/blob/master/CODINGSTYLE.md
    */
  val scalaJs: ScalafmtConfig = default.copy(
    binPack = BinPack(
      unsafeDefnSite = true,
      unsafeCallSite = true,
      parentConstructors = true
    ),
    continuationIndent = ContinuationIndent(4, 4),
    importSelectors = ImportSelectors.binPack,
    newlines = default.newlines.copy(
      neverInResultType = true,
      neverBeforeJsNative = true,
      sometimesBeforeColonInMethodReturnType = false
    ),
    // For some reason, the bin packing does not play nicely with forced
    // config style. It's fixable, but I don't want to spend time on it
    // right now.
    runner = conservativeRunner,
    docstrings = Docstrings.JavaDoc,
    align = default.align.copy(
      arrowEnumeratorGenerator = false,
      tokens = Seq(AlignToken.caseArrow),
      ifWhileOpenParen = false
    )
  )

  /**
    * Ready styles provided by scalafmt.
    */
  val activeStyles: Map[String, ScalafmtConfig] =
    Map(
      "Scala.js" -> scalaJs,
      "IntelliJ" -> intellij
    ) ++ LoggerOps.name2style(
      default,
      defaultWithAlign
    )

  val availableStyles: Map[String, ScalafmtConfig] = {
    activeStyles ++ LoggerOps.name2style(
      scalaJs
    )
  }.map { case (k, v) => k.toLowerCase -> v }

  def conservativeRunner: ScalafmtRunner = default.runner.copy(
    optimizer = default.runner.optimizer.copy(
      // The tests were not written in this style
      forceConfigStyleOnOffset = 500,
      forceConfigStyleMinArgCount = 5
    )
  )

  def oneOf[T](m: Map[String, T])(input: String): Configured[T] =
    m.get(input.toLowerCase()) match {
      case Some(x) => Ok(x)
      case None =>
        val available = m.keys.mkString(", ")
        val msg =
          s"Unknown line endings type $input. Expected one of $available"
        ConfError.message(msg).notOk

    }

  def configReader(baseReader: ScalafmtConfig): ConfDecoder[ScalafmtConfig] =
    ConfDecoder.instance[ScalafmtConfig] {
      case conf @ Conf.Obj(values) =>
        val map = values.toMap
        map.get("style") match {
          case Some(Conf.Str(baseStyle)) =>
            val noStyle = Conf.Obj(values.filterNot(_._1 == "style"))
            ScalafmtConfig.availableStyles.get(baseStyle.toLowerCase) match {
              case Some(s) => s.reader.read(noStyle)
              case None =>
                val alternatives =
                  ScalafmtConfig.activeStyles.keys.mkString(", ")
                ConfError
                  .message(
                    s"Unknown style name $baseStyle. Expected one of: $alternatives"
                  )
                  .notOk
            }
          case _ =>
            baseReader.reader.read(conf)
        }
    }

  def alignReader(base: ConfDecoder[Align]): ConfDecoder[Align] =
    ConfDecoder.instance[Align] {
      case Align.Builtin(a) => Ok(a)
      case els => base.read(els)
    }
  def alignTokenReader(
      initTokens: Seq[AlignToken]
  ): ConfDecoder[Seq[AlignToken]] = {
    val baseReader = ConfDecoder[Seq[AlignToken]]
    ConfDecoder.instance[Seq[AlignToken]] {
      case Conf.Obj(("add", conf) :: Nil) =>
        baseReader.read(conf).map(initTokens ++ _)
      case Align.Builtin(a) => Ok(a.tokens)
      case els => baseReader.read(els)
    }
  }
}
