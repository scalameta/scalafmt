package org.scalafmt.config

import java.nio.file
import java.util.regex

import scala.collection.mutable
import scala.io.Codec
import scala.meta.Dialect
import scala.util.Try
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
  *  NB: failure unless newlines.source=classic
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
  *  NB: ignored unless newlines.source=classic
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
    docstrings: Docstrings = Docstrings(),
    comments: Comments = Comments(),
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
    @annotation.ExtraName("binPackImportSelectors")
    importSelectors: ImportSelectors = ImportSelectors.noBinPack,
    @annotation.DeprecatedName(
      "unindentTopLevelOperators",
      "Use indentOperator.topLevelOnly instead",
      "2.7.0"
    )
    private val unindentTopLevelOperators: Boolean = false,
    includeCurlyBraceInSelectChains: Boolean = true,
    includeNoParensInSelectChains: Boolean = false,
    assumeStandardLibraryStripMargin: Boolean = false,
    danglingParentheses: DanglingParentheses = DanglingParentheses(true, true),
    @annotation.DeprecatedName(
      "poorMansTrailingCommasInConfigStyle",
      "Scala supports trailing commas after 2.12.2. Use trailingCommas instead",
      "2.5.0"
    )
    poorMansTrailingCommasInConfigStyle: Boolean = false,
    trailingCommas: TrailingCommas = TrailingCommas.never,
    verticalMultiline: VerticalMultiline = VerticalMultiline(),
    verticalAlignMultilineOperators: Boolean = false,
    onTestFailure: String = "",
    encoding: Codec = "UTF-8",
    project: ProjectFiles = ProjectFiles(),
    fileOverride: Conf.Obj = Conf.Obj.empty,
    xmlLiterals: XmlLiterals = XmlLiterals()
) {

  private implicit def runnerReader = runner.reader
  private implicit def projectReader = project.reader
  private implicit def rewriteReader = rewrite.reader
  private implicit def spacesReader = spaces.reader
  private implicit def literalsReader = literals.reader
  private implicit def xmlLiteralsDecoder = xmlLiterals.decoder
  private implicit def continuationIndentReader = continuationIndent.reader
  private implicit def binpackReader = binPack.decoder
  private implicit def newlinesReader = newlines.reader
  private implicit def optInReader = optIn.reader
  private implicit def verticalMultilineReader = verticalMultiline.reader
  private implicit def alignDecoder = align.decoder
  private implicit def danglingParenthesesReader = danglingParentheses.decoder
  private implicit def indentOperatorReader = indentOperator.decoder
  private implicit def importSelectorsReader = importSelectors.decoder
  private implicit def docstringsDecoder = docstrings.decoder
  private implicit def commentsDecoder = comments.decoder
  lazy val alignMap: Map[String, regex.Pattern] =
    align.tokens.map(x => x.code -> x.owner.r.pattern).toMap
  private implicit val confObjReader = ScalafmtConfig.confObjReader
  private def baseDecoder = generic.deriveDecoder(this).noTypos

  implicit final lazy val decoder: ConfDecoder[ScalafmtConfig] =
    new ConfDecoder[ScalafmtConfig] {
      override def read(conf: Conf): Configured[ScalafmtConfig] = {
        val stylePreset = conf match {
          case x: Conf.Obj =>
            val section = Seq(Decodable.presetKey, "style").flatMap { y =>
              x.field(y).map(y -> _)
            }
            section.headOption.map {
              case (field, obj) => obj -> Conf.Obj((x.map - field).toList)
            }
          case _ => None
        }
        val parsed = stylePreset match {
          case Some((styleConf, restConf)) =>
            ScalafmtConfig
              .readActiveStylePresets(styleConf)
              .andThen(_.baseDecoder.read(restConf))
          case _ => baseDecoder.read(conf)
        }
        parsed.andThen(ScalafmtConfig.validate)
      }
    }

  def withDialect(dialect: Dialect): ScalafmtConfig =
    copy(runner = runner.copy(dialect = dialect))

  def forSbt: ScalafmtConfig = copy(runner = runner.forSbt)

  private lazy val expandedFileOverride = Try {
    val fs = file.FileSystems.getDefault
    fileOverride.values.map {
      case (pattern, conf) =>
        val style = decoder.read(conf).get
        fs.getPathMatcher(pattern) -> style
    }
  }
  def getConfigFor(filename: String): ScalafmtConfig = {
    val path = file.FileSystems.getDefault.getPath(filename)
    expandedFileOverride.get
      .collectFirst { case (pm, style) if pm.matches(path) => style }
      .getOrElse(this)
  }

  lazy val encloseSelectChains =
    optIn.encloseClassicChains || newlines.source.ne(Newlines.classic)

  def indentOperatorTopLevelOnly =
    indentOperator.topLevelOnly && !unindentTopLevelOperators

}

object ScalafmtConfig {
  implicit lazy val surface: generic.Surface[ScalafmtConfig] =
    generic.deriveSurface
  implicit lazy val encoder: ConfEncoder[ScalafmtConfig] =
    generic.deriveEncoder[ScalafmtConfig]

  implicit lazy val codecEncoder: ConfEncoder[Codec] =
    ConfEncoder.StringEncoder.contramap(_.name)

  implicit val confObjReader: ConfDecoder[Conf.Obj] =
    ConfDecoder.instance[Conf.Obj] {
      case x: Conf.Obj => Ok(x)
      case _ => ConfError.message("not a config").notOk
    }

  val default = ScalafmtConfig()

  val intellij: ScalafmtConfig = default.copy(
    continuationIndent = ContinuationIndent(2, 2),
    align = default.align.copy(openParenCallSite = false),
    optIn = default.optIn.copy(
      configStyleArguments = false
    ),
    danglingParentheses = DanglingParentheses(true, true)
  )

  def addAlign(style: ScalafmtConfig): ScalafmtConfig =
    style.copy(
      align = style.align.copy(
        tokens = AlignToken.default
      )
    )
  val defaultWithAlign: ScalafmtConfig = addAlign(default)

  /**
    * Experimental implementation of:
    * https://github.com/scala-js/scala-js/blob/master/CODINGSTYLE.md
    */
  val scalaJs: ScalafmtConfig = default.copy(
    binPack = BinPack(
      unsafeDefnSite = true,
      unsafeCallSite = true,
      parentConstructors = BinPack.ParentCtors.Always
    ),
    continuationIndent = ContinuationIndent(4, 4),
    importSelectors = ImportSelectors.binPack,
    newlines = default.newlines.copy(
      avoidInResultType = true,
      neverBeforeJsNative = true,
      sometimesBeforeColonInMethodReturnType = false
    ),
    // For some reason, the bin packing does not play nicely with forced
    // config style. It's fixable, but I don't want to spend time on it
    // right now.
    runner = conservativeRunner,
    docstrings = default.docstrings.copy(style = Some(Docstrings.Asterisk)),
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

  def conservativeRunner: ScalafmtRunner =
    default.runner.copy(
      optimizer = default.runner.optimizer.copy(
        // The tests were not written in this style
        forceConfigStyleOnOffset = 500,
        forceConfigStyleMinArgCount = 5
      )
    )

  private def readActiveStylePresets(conf: Conf): Configured[ScalafmtConfig] =
    (conf match {
      case Conf.Str(x) =>
        availableStyles.get(x.toLowerCase).map { style =>
          Configured.ok(style)
        }
      case _ => None
    }).getOrElse {
      val alternatives = activeStyles.keys.mkString(", ")
      val err = s"Unknown style $conf. Expected one of: $alternatives"
      ConfError.message(err).notOk
    }

  private def validate(cfg: ScalafmtConfig): Configured[ScalafmtConfig] = {
    import cfg._
    import Newlines._
    import ValidationOps._
    val allErrors = new mutable.ArrayBuffer[String]
    locally {
      implicit val errors = new mutable.ArrayBuffer[String]
      if (newlines.sourceIgnored) {
        addIf(
          newlines.afterCurlyLambdaParams == AfterCurlyLambdaParams.preserve
        )
        addIf(optIn.configStyleArguments && align.openParenCallSite)
        addIf(optIn.configStyleArguments && align.openParenDefnSite)
        newlines.beforeMultiline.foreach { x =>
          addIfDirect(
            x.eq(Newlines.classic) || x.eq(Newlines.keep),
            s"newlines.beforeMultiline=$x"
          )
        }
        newlines.beforeMultilineDef.foreach { x =>
          addIfDirect(
            x.eq(Newlines.classic) || x.eq(Newlines.keep),
            s"newlines.beforeMultilineDef=$x"
          )
        }
      }
      if (newlines.source == Newlines.unfold) {
        addIf(align.arrowEnumeratorGenerator)
      }
      if (newlines.source != Newlines.classic) {
        addIf(optIn.breaksInsideChains)
        addIf(!includeCurlyBraceInSelectChains)
      }
      if (errors.nonEmpty) {
        val prefix = s"newlines.source=${newlines.source} and ["
        allErrors += errors.mkString(prefix, ",", "]")
      }
    }
    locally {
      implicit val errors = allErrors
      addIf(align.ifWhileOpenParen && danglingParentheses.ctrlSite)
      if (!runner.dialect.allowTrailingCommas) {
        def err = " (no support in Scala dialect)"
        addIf(trailingCommas == TrailingCommas.always, err)
        addIf(trailingCommas == TrailingCommas.multiple, err)
      }
      addIfDirect( // can't use addIf on multiline conditions
        (binPack.unsafeCallSite || binPack.unsafeDefnSite) && {
          newlines.implicitParamListModifierForce.nonEmpty ||
          newlines.implicitParamListModifierPrefer.nonEmpty
        },
        "binPack.unsafeXXX && newlines.implicitParamListModifierXXX (not implemented)"
      )
      addIfNegative(continuationIndent.callSite, continuationIndent.defnSite)
    }
    if (allErrors.isEmpty) Configured.ok(cfg)
    else {
      val msg = allErrors.mkString("can't use: [\n\t", "\n\t", "\n]")
      Configured.notOk(ConfError.message(msg))
    }
  }

}
