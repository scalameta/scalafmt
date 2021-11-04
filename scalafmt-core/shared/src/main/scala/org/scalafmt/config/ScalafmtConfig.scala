package org.scalafmt.config

import java.nio.file

import scala.collection.mutable
import scala.io.Codec
import scala.meta.Dialect
import scala.util.Try

import metaconfig._
import org.scalafmt.util.LoggerOps
import org.scalafmt.util.OsSpecific._
import org.scalafmt.util.ValidationOps

/** Configuration options for scalafmt.
  *
  * @param version
  *   The version of scalafmt to use for this project. Must match the currently
  *   running version of scalafmt.
  * @param maxColumn
  *   Column limit, any formatting exceeding this field is penalized heavily.
  * @param assumeStandardLibraryStripMargin
  *   If true, the margin character | is treated as the new indentation in
  *   multiline strings ending with `.stripMargin`.
  * @param danglingParentheses
  *   If true AND @binPackArguments is true AND @configStyleArguments is false,
  *   then this
  *
  * function( longerArg1, longerArg3)
  *
  * is formatted like this
  *
  * function( longerArg1, longerArg3 )
  * @param rewriteTokens
  *   Map of tokens to rewrite. For example, Map("⇒" -> "=>") will rewrite
  *   unicode arrows to regular ascii arrows.
  * @param importSelectors
  *   Controls formatting of import selectors with multiple names from the same
  *   package; If [[org.scalafmt.config.ImportSelectors.binPack]], import
  *   selectors are arranged to fit within the maximum line width If
  *   [[org.scalafmt.config.ImportSelectors.noBinPack]], import selectors are
  *   broken to one per line If
  *   [[org.scalafmt.config.ImportSelectors.singleLine]], import selectors are
  *   kept on a single line The default setting is currently `noBinPack`.
  * @param indentYieldKeyword
  *   If true, indents `yield` by two spaces for (i <- j) yield banana If false,
  *   treats `yield` like `else` for (i <- j) yield banana
  * @param lineEndings
  *   If [[LineEndings.unix]], output will include only unix line endings If
  *   [[LineEndings.windows]], output will include only windows line endings If
  *   [[LineEndings.preserve]], output will include endings included in original
  *   file (windows if there was at least one windows line ending, unix if there
  *   was zero occurrences of windows line endings)
  * @param includeCurlyBraceInSelectChains
  *   NB: failure unless newlines.source=classic If true, includes curly brace
  *   applications in select chains/pipelines.
  * {{{
  *     // If true
  *     List(1)
  *       .map { x =>
  *         x + 2
  *       }
  *       .filter(_ > 2)
  *     // If false
  *     List(1).map { x =>
  *         x + 2
  *     }.filter(_ > 2)
  * }}}
  * @param includeNoParensInSelectChains
  *   NB: ignored unless newlines.source=classic If true, includes applications
  *   without parens in select chains/pipelines.
  * {{{
  *     // If true
  *     List(1)
  *       .toIterator
  *       .buffered
  *       .map(_ + 2)
  *       .filter(_ > 2)
  *     // If false
  *     List(1).toIterator.buffered
  *       .map(_ + 2)
  *       .filter(_ > 2)
  * }}}
  */
case class ScalafmtConfig(
    version: String = org.scalafmt.Versions.stable,
    maxColumn: Int = 80,
    docstrings: Docstrings = Docstrings(),
    comments: Comments = Comments(),
    optIn: OptIn = OptIn(),
    binPack: BinPack = BinPack(),
    @annotation.ExtraName("continuationIndent")
    indent: Indents = Indents(),
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
    @annotation.DeprecatedName(
      "trailingCommas",
      "use rewrite.trailingCommas.style instead",
      "3.0.5"
    )
    private val trailingCommas: Option[TrailingCommas.Style] = None,
    verticalMultiline: VerticalMultiline = VerticalMultiline(),
    verticalAlignMultilineOperators: Boolean = false,
    onTestFailure: String = "",
    encoding: Codec = "UTF-8",
    project: ProjectFiles = ProjectFiles(),
    fileOverride: Conf.Obj = Conf.Obj.empty,
    xmlLiterals: XmlLiterals = XmlLiterals()
) {
  private[scalafmt] lazy val alignMap: Map[String, Seq[AlignToken.Matcher]] =
    align.tokens.map(x => x.code -> x).toMap.map { case (k, v) =>
      k -> v.getMatcher
    }

  private[scalafmt] def withDialect(
      dialect: ScalafmtRunner.Dialect.WithName
  ): ScalafmtConfig =
    copy(runner = runner.copy(dialect = dialect))

  def withDialect(dialect: Dialect, name: String): ScalafmtConfig =
    withDialect(ScalafmtRunner.Dialect.withName(name, dialect))

  def withDialect(dialect: Dialect): ScalafmtConfig = withDialect(
    dialect,
    ScalafmtRunner.Dialect.getName(dialect).getOrElse("unknown dialect")
  )

  def forSbt: ScalafmtConfig = copy(runner = runner.forSbt)

  private lazy val expandedFileOverride = Try {
    val fs = file.FileSystems.getDefault
    fileOverride.values.map { case (pattern, conf) =>
      val regex = "regex:" + ProjectFiles.FileMatcher.createRegexFromGlob(
        pattern.asFilename
      )
      val style = ScalafmtConfig.decoder.read(Some(this), conf).get
      fs.getPathMatcher(regex) -> style
    }
  }
  def getConfigFor(filename: String): ScalafmtConfig = {
    val path = file.FileSystems.getDefault.getPath(filename)
    expandedFileOverride.get
      .collectFirst { case (pm, style) if pm.matches(path) => style }
      .getOrElse(this)
  }

  private[scalafmt] lazy val encloseSelectChains =
    optIn.encloseClassicChains || newlines.source.ne(Newlines.classic)

  private[scalafmt] def indentOperatorTopLevelOnly =
    indentOperator.topLevelOnly && !unindentTopLevelOperators

  private[scalafmt] lazy val docstringsWrapMaxColumn: Int =
    docstrings.wrapMaxColumn.getOrElse(maxColumn)

  private[scalafmt] lazy val dialect = runner.getDialect

  private[scalafmt] def getTrailingCommas = rewrite.trailingCommas.style
}

object ScalafmtConfig {
  implicit lazy val surface: generic.Surface[ScalafmtConfig] =
    generic.deriveSurface
  implicit lazy val encoder: ConfEncoder[ScalafmtConfig] =
    generic.deriveEncoder[ScalafmtConfig]

  implicit lazy val codecEncoder: ConfEncoder[Codec] =
    ConfEncoder.StringEncoder.contramap(_.name)

  val default = ScalafmtConfig()
  private[scalafmt] val uncheckedDefault = ScalafmtConfig(version = null)

  val intellij: ScalafmtConfig = default.copy(
    indent = Indents(callSite = 2, defnSite = 2),
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

  /** Experimental implementation of:
    * https://github.com/scala-js/scala-js/blob/master/CODINGSTYLE.md
    */
  val scalaJs: ScalafmtConfig = default.copy(
    binPack = BinPack(
      unsafeDefnSite = BinPack.Unsafe.Always,
      unsafeCallSite = BinPack.Unsafe.Always,
      parentConstructors = BinPack.ParentCtors.Always
    ),
    indent = Indents(callSite = 4, defnSite = 4),
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
    docstrings = default.docstrings.copy(style = Docstrings.Asterisk),
    align = default.align.copy(
      arrowEnumeratorGenerator = false,
      tokens = Seq(AlignToken.caseArrow),
      openParenCtrlSite = false
    )
  )

  /** Ready styles provided by scalafmt.
    */
  private val activeStyles: Map[String, ScalafmtConfig] =
    Map(
      "Scala.js" -> scalaJs,
      "IntelliJ" -> intellij
    ) ++ LoggerOps.name2style(
      default,
      defaultWithAlign
    )

  private val availableStyles: Map[String, ScalafmtConfig] = {
    activeStyles ++ LoggerOps.name2style(
      scalaJs
    )
  }.map { case (k, v) => k.toLowerCase -> v }

  private[scalafmt] def conservativeRunner: ScalafmtRunner =
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
    // scalafmt: { maxColumn = 200 }
    import cfg._
    import ValidationOps._
    val errDialect = " (no support in Scala dialect)"
    val allErrors = new mutable.ArrayBuffer[String]
    locally {
      implicit val errors = new mutable.ArrayBuffer[String]
      if (newlines.sourceIgnored) {
        addIf(optIn.configStyleArguments && align.openParenCallSite && newlines.beforeOpenParenCallSite.isEmpty)
        addIf(optIn.configStyleArguments && align.openParenDefnSite && newlines.beforeOpenParenDefnSite.isEmpty)
        newlines.beforeMultiline.foreach { x =>
          addIf(
            x.eq(Newlines.classic) || x.eq(Newlines.keep),
            s"newlines.beforeMultiline=$x"
          )
        }
        newlines.beforeMultilineDef.foreach { x =>
          addIf(
            x.eq(Newlines.classic) || x.eq(Newlines.keep),
            s"newlines.beforeMultilineDef=$x"
          )
        }
        addIf(newlines.beforeTypeBounds eq Newlines.keep)
        addIf(binPack.parentConstructors eq BinPack.ParentCtors.keep)
        addIf(newlines.beforeOpenParenCallSite.exists(_.src eq Newlines.keep))
        addIf(newlines.beforeOpenParenDefnSite.exists(_.src eq Newlines.keep))
        addIf(newlines.selectChains.exists(_ eq Newlines.keep))
        addIf(getTrailingCommas.eq(TrailingCommas.keep))
      }
      if (newlines.source == Newlines.unfold) {
        addIf(align.arrowEnumeratorGenerator)
      }
      if (newlines.source != Newlines.classic) {
        addIf(optIn.breaksInsideChains)
        addIf(!includeCurlyBraceInSelectChains)
      }
      docstrings.validate
      if (errors.nonEmpty) {
        val prefix = s"newlines.source=${newlines.source} and ["
        allErrors += errors.mkString(prefix, ",", "]")
      }
    }
    locally {
      implicit val errors = allErrors
      if (!dialect.allowTrailingCommas) {
        addIf(getTrailingCommas eq TrailingCommas.always, errDialect)
        addIf(getTrailingCommas eq TrailingCommas.multiple, errDialect)
      }
      if (!dialect.allowSignificantIndentation) {
        addIf(newlines.beforeOpenParenCallSite.nonEmpty, errDialect)
      }
      addIfDirect( // can't use addIf on multiline conditions
        !(binPack.unsafeCallSite.isNever && binPack.unsafeDefnSite.isNever) && {
          newlines.implicitParamListModifierForce.nonEmpty ||
          newlines.implicitParamListModifierPrefer.nonEmpty
        },
        "binPack.unsafeXXXSite && newlines.implicitParamListModifierXXX (not implemented)"
      )
      checkPositive(
        indent.main,
        indent.callSite,
        indent.defnSite,
        indent.commaSiteRelativeToExtends
      )
      checkNonNeg(
        indent.caseSite,
        indent.extendSite,
        indent.withSiteRelativeToExtends
      )
      checkPositiveOpt(
        indent.significant,
        indent.ctorSite
      )
      if (rewrite.scala3.insertEndMarkerMinLines != 0)
        addIf(rewrite.scala3.removeEndMarkerMaxLines >= rewrite.scala3.insertEndMarkerMinLines)
    }
    // scalafmt: {}
    if (allErrors.isEmpty) Configured.ok(cfg)
    else {
      val msg = allErrors.mkString("can't use: [\n\t", "\n\t", "\n]")
      Configured.notOk(ConfError.message(msg))
    }
  }

  private val baseDecoder = generic.deriveDecoderEx(default).noTypos

  implicit final val decoder: ConfDecoderEx[ScalafmtConfig] =
    (stateOpt, conf) => {
      val stylePreset = conf match {
        case x: Conf.Obj =>
          val section = Seq(Presets.presetKey, "style").flatMap { y =>
            x.field(y).map(y -> _)
          }
          section.headOption.map { case (field, obj) =>
            obj -> Conf.Obj((x.map - field).toList)
          }
        case _ => None
      }
      val parsed = stylePreset match {
        case Some((styleConf, restConf)) =>
          ScalafmtConfig.readActiveStylePresets(styleConf).andThen { x =>
            val preset = stateOpt.fold(x) { state =>
              val isDefaultDialect = x.runner.isDefaultDialect
              val dialect = (if (isDefaultDialect) state else x).runner.dialect
              val parser = state.runner.parser
              x.copy(runner = x.runner.copy(parser = parser, dialect = dialect))
            }
            baseDecoder.read(Some(preset), restConf)
          }
        case _ => baseDecoder.read(stateOpt, conf)
      }
      parsed
        .map { cfg =>
          cfg.trailingCommas.fold(cfg) { tc =>
            val rt = cfg.rewrite.trailingCommas.copy(style = tc)
            cfg.copy(rewrite = cfg.rewrite.copy(trailingCommas = rt))
          }
        }
        .andThen(ScalafmtConfig.validate)
    }

}
