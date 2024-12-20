package org.scalafmt.config

import org.scalafmt.Versions
import org.scalafmt.rewrite._
import org.scalafmt.sysops.AbsoluteFile
import org.scalafmt.sysops.OsSpecific._
import org.scalafmt.util._

import scala.meta._

import java.nio.file._

import scala.collection.mutable
import scala.io.Codec
import scala.util.Try

import metaconfig._

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
  *   {{{
  *     function(
  *         longerArg1,
  *         longerArg3)
  *   }}}
  *   is formatted like this
  *   {{{
  *     function(
  *         longerArg1,
  *         longerArg3
  *     )
  *   }}}
  * @param rewriteTokens
  *   Map of tokens to rewrite. For example, Map("⇒" -> "=>") will rewrite
  *   unicode arrows to regular ascii arrows.
  * @param importSelectors
  *   Controls formatting of import selectors with multiple names from the same
  *   package
  *   - If [[org.scalafmt.config.ImportSelectors.binPack]], import selectors are
  *     arranged to fit within the maximum line width
  *   - If [[org.scalafmt.config.ImportSelectors.noBinPack]], import selectors
  *     are broken to one per line
  *   - If [[org.scalafmt.config.ImportSelectors.singleLine]], import selectors
  *     are kept on a single line The default setting is currently `noBinPack`.
  * @param indentYieldKeyword
  *   - If true, indents `yield` by two spaces
  *     {{{
  *       for (i <- j)
  *         yield banana
  *     }}}
  *   - If false, treats `yield` like `else`
  *     {{{
  *       for (i <- j)
  *       yield banana
  *     }}}
  * @param lineEndings
  *   - If [[LineEndings.unix]], output will include only unix line endings
  *   - If [[LineEndings.windows]], output will include only windows line
  *     endings
  *   - If [[LineEndings.preserve]], output will include endings included in
  *     original file (windows if there was at least one windows line ending,
  *     unix if there was zero occurrences of windows line endings)
  * @param includeCurlyBraceInSelectChains
  *   NB: failure unless newlines.source=classic If true, includes curly brace
  *   applications in select chains/pipelines.
  *   {{{
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
  *   }}}
  * @param includeNoParensInSelectChains
  *   NB: ignored unless newlines.source=classic If true, includes applications
  *   without parens in select chains/pipelines.
  *   {{{
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
  *   }}}
  */
// scalafmt: { maxColumn = 100 }
@annotation.SectionRename("optIn.configStyleArguments", "newlines.configStyle.fallBack.prefer") // v3.8.2
@annotation.SectionRename("trailingCommas", "rewrite.trailingCommas.style") // v3.0.5
@annotation.SectionRename("optIn.forceBlankLineBeforeDocstring", "docstrings.forceBlankLineBefore") // v3.4.0
// scalafmt: { maxColumn = 80 }
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
    lineEndings: Option[LineEndings] = None,
    rewriteTokens: Map[String, String] = Map.empty[String, String],
    rewrite: RewriteSettings = RewriteSettings.default,
    indentOperator: IndentOperator = IndentOperator(),
    newlines: Newlines = Newlines(),
    runner: ScalafmtRunner = ScalafmtRunner.default,
    // Settings which belong to no group
    indentYieldKeyword: Boolean = true,
    @annotation.ExtraName("binPackImportSelectors")
    importSelectors: ImportSelectors = ImportSelectors.noBinPack,
    includeCurlyBraceInSelectChains: Boolean = true,
    includeNoParensInSelectChains: Boolean = false,
    assumeStandardLibraryStripMargin: Boolean = false,
    danglingParentheses: DanglingParentheses = DanglingParentheses.default,
    @annotation.DeprecatedName(
      "poorMansTrailingCommasInConfigStyle",
      "Scala supports trailing commas after 2.12.2. Use trailingCommas instead",
      "2.5.0",
    )
    poorMansTrailingCommasInConfigStyle: Boolean = false,
    verticalMultiline: VerticalMultiline = VerticalMultiline(),
    verticalAlignMultilineOperators: Boolean = false,
    onTestFailure: String = "",
    encoding: Codec = "UTF-8",
    project: ProjectFiles = ProjectFiles(),
    fileOverride: Conf.Obj = Conf.Obj.empty,
    xmlLiterals: XmlLiterals = XmlLiterals(),
) {
  import ScalafmtConfig._

  private[scalafmt] lazy val alignMap: Map[String, Seq[AlignToken.Matcher]] =
    align.tokens.map(x => x.code -> x).toMap
      .map { case (k, v) => k -> v.getMatcher }

  private[scalafmt] def withDialect(dialect: NamedDialect): ScalafmtConfig =
    copy(runner = runner.withDialect(dialect))

  private[scalafmt] def withDialect(
      dialect: Option[NamedDialect],
  ): ScalafmtConfig = dialect.fold(this)(withDialect)

  def withDialect(dialect: Dialect, name: String): ScalafmtConfig =
    withDialect(NamedDialect(name, dialect))

  def withDialect(dialect: Dialect): ScalafmtConfig = withDialect(
    dialect,
    NamedDialect.getName(dialect).getOrElse("unknown dialect"),
  )

  private[scalafmt] def withFileOverride(conf: Conf.Obj): ScalafmtConfig =
    copy(fileOverride = conf)

  private[scalafmt] def withCompleteCallback(
      cb: FormatEvent.CompleteFormat => Unit,
  ): ScalafmtConfig = copy(runner = runner.withCompleteCallback(cb))

  // used by dynamic
  def needGitAutoCRLF: Boolean = project.git && lineEndings.isEmpty &&
    System.lineSeparator() == "\r\n"

  // used by dynamic; assumes `needGitAutoCRLF` was used before
  def withGitAutoCRLF(value: String): ScalafmtConfig = value.toLowerCase match {
    case "input" => withLineEndings(LineEndings.unix)
    case "true" => withLineEndings(LineEndings.windows)
    case "false" => withLineEndings(LineEndings.preserve)
    case _ => this
  }

  def withLineEndings(value: LineEndings): ScalafmtConfig =
    copy(lineEndings = Option(value))

  private lazy val forMain: ScalafmtConfig =
    if (project.layout.isEmpty) forTest
    else rewrite.forMainOpt.fold(this)(x => copy(rewrite = x))

  private lazy val forTest: ScalafmtConfig = rewrite.forTestOpt
    .fold(this)(x => copy(rewrite = x))

  def forSbt: ScalafmtConfig = rewrite.forSbtOpt
    .fold(this)(x => copy(rewrite = x))

  private lazy val expandedFileOverride = Try {
    val langPrefix = "lang:"
    val param = fileOverride.values.filter(_._1.nonEmpty)
    val hasLayout = project.layout.isDefined
    val patStyles = param.map { case (pat, conf) =>
      val isLang = hasLayout && pat.startsWith(langPrefix)
      val eitherPat =
        if (isLang) Left(pat.substring(langPrefix.length)) else Right(pat)
      val cfg = conf match {
        case x: Conf.Str => withDialect(NamedDialect.codec.read(None, x).get)
        case x =>
          val styleOpt = eitherPat.left.toOption
            .flatMap(lang => project.layout.map(_.withLang(lang, this)))
          decoder.read(styleOpt.orElse(Some(this)), x).get
      }
      eitherPat -> cfg
    }
    val langResult = patStyles.collect { case (Left(lang), cfg) => lang -> cfg }
    val fs = FileSystems.getDefault
    val pmResult = patStyles.collect { case (Right(pat), cfg) =>
      val pattern =
        if (pat(0) == '.') "glob:**" + pat else pat.inPathMatcherForm
      fs.getPathMatcher(pattern) -> cfg
    }
    (langResult, pmResult)
  }

  private def getConfigViaLayoutInfoFor(absfile: AbsoluteFile)(
      f: (ProjectFiles.Layout, String) => ScalafmtConfig,
  ): Option[ScalafmtConfig] = project.layout.flatMap { layout =>
    layout.getInfo(absfile).map { info =>
      val style = f(layout, info.lang)
      if (info.isTest) style.forTest else style.forMain
    }
  }

  def getConfigFor(filename: String): Try[ScalafmtConfig] = {
    val absfile = AbsoluteFile(filename)
    expandedFileOverride.map { case (langStyles, pmStyles) =>
      def langStyle = getConfigViaLayoutInfoFor(absfile) { (layout, lang) =>
        val style = langStyles.collectFirst { case (`lang`, x) => x }
        style.getOrElse(layout.withLang(lang, this))
      }
      val pmStyle = pmStyles.collectFirst {
        case (pm, style) if pm.matches(absfile.path) =>
          style.getConfigViaLayoutInfoFor(absfile) { (layout, lang) =>
            val sameDialect = style.dialect.isEquivalentTo(dialect)
            if (sameDialect) layout.withLang(lang, style) else style
          }.getOrElse(style)
      }
      pmStyle.orElse(langStyle).getOrElse(forTest)
    }
  }

  private[scalafmt] lazy val encloseSelectChains = optIn.encloseClassicChains ||
    newlines.source.ne(Newlines.classic)

  private[scalafmt] lazy val docstringsWrapMaxColumn: Int = docstrings
    .wrapMaxColumn.getOrElse(maxColumn)

  @inline
  private[scalafmt] def dialect = runner.getDialectForParser

  private[scalafmt] def getTrailingCommas = rewrite.trailingCommas.style

  // used in ScalafmtReflectConfig
  def hasRewrites: Boolean = rewrite.rewriteFactoryRules.nonEmpty ||
    FormatTokensRewrite.getEnabledFactories(this).nonEmpty

  // used in ScalafmtReflectConfig
  def withoutRewrites: ScalafmtConfig = copy(
    docstrings = docstrings.withoutRewrites,
    rewrite = rewrite.withoutRewrites,
  )

  def breakAfterInfix(tree: => Tree): Newlines.AfterInfix = newlines.afterInfix
    .getOrElse {
      val useSome = newlines.source == Newlines.classic &&
        tree.is[Type.ApplyInfix] && dialect.useInfixTypePrecedence
      if (useSome) Newlines.AfterInfix.some else newlines.breakAfterInfix
    }

  def formatInfix(tree: => Tree): Boolean = breakAfterInfix(tree) ne
    Newlines.AfterInfix.keep

  def getFewerBraces(): Indents.FewerBraces =
    if (indent.getSignificant < 2) Indents.FewerBraces.never
    else indent.fewerBraces

  def forScalaJs: ScalafmtConfig = copy(
    binPack = BinPack.always,
    danglingParentheses = DanglingParentheses(false, false),
    indent = Indents(callSite = 4, defnSite = 4),
    importSelectors = ImportSelectors.binPack,
    newlines = newlines.copy(
      avoidInResultType = true,
      neverBeforeJsNative = true,
      sometimesBeforeColonInMethodReturnType = false,
    ),
    // For some reason, the bin packing does not play nicely with forced
    // config style. It's fixable, but I don't want to spend time on it
    // right now.
    runner = runner.conservative,
    docstrings = docstrings.copy(style = Docstrings.Asterisk),
    align = align.copy(
      arrowEnumeratorGenerator = false,
      tokens = Seq(AlignToken.caseArrow),
      openParenCtrlSite = false,
    ),
  )

  def withAlign(align: Align): ScalafmtConfig = copy(align = align)

  def withAlign(tokens: AlignToken*): ScalafmtConfig = withAlign(
    align.copy(tokens = if (tokens.isEmpty) AlignToken.default else tokens),
  )

}

object ScalafmtConfig {
  implicit lazy val surface: generic.Surface[ScalafmtConfig] =
    generic.deriveSurface
  implicit lazy val encoder: ConfEncoder[ScalafmtConfig] = generic
    .deriveEncoder[ScalafmtConfig]

  implicit lazy val codecEncoder: ConfEncoder[Codec] = ConfEncoder.StringEncoder
    .contramap(_.name)

  val default = ScalafmtConfig()
  private[scalafmt] val uncheckedDefault = ScalafmtConfig(version = null)

  val intellij: ScalafmtConfig = default.copy(
    indent = Indents(callSite = 2, defnSite = 2),
    align = default.align.copy(openParenCallSite = false),
    newlines = default.newlines.copy(configStyle =
      default.newlines.configStyle
        .copy(fallBack = Newlines.ConfigStyleElement(prefer = false)),
    ),
    danglingParentheses = DanglingParentheses.shortcutTrue,
  )

  val defaultWithAlign: ScalafmtConfig = default.withAlign(Align.more)

  /** Experimental implementation of:
    * https://github.com/scala-js/scala-js/blob/master/CODINGSTYLE.md
    */
  val scalaJs: ScalafmtConfig = default.forScalaJs

  /** Ready styles provided by scalafmt.
    */
  private val activeStyles: Map[String, ScalafmtConfig] =
    Map("Scala.js" -> scalaJs, "IntelliJ" -> intellij) ++
      LoggerOps.name2style(default, defaultWithAlign)

  private val availableStyles: Map[String, ScalafmtConfig] = {
    activeStyles ++ LoggerOps.name2style(scalaJs)
  }.map { case (k, v) => k.toLowerCase -> v }

  private def readActiveStylePresets(conf: Conf): Configured[ScalafmtConfig] =
    (conf match {
      case Conf.Str(x) => availableStyles.get(x.toLowerCase)
          .map(style => Configured.ok(style))
      case _ => None
    }).getOrElse {
      val alternatives = activeStyles.keys.mkString(", ")
      val err = s"Unknown style $conf. Expected one of: $alternatives"
      Configured.error(err)
    }

  private def validate(cfg: ScalafmtConfig): Configured[ScalafmtConfig] = {
    // scalafmt: { maxColumn = 130 }
    import cfg._
    import ValidationOps._
    val errDialect = s" (no support in Scala dialect ${runner.dialectName})"
    val allErrors = new mutable.ArrayBuffer[String]
    locally {
      implicit val errors = new mutable.ArrayBuffer[String]
      if (newlines.sourceIgnored) {
        newlines.beforeOpenParenCallSite.fold(addIfDirect(
          newlines.configStyle.getParenCallSite.prefer && align.openParenCallSite,
          "newlines.configStyle.callSite && align.openParenCallSite && !newlines.beforeOpenParenCallSite",
        ))(x => addIfDirect(x.src eq Newlines.keep, "newlines.beforeOpenParenCallSite.src = keep"))
        newlines.beforeOpenParenDefnSite.fold(addIfDirect(
          newlines.configStyle.getParenDefnSite.prefer && align.openParenDefnSite,
          "newlines.configStyle.defnSite && align.openParenDefnSite && !newlines.beforeOpenParenDefnSite",
        ))(x => addIfDirect(x.src eq Newlines.keep, "newlines.beforeOpenParenDefnSite.src = keep"))
        def mustIgnoreSourceSplit(what: sourcecode.Text[Option[Newlines.IgnoreSourceSplit]]) = what.value
          .foreach(x => addIfDirect(!x.ignoreSourceSplit, s"${what.source}=$x"))
        mustIgnoreSourceSplit(newlines.beforeMultiline)
        mustIgnoreSourceSplit(newlines.beforeMultilineDef)
        addIf(newlines.beforeTypeBounds eq Newlines.keep)
        addIf(binPack.parentConstructors eq BinPack.ParentCtors.keep)
        addIf(newlines.selectChains.exists(_ eq Newlines.keep))
        addIf(getTrailingCommas.eq(TrailingCommas.keep))
      }
      if (newlines.source == Newlines.unfold) addIf(align.arrowEnumeratorGenerator)
      if (newlines.source != Newlines.classic) {
        addIf(optIn.breaksInsideChains)
        addIf(!includeCurlyBraceInSelectChains)
      }
      if (errors.nonEmpty) {
        allErrors += s"newlines.source=${newlines.source} and ["
        errors.foreach(x => allErrors += "\t" + x)
        allErrors += "]"
      }
    }
    locally {
      implicit val errors = allErrors
      if (!dialect.allowTrailingCommas) {
        addIf(getTrailingCommas eq TrailingCommas.always, errDialect)
        addIf(getTrailingCommas eq TrailingCommas.multiple, errDialect)
      }
      if (!dialect.allowSignificantIndentation) addIf(newlines.beforeOpenParenCallSite.nonEmpty, errDialect)
      addIfDirect( // can't use addIf on multiline conditions
        !(binPack.callSite == BinPack.Site.Never && binPack.defnSite == BinPack.Site.Never) && {
          newlines.implicitParamListModifierForce.nonEmpty || newlines.implicitParamListModifierPrefer.nonEmpty
        },
        "binPack.xxxSite && newlines.implicitParamListModifierXXX (not implemented)",
      )
      checkPositive(indent.main, indent.callSite, indent.defnSite, indent.commaSiteRelativeToExtends)
      checkNonNeg(indent.caseSite, indent.extendSite, indent.withSiteRelativeToExtends)
      checkPositiveOpt(indent.significant, indent.ctorSite)
      if (rewrite.scala3.insertEndMarkerMinLines != 0)
        addIf(rewrite.scala3.removeEndMarkerMaxLines >= rewrite.scala3.insertEndMarkerMinLines)
      addIf(rewrite.insertBraces.minLines != 0 && rewrite.scala3.insertEndMarkerMinLines != 0)
      addIf(rewrite.insertBraces.minLines != 0 && rewrite.scala3.removeOptionalBraces.oldSyntaxToo)
      if (rewrite.insertBraces.minLines != 0 && rewrite.rules.contains(RedundantBraces))
        addIf(rewrite.insertBraces.minLines < rewrite.redundantBraces.maxBreaks)
      addIf(align.beforeOpenParenDefnSite && !align.closeParenSite)
      addIf(align.beforeOpenParenCallSite && !align.closeParenSite)
      addIf(rewrite.scala3.removeOptionalBraces.fewerBracesMinSpan <= 0)
      if (rewrite.scala3.removeOptionalBraces.fewerBracesMaxSpan != 0) {
        addIf(rewrite.scala3.removeOptionalBraces.fewerBracesMaxSpan < 0)
        addIf(rewrite.scala3.removeOptionalBraces.fewerBracesMinSpan > rewrite.scala3.removeOptionalBraces.fewerBracesMaxSpan)
      }
    }
    // scalafmt: {}
    if (allErrors.isEmpty) Configured.ok(cfg)
    else Configured.error(allErrors.mkString("can't use: [\n\t", "\n\t", "\n]"))
  }

  private val baseDecoder = generic.deriveDecoderEx(default).noTypos
    .detectSectionRenames

  implicit final val decoder: ConfDecoderEx[ScalafmtConfig] =
    (stateOpt, conf) => {
      val stylePreset = conf match {
        case x: Conf.Obj =>
          val section = Seq(Presets.presetKey, "style")
            .flatMap(y => x.field(y).map(y -> _))
          section.headOption.map { case (field, obj) =>
            obj -> Conf.Obj((x.map - field).toList)
          }
        case _ => None
      }
      val parsed = stylePreset match {
        case Some((styleConf, restConf)) => readActiveStylePresets(styleConf)
            .andThen { x =>
              val preset = stateOpt.fold(x) { state =>
                val isDefaultDialect = x.runner.isDefaultDialect
                val dialect =
                  (if (isDefaultDialect) state else x).runner.dialect
                x.copy(
                  version = state.version,
                  runner = x.runner.withParser(state.runner.parser)
                    .withDialect(dialect),
                )
              }
              baseDecoder.read(Some(preset), restConf)
            }
        case _ => baseDecoder.read(stateOpt, conf)
      }
      val res = parsed.andThen(validate)
      res
    }

  def fromHoconString(
      string: String,
      default: ScalafmtConfig = ScalafmtConfig.default,
      path: Option[String] = None,
  ): Configured[ScalafmtConfig] =
    fromConf(ConfParsed.fromString(string, path), default = default)

  /** Read ScalafmtConfig from String contents from an optional HOCON path. */
  def fromHoconFile(
      file: Path,
      default: ScalafmtConfig = ScalafmtConfig.default,
      path: Option[String] = None,
  ): Configured[ScalafmtConfig] =
    fromConf(ConfParsed.fromPath(file, path), default = default)

  def fromConf(
      parsed: ConfParsed,
      default: ScalafmtConfig,
  ): Configured[ScalafmtConfig] =
    ScalafmtConfig.decoder.read(Option(default), parsed.conf) match {
      case Configured.Ok(x)
          if default.version == null && x.version != Versions.stable &&
            x.version != Versions.version =>
        val version = Option(x.version).getOrElse("missing")
        val expected = s"${Versions.stable} or ${Versions.version}"
        Configured.error(s"version [expected $expected]: $version")
      case Configured.Ok(x)
          if default.eq(ScalafmtConfig.uncheckedDefault) &&
            x.runner.isDefaultDialect =>
        Configured.error(NamedDialect.getUnknownError)
      case x => x
    }

}
