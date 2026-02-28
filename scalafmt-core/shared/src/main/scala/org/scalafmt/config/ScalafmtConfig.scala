package org.scalafmt.config

import org.scalafmt.Versions
import org.scalafmt.rewrite._
import org.scalafmt.sysops._
import org.scalafmt.util._

import scala.meta._
import scala.meta.tokens.{Token => T}

import java.nio.file.Path

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
  * @param lineEndings
  *   - If [[LineEndings.unix]], output will include only unix line endings
  *   - If [[LineEndings.windows]], output will include only windows line
  *     endings
  *   - If [[LineEndings.keep]], output will include endings included in
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
// scalafmt: { maxColumn = 120 }
@annotation.SectionRename("optIn.configStyleArguments", "newlines.configStyle.fallBack.prefer") // v3.8.2
@annotation.SectionRename("trailingCommas", "rewrite.trailingCommas.style") // v3.0.5
@annotation.SectionRename("poorMansTrailingCommasInConfigStyle", "newlines.configStyle.beforeComma") // v3.8.4
@annotation.SectionRename("optIn.forceBlankLineBeforeDocstring", "docstrings.forceBlankLineBefore") // v3.4.0
@annotation.SectionRename("rewriteTokens", "rewrite.tokens") // v3.8.4
@annotation.SectionRename("importSelectors", "binPack.importSelectors") // v3.8.4
@annotation.SectionRename("binPackImportSelectors", "binPack.importSelectors") // v3.8.4
// select chains
@annotation.SectionRename("optIn.encloseClassicChains", "newlines.selectChains.enclose") // v3.8.4
@annotation.SectionRename("optIn.breakChainOnFirstMethodDot", "newlines.selectChains.classicKeepFirst") // v3.8.4
@annotation.SectionRename("optIn.breaksInsideChains", "newlines.selectChains.classicKeepAfterFirstBreak") // v3.8.4
@annotation.SectionRename("includeNoParensInSelectChains", "newlines.selectChains.classicCanStartWithoutApply") // v3.8.4
@annotation.SectionRename("includeCurlyBraceInSelectChains", "newlines.selectChains.classicCanStartWithBraceApply") // v3.8.4
// annotations
@annotation.SectionRename("optIn.annotationNewlines", "newlines.annotation") // v3.8.4
@annotation.SectionRename("optIn.selfAnnotationNewline", "newlines.selfAnnotation") // v3.8.4
// indent
@annotation.SectionRename("continuationIndent", "indent") // v3.8.5
@annotation.SectionRename("indentOperator", "indent.infix") // v3.8.4
@annotation.SectionRename("verticalAlignMultilineOperators", "indent.infix", IndentOperator.boolToAssign) // v3.8.4
@annotation.SectionRename("indentYieldKeyword", "indent.yieldKeyword") // v3.8.4
// scalafmt: { maxColumn = 80 }
case class ScalafmtConfig(
    version: String = org.scalafmt.Versions.stable,
    maxColumn: Int = 80,
    docstrings: Docstrings = Docstrings(),
    comments: Comments = Comments(),
    binPack: BinPack = BinPack(),
    indent: Indents = Indents(),
    align: Align = Align(),
    spaces: Spaces = Spaces(),
    literals: Literals = Literals.default,
    lineEndings: Option[LineEndings] = None,
    rewrite: RewriteSettings = RewriteSettings.default,
    newlines: Newlines = Newlines(),
    runner: RunnerSettings = RunnerSettings.default,
    assumeStandardLibraryStripMargin: Boolean = false,
    danglingParentheses: DanglingParentheses = DanglingParentheses.default,
    verticalMultiline: VerticalMultiline = VerticalMultiline(),
    onTestFailure: String = "",
    encoding: Codec = "UTF-8",
    project: ProjectFiles = ProjectFiles(),
    fileOverride: Conf.Obj = Conf.Obj.empty,
    xmlLiterals: XmlLiterals = XmlLiterals(),
    private val formatOn: List[String] = ScalafmtConfig.defaultFormatOn,
    private val formatOff: List[String] = ScalafmtConfig.defaultFormatOff,
) {
  import ScalafmtConfig._

  private[scalafmt] lazy val alignMap: Map[String, Seq[TreePattern.Matcher]] =
    align.tokens.map(x => x.code -> x).toMap.map { case (k, v) =>
      k -> v.getMatcher
    }

  def withDialect(nd: NamedDialect): ScalafmtConfig =
    copy(runner = runner.withDialect(nd))

  def withDialect(nd: Option[NamedDialect]): ScalafmtConfig = nd
    .fold(this)(withDialect)

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
    case "false" => withLineEndings(LineEndings.keep)
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
    // longest pattern first
    val param = fileOverride.values.filter(_._1.nonEmpty).sortBy(-_._1.length)
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
    val pmResult = patStyles.collect { case (Right(pat), cfg) =>
      val pattern =
        if (pat(0) == '.') "glob:**" + pat else OsSpecific.inPathMatcherForm(pat)
      PlatformPathMatcher(pattern) -> cfg
    }
    (langResult, pmResult)
  }

  private def getConfigViaLayoutInfoFor(absfile: AbsoluteFile)(
      f: (ProjectFiles.Layout, String) => ScalafmtConfig,
  ): Option[ScalafmtConfig] = project.layout.flatMap(layout =>
    layout.getInfo(absfile).map { info =>
      val style = f(layout, info.lang)
      if (info.isTest) style.forTest else style.forMain
    },
  )

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
          }.getOrElse(style.forMain)
      }
      pmStyle.getOrElse(langStyle.getOrElse(this).forMain)
    }
  }

  private[scalafmt] lazy val docstringsWrapMaxColumn: Int = docstrings
    .wrapMaxColumn.getOrElse(maxColumn)

  @inline
  private[scalafmt] implicit def dialect: Dialect = runner.getDialectForParser

  private[scalafmt] def getTrailingCommas = rewrite.trailingCommas.style

  // used in ScalafmtReflectConfig
  def hasRewrites: Boolean = rewrite.rewriteFactoryRules.nonEmpty ||
    FormatTokensRewrite.getEnabledFactories(this).nonEmpty

  // used in ScalafmtReflectConfig
  def withoutRewrites: ScalafmtConfig = copy(
    docstrings = docstrings.withoutRewrites,
    rewrite = rewrite.withoutRewrites,
  )

  def getFewerBraces(): Indents.FewerBraces =
    if (indent.getSignificant < 2) Indents.FewerBraces.never
    else indent.fewerBraces

  def forScalaJs: ScalafmtConfig = copy(
    binPack = BinPack.always,
    danglingParentheses = DanglingParentheses(false, false),
    indent = Indents(callSite = 4, defnSite = 4),
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

  @inline
  def isFormatOn(token: T.Comment): Boolean = isFormatIn(token, formatOn)
  @inline
  def isFormatOn(token: T): Boolean = isFormatIn(token, formatOn)

  @inline
  def isFormatOff(token: T.Comment): Boolean = isFormatIn(token, formatOff)
  @inline
  def isFormatOff(token: T): Boolean = isFormatIn(token, formatOff)

  def importSelectorsRewrite: Newlines.SourceHints = rewrite.imports.selectors
    .getOrElse(newlines.source)

  def importSelectorsBinPack: ImportSelectors = binPack.importSelectors
    .getOrElse(newlines.source match {
      case Newlines.fold => ImportSelectors.fold
      case Newlines.keep => ImportSelectors.keep
      case Newlines.unfold => ImportSelectors.unfold
      case _ => null
    })

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

  private def checkErrors(cfg: ScalafmtConfig): Configured[ScalafmtConfig] = {
    // scalafmt: { maxColumn = 140 }
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
        addIf(binPack.importSelectors.orNull eq ImportSelectors.keep)
        addIf(newlines.selectChains.style.orNull eq Newlines.keep)
        addIf(getTrailingCommas.eq(TrailingCommas.keep))
        addIfDirect(!newlines.infix.sourceIgnoredIfSet, "newlines.infix.XXX.style is keep; reduce maxCountPerFile instead")
      }
      if (newlines.source == Newlines.unfold) addIf(align.arrowEnumeratorGenerator)
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
        !(binPack.callSite == BinPack.Site.Never && binPack.defnSite == BinPack.Site.Never) &&
          { newlines.implicitParamListModifierForce.nonEmpty || newlines.implicitParamListModifierPrefer.nonEmpty },
        "binPack.xxxSite && newlines.implicitParamListModifierXXX (not implemented)",
      )
      checkPositive(indent.main, indent.callSite, indent.defnSite, indent.commaSiteRelativeToExtends)
      checkNonNeg(indent.caseSite, indent.extendSite, indent.withSiteRelativeToExtends)
      checkPositiveOpt(indent.significant, indent.ctorSite)
      if (rewrite.scala3.endMarker.insertMinSpan != 0)
        addIf(rewrite.scala3.endMarker.removeMaxSpan >= rewrite.scala3.endMarker.insertMinSpan)
      addIf(rewrite.insertBraces.settings.minBreaks != 0 && rewrite.scala3.endMarker.insertMinSpan != 0)
      addIf(rewrite.insertBraces.settings.minBreaks != 0 && rewrite.scala3.removeOptionalBraces.oldSyntaxToo)
      if (RedundantBraces.usedIn(rewrite)) {
        if (rewrite.insertBraces.settings.minBreaks != 0) addIf(rewrite.insertBraces.settings.minBreaks <= rewrite.redundantBraces.maxBreaks)
        if (rewrite.redundantBraces.oneStatApply.bracesMinSpan >= 0)
          addIf(rewrite.redundantBraces.oneStatApply.bracesMinSpan < rewrite.redundantBraces.oneStatApply.parensMaxSpan)
      }
      addIf(align.beforeOpenParenDefnSite && !align.closeParenSite)
      addIf(align.beforeOpenParenCallSite && !align.closeParenSite)
      if (rewrite.scala3.removeOptionalBraces.fewerBracesMaxSpan > 0) {
        addIf(rewrite.scala3.removeOptionalBraces.fewerBracesMinSpan > rewrite.scala3.removeOptionalBraces.fewerBracesMaxSpan)
        if (rewrite.scala3.removeOptionalBraces.removeBracesMaxSpan > 0)
          addIf(rewrite.scala3.removeOptionalBraces.removeBracesMaxSpan < rewrite.scala3.removeOptionalBraces.fewerBracesMaxSpan)
      }
      if (rewrite.rules.contains(Imports)) binPack.importSelectors match {
        case Some(ImportSelectors.singleLine) => // if we fold but not bin pack, we might end up with very long lines
          addIfDirect(importSelectorsRewrite eq Newlines.fold, "rewrite.imports.selectors == fold && binPack.importSelectors == singleLine")
        case Some(ImportSelectors.keep) =>
          val isEnabled = cfg.importSelectorsRewrite.ignoreSourceSplit || !cfg.rewrite.imports.noGroups
          addIfDirect(isEnabled, "binPack.importSelectors == keep and rewrite.rules enables Imports")
        case _ =>
      }
    }
    // scalafmt: {}
    if (allErrors.isEmpty) Configured.ok(cfg)
    else Configured.error(allErrors.mkString("can't use: [\n\t", "\n\t", "\n]"))
  }

  private def validateImports(cfg: ScalafmtConfig): Configured[ScalafmtConfig] =
    Configured.Ok {
      val binPackWouldBeKeep = cfg.binPack.importSelectors.isEmpty &&
        cfg.newlines.keep && cfg.rewrite.rules.contains(Imports)
      if (binPackWouldBeKeep) { // check rule is enabled
        val selectors = cfg.importSelectorsRewrite
        if (selectors.ignoreSourceSplit || !cfg.rewrite.imports.noGroups) {
          val fold = selectors eq Newlines.fold
          val bp = if (fold) ImportSelectors.fold else ImportSelectors.unfold
          cfg.copy(binPack = cfg.binPack.copy(importSelectors = Some(bp)))
        } else cfg
      } else cfg
    }

  private val validations: Seq[ScalafmtConfig => Configured[ScalafmtConfig]] =
    Seq(checkErrors, validateImports)

  private val baseDecoder = generic.deriveDecoderEx(default).noTypos
    .detectSectionRenames

  implicit final val decoder: ConfDecoderEx[ScalafmtConfig] =
    new ConfDecoderEx[ScalafmtConfig] {
      override def read(
          state: Option[ScalafmtConfig],
          conf: Conf,
      ): Configured[ScalafmtConfig] = {
        val stylePreset = conf match {
          case x: Conf.Obj =>
            val section = Seq(Presets.presetKey, "style")
              .flatMap(y => x.field(y).map(y -> _))
            section.headOption.map { case (field, obj) =>
              obj -> Conf.Obj(x.values.filter { case (k, _) => k != field })
            }
          case _ => None
        }
        val parsed = stylePreset match {
          case Some((styleConf, restConf)) => readActiveStylePresets(styleConf)
              .andThen { x =>
                val preset = state.fold(x) { state =>
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
          case _ => baseDecoder.read(state, conf)
        }
        validations.foldLeft(parsed)(_ andThen _)
      }

      override def convert(conf: Conf): Conf = baseDecoder.convert(conf) match {
        case c @ Conf.Obj(elems) =>
          val fileOverrideKey = Conf.nameOf(default.fileOverride).value
          elems.collectFirst {
            case (`fileOverrideKey`, Conf.Obj(vv)) if vv.nonEmpty =>
              val fo = fileOverrideKey -> Conf.Obj(vv.map {
                case (k, v: Conf.Obj) => k -> baseDecoder.convert(v)
                case x => x
              })
              Conf.Obj(fo :: elems.filter(_._1 != fileOverrideKey))
          }.getOrElse(c)
        case c => c
      }
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

  private lazy val (defaultFormatOn, defaultFormatOff) = {
    val prefixes = List(
      "@formatter:", // IntelliJ
      "format: ", // scalariform
    )
    (prefixes.map(_ + "on"), prefixes.map(_ + "off"))
  }

  @inline
  private def isFormatIn(token: T.Comment, set: List[String]): Boolean = set
    .contains(token.value.trim.toLowerCase)

  private def isFormatIn(token: T, set: List[String]): Boolean = token match {
    case t: T.Comment => isFormatIn(t, set)
    case _ => false
  }

}
