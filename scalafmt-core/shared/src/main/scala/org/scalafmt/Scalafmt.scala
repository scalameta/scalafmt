package org.scalafmt

import org.scalafmt.Error.PreciseIncomplete
import org.scalafmt.config.FormatEvent.CreateFormatOps
import org.scalafmt.config.NamedDialect
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.BestFirstSearch
import org.scalafmt.internal.FormatOps
import org.scalafmt.internal.FormatWriter
import org.scalafmt.rewrite.Rewrite
import org.scalafmt.sysops.FileOps
import org.scalafmt.util.MarkdownParser

import scala.meta.Input
import scala.meta.dialects
import scala.meta.parsers.ParseException
import scala.meta.tokenizers.TokenizerOptions

import java.nio.file.Path

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import metaconfig.Configured

/** WARNING. This API is discouraged when integrating with Scalafmt from a build
  * tool or editor plugin. It is recommended to use the `scalafmt-dynamic`
  * module instead.
  */
object Scalafmt {

  private val defaultFilename = "<input>"
  private implicit val tokenizerOptions: TokenizerOptions =
    new TokenizerOptions(groupWhitespace = true)

  // XXX: don't modify signature, scalafmt-dynamic expects it via reflection
  /** Format Scala code using scalafmt.
    *
    * WARNING. This API is discouraged when integrating with Scalafmt from a
    * build tool or editor plugin. It is recommended to use the
    * `scalafmt-dynamic` module instead.
    *
    * @param code
    *   Code string to format.
    * @param style
    *   Configuration for formatting output.
    * @param range
    *   EXPERIMENTAL. Format a subset of lines.
    * @return
    *   [[Formatted.Success]] if successful, [[Formatted.Failure]] otherwise. If
    *   you are OK with throwing exceptions, use [[Formatted.Success.get]] to
    *   get back a string.
    */
  def format(
      code: String,
      style: ScalafmtConfig,
      range: Set[Range],
      filename: String,
  ): Formatted = formatCode(code, style, range, filename).formatted

  private[scalafmt] def formatCode(
      code: String,
      baseStyle: ScalafmtConfig = ScalafmtConfig.uncheckedDefault,
      range: Set[Range] = Set.empty,
      filename: String = defaultFilename,
  ): Formatted.Result = {
    def getStyleByFile(style: ScalafmtConfig) = {
      val isSbt = FileOps.isSbt(filename)
      if (isSbt) {
        val sbtStyle = style.forSbt
        if (sbtStyle.dialect ne baseStyle.dialect) sbtStyle
        else sbtStyle.withDialect(NamedDialect(dialects.Sbt))
      } else style
    }
    val styleTry =
      if (filename == defaultFilename) Success(baseStyle)
      else baseStyle.getConfigFor(filename).map(getStyleByFile)
    styleTry.fold(
      Formatted.Result(_, baseStyle),
      x => Formatted.Result(doFormat(code, x, filename, range), x),
    )
  }

  private def doFormat(
      code: String,
      style: ScalafmtConfig,
      file: String,
      range: Set[Range],
  ): Try[String] =
    if (FileOps.isMarkdown(file)) MarkdownParser
      .transformMdoc(code)(doFormatOne(_, style, file, range))
    else doFormatOne(code, style, file, range)

  private[scalafmt] def toInput(code: String, file: String): Input = {
    val fileInput = Input.VirtualFile(file, code).withTokenizerOptions
    if (FileOps.isAmmonite(file)) Input.Ammonite(fileInput) else fileInput
  }

  private def doFormatOne(
      code: String,
      style: ScalafmtConfig,
      file: String,
      range: Set[Range],
  ): Try[String] = {
    val runner = style.runner
    val codeToInput: String => Input = toInput(_, file)
    val parsed = runner.parse(Rewrite(codeToInput(code), style, codeToInput))
    parsed.fold(
      _.details match {
        case ed: ParseException =>
          val dialect = runner.dialectName
          val msg = s"[dialect $dialect] ${ed.shortMessage}"
          Failure(new ParseException(ed.pos, msg))
        case ed => Failure(ed)
      },
      tree => {
        implicit val formatOps = new FormatOps(tree, style, file)
        runner.event(CreateFormatOps(formatOps))
        implicit val formatWriter = new FormatWriter(formatOps)
        Try(BestFirstSearch(range)).flatMap { res =>
          val formattedString = formatWriter.mkString(res.state)
          if (res.reachedEOF) Success(formattedString)
          else {
            val pos = formatOps.tokens(res.state.depth).left.pos
            Failure(PreciseIncomplete(pos, formattedString))
          }
        }
      },
    )
  }

  // XXX: don't modify signature, scalafmt-dynamic expects it via reflection
  def format(
      code: String,
      style: ScalafmtConfig = ScalafmtConfig.default,
      range: Set[Range] = Set.empty[Range],
  ): Formatted = formatCode(code, style, range).formatted

  // used by ScalafmtReflect.parseConfig
  def parseHoconConfigFile(configPath: Path): Configured[ScalafmtConfig] =
    ScalafmtConfig.fromHoconFile(configPath, ScalafmtConfig.uncheckedDefault)

  // used by ScalafmtReflect.parseConfig
  def parseHoconConfig(configString: String): Configured[ScalafmtConfig] =
    ScalafmtConfig.fromHoconString(configString, ScalafmtConfig.uncheckedDefault)

}
