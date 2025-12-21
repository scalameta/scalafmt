package org.scalafmt

import org.scalafmt.Error.PreciseIncomplete
import org.scalafmt.config._
import org.scalafmt.internal._
import org.scalafmt.rewrite.Rewrite
import org.scalafmt.sysops.FileOps
import org.scalafmt.util.{LoggerOps, MarkdownParser}

import scala.meta.parsers.ParseException
import scala.meta.tokenizers.TokenizerOptions
import scala.meta.{Input, dialects}

import java.nio.file.Path

import scala.util._

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
  ): Formatted = formatCode(code, style, range, filename).formatted match {
    case Formatted.Failure(Error.WithCode(ex, _)) => Formatted.Failure(ex)
    case x => x
  }

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
        else sbtStyle.withDialect(dialects.Sbt, "sbt")
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
    if (FileOps.isMarkdown(file)) {
      val mdocStyle = style.withLineEndings(LineEndings.keep)
      val res = MarkdownParser
        .transformMdoc(code)(doFormatOne(_, mdocStyle, file, range))
      style.lineEndings match {
        case Some(LineEndings.unix) => res.map(LoggerOps.lf)
        case Some(LineEndings.windows) => res.map(LoggerOps.crlf)
        case _ => res
      }
    } else doFormatOne(code, style, file, range)

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
    val original = codeToInput(code)
    val rewritten = Rewrite(original, style)
    runner.parse(rewritten.fold(original)(codeToInput)).fold(
      x => {
        val err = x.details match {
          case ParseException(pos, msg) =>
            ParseException(pos, s"[dialect ${runner.dialectName}] $msg")
          case ed => ed
        }
        Failure(Error.WithCode(err, rewritten.getOrElse(code)))
      },
      tree => {
        implicit val formatOps = new FormatOps(tree, style, file)
        runner.event(FormatEvent.CreateFormatOps(formatOps))
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
  ): Formatted = format(code, style, range, defaultFilename)

  // used by ScalafmtReflect.parseConfig
  def parseHoconConfigFile(configPath: Path): Configured[ScalafmtConfig] =
    ScalafmtConfig.fromHoconFile(configPath, ScalafmtConfig.uncheckedDefault)

  // used by ScalafmtReflect.parseConfig
  def parseHoconConfig(configString: String): Configured[ScalafmtConfig] =
    ScalafmtConfig.fromHoconString(configString, ScalafmtConfig.uncheckedDefault)

}
