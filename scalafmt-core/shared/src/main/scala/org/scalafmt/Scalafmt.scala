package org.scalafmt

import java.nio.file.Path

import metaconfig.Configured
import scala.annotation.tailrec
import scala.meta.Dialect
import scala.meta.Input
import scala.meta.parsers.ParseException
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import org.scalafmt.config.Config
import org.scalafmt.Error.PreciseIncomplete
import org.scalafmt.config.FormatEvent.CreateFormatOps
import org.scalafmt.config.LineEndings
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.BestFirstSearch
import org.scalafmt.internal.FormatOps
import org.scalafmt.internal.FormatWriter
import org.scalafmt.rewrite.Rewrite
import org.scalafmt.util.{FileOps, MarkdownFile, MarkdownPart}
import org.scalafmt.internal.RegexCompat

/** WARNING. This API is discouraged when integrating with Scalafmt from a build
  * tool or editor plugin. It is recommended to use the `scalafmt-dynamic`
  * module instead.
  */
object Scalafmt {

  private val WinLineEnding = "\r\n"
  private val UnixLineEnding = "\n"
  private val defaultFilename = "<input>"

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
      filename: String
  ): Formatted = {
    formatCode(code, style, range, filename).formatted
  }

  private[scalafmt] def formatCode(
      code: String,
      baseStyle: ScalafmtConfig = ScalafmtConfig.uncheckedDefault,
      range: Set[Range] = Set.empty,
      filename: String = defaultFilename
  ): Formatted.Result = {
    val style =
      if (filename == defaultFilename) baseStyle
      else { // might throw for invalid conf
        val style = baseStyle.getConfigFor(filename)
        val isSbt = FileOps.isAmmonite(filename) || FileOps.isSbt(filename) ||
          FileOps.isMarkdown(filename)
        if (isSbt) style.forSbt else style
      }
    val isWin = code.contains(WinLineEnding)
    val unixCode =
      if (isWin) code.replaceAll(WinLineEnding, UnixLineEnding) else code
    doFormat(unixCode, style, filename, range) match {
      case Failure(e) => Formatted.Result(Formatted.Failure(e), style)
      case Success(s) =>
        val asWin = style.lineEndings == LineEndings.windows ||
          (isWin && style.lineEndings == LineEndings.preserve)
        val res = if (asWin) s.replaceAll(UnixLineEnding, WinLineEnding) else s
        Formatted.Result(Formatted.Success(res), style)
    }
  }

  private def flatMapAll[A, B](xs: Iterator[A])(f: A => Try[B]): Try[Seq[B]] = {
    @tailrec
    def iter(res: Seq[B]): Try[Seq[B]] =
      if (!xs.hasNext) Success(res)
      else
        f(xs.next()) match {
          case Success(x) => iter(res :+ x)
          case Failure(e) => Failure(e)
        }
    iter(Seq.empty)
  }

  private def doFormat(
      code: String,
      style: ScalafmtConfig,
      file: String,
      range: Set[Range] = Set.empty
  ): Try[String] =
    if (FileOps.isAmmonite(file)) {
      // XXX: we won't support ranges as we don't keep track of lines
      val chunks = RegexCompat.splitByAmmonitePattern(code)
      if (chunks.length <= 1) doFormatOne(code, style, file, range)
      else
        flatMapAll(chunks.iterator)(doFormatOne(_, style, file))
          .map(_.mkString("\n@\n"))
    } else if (FileOps.isMarkdown(file)) {
      val markdown = MarkdownFile.parse(Input.VirtualFile(file, code))

      val resultIterator: Iterator[Try[String]] =
        markdown.parts.iterator.collect {
          case fence: MarkdownPart.CodeFence
              if fence.info.startsWith("scala mdoc") =>
            val res = doFormatOne(fence.body, style, file)
            res.foreach { formatted =>
              fence.newBody = Some(formatted.trim)
            }
            res
        }
      if (resultIterator.isEmpty) Success(code)
      else
        resultIterator
          .find(_.isFailure)
          .getOrElse(Success(markdown.renderToString))
    } else
      doFormatOne(code, style, file, range)

  private def doFormatOne(
      code: String,
      style: ScalafmtConfig,
      file: String,
      range: Set[Range] = Set.empty
  ): Try[String] =
    if (code.matches("\\s*")) Try("\n")
    else {
      val runner = style.runner
      val parsed = runner.parse(Rewrite(Input.VirtualFile(file, code), style))
      parsed.fold(
        _.details match {
          case ed: ParseException =>
            val dialect = runner.dialect.source
            val msg = s"[dialect $dialect] ${ed.shortMessage}"
            Failure(new ParseException(ed.pos, msg))
          case ed => Failure(ed)
        },
        tree => {
          val formatOps = new FormatOps(tree, style, file)
          runner.event(CreateFormatOps(formatOps))
          val formatWriter = new FormatWriter(formatOps)
          Try(BestFirstSearch(formatOps, range, formatWriter)).flatMap { res =>
            val formattedString = formatWriter.mkString(res.state)
            if (res.reachedEOF) Success(formattedString)
            else {
              val pos = formatOps.tokens(res.state.depth).left.pos
              Failure(PreciseIncomplete(pos, formattedString))
            }
          }
        }
      )
    }

  // XXX: don't modify signature, scalafmt-dynamic expects it via reflection
  def format(
      code: String,
      style: ScalafmtConfig = ScalafmtConfig.default,
      range: Set[Range] = Set.empty[Range]
  ): Formatted = {
    formatCode(code, style, range).formatted
  }

  // used by ScalafmtReflect.parseConfig
  def parseHoconConfigFile(configPath: Path): Configured[ScalafmtConfig] =
    Config.fromHoconFile(configPath.toFile, ScalafmtConfig.uncheckedDefault)

  // used by ScalafmtReflect.parseConfig
  def parseHoconConfig(configString: String): Configured[ScalafmtConfig] =
    Config.fromHoconString(configString, ScalafmtConfig.uncheckedDefault)

  /** Utility method to change dialect on ScalafmtConfig.
    *
    * Binary compatibility is guaranteed between releases, unlike with
    * ScalafmtConfig.copy.
    */
  def configWithDialect(
      config: ScalafmtConfig,
      dialect: Dialect
  ): ScalafmtConfig =
    config.withDialect(dialect)

  def configForSbt(
      config: ScalafmtConfig
  ): ScalafmtConfig =
    config.forSbt
}
