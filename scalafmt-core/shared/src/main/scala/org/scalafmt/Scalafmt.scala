package org.scalafmt

import metaconfig.Configured
import scala.meta.Dialect
import scala.meta.inputs.Input
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import org.scalafmt.config.Config
import org.scalafmt.Error.PreciseIncomplete
import org.scalafmt.config.FormatEvent.CreateFormatOps
import org.scalafmt.config.LineEndings.preserve
import org.scalafmt.config.LineEndings.windows
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.BestFirstSearch
import org.scalafmt.internal.FormatOps
import org.scalafmt.internal.FormatWriter
import org.scalafmt.rewrite.Rewrite

/**
  * WARNING. This API is discouraged when integrating with Scalafmt from a build tool
  * or editor plugin. It is recommended to use the `scalafmt-dynamic` module instead.
  */
object Scalafmt {

  private val WindowsLineEnding = "\r\n"
  private val UnixLineEnding = "\n"
  private val defaultFilename = "<input>"

  // XXX: don't modify signature, scalafmt-dynamic expects it via reflection
  /**
    * Format Scala code using scalafmt.
    *
    * WARNING. This API is discouraged when integrating with Scalafmt from a build tool
    * or editor plugin. It is recommended to use the `scalafmt-dynamic` module instead.
    *
    * @param code Code string to format.
    * @param style Configuration for formatting output.
    * @param range EXPERIMENTAL. Format a subset of lines.
    * @return [[Formatted.Success]] if successful,
    *         [[Formatted.Failure]] otherwise. If you are OK with throwing
    *         exceptions, use [[Formatted.Success.get]] to get back a
    *         string.
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
      baseStyle: ScalafmtConfig = ScalafmtConfig.default,
      range: Set[Range] = Set.empty,
      filename: String = defaultFilename
  ): Formatted.Result = {
    val style =
      if (filename == defaultFilename) baseStyle
      else baseStyle.getConfigFor(filename) // might throw for invalid conf
    val runner = style.runner
    Try {
      if (code.matches("\\s*")) System.lineSeparator()
      else {
        val isWindows = containsWindowsLineEndings(code)
        val unixCode = if (isWindows) {
          code.replaceAll(WindowsLineEnding, UnixLineEnding)
        } else {
          code
        }
        val toParse = Rewrite(Input.VirtualFile(filename, unixCode), style)
        val tree = runner.parse(toParse).get
        val formatOps = new FormatOps(tree, style, filename)
        runner.event(CreateFormatOps(formatOps))
        val formatWriter = new FormatWriter(formatOps)
        val partial = BestFirstSearch(formatOps, range, formatWriter)
        val formattedString = formatWriter.mkString(partial.state)
        val correctedFormattedString =
          if (
            (style.lineEndings == preserve && isWindows) ||
            style.lineEndings == windows
          ) {
            formattedString.replaceAll(UnixLineEnding, WindowsLineEnding)
          } else {
            formattedString
          }
        if (partial.reachedEOF) {
          correctedFormattedString
        } else {
          val pos = formatOps.tokens(partial.state.depth).left.pos
          throw PreciseIncomplete(pos, correctedFormattedString)
        }
      }
    } match {
      case Failure(e) => Formatted.Result(Formatted.Failure(e), style)
      case Success(s) => Formatted.Result(Formatted.Success(s), style)
    }
  }

  // XXX: don't modify signature, scalafmt-dynamic expects it via reflection
  def format(
      code: String,
      style: ScalafmtConfig = ScalafmtConfig.default,
      range: Set[Range] = Set.empty[Range]
  ): Formatted = {
    formatCode(code, style, range).formatted
  }

  def parseHoconConfig(configString: String): Configured[ScalafmtConfig] =
    Config.fromHoconString(configString, None)

  private[this] def containsWindowsLineEndings(code: String): Boolean =
    code.contains(WindowsLineEnding)

  /** Utility method to change dialect on ScalafmtConfig.
    *
    * Binary compatibility is guaranteed between releases, unlike with ScalafmtConfig.copy.
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
