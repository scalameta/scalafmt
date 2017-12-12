package org.scalafmt

import scala.meta.Dialect
import scala.meta.Input.stringToInput
import scala.meta.inputs.Input
import scala.util.control.NonFatal
import org.scalafmt.Error.Incomplete
import org.scalafmt.config.FormatEvent.CreateFormatOps
import org.scalafmt.config.LineEndings.preserve
import org.scalafmt.config.LineEndings.windows
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.BestFirstSearch
import org.scalafmt.internal.FormatOps
import org.scalafmt.internal.FormatWriter
import org.scalafmt.rewrite.Rewrite

object Scalafmt {

  private val WindowsLineEnding = "\r\n"
  private val UnixLineEnding = "\n"

  /**
    * Format Scala code using scalafmt.
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
      style: ScalafmtConfig = ScalafmtConfig.default,
      range: Set[Range] = Set.empty[Range]): Formatted = {
    try {
      val runner = style.runner
      if (code.matches("\\s*")) Formatted.Success(System.lineSeparator())
      else {
        val isWindows = containsWindowsLineEndings(code)
        val unixCode = if (isWindows) {
          code.replaceAll(WindowsLineEnding, UnixLineEnding)
        } else {
          code
        }
        val toParse = Rewrite(Input.String(unixCode), style)
        val tree = runner.dialect(toParse).parse(runner.parser).get
        val formatOps = new FormatOps(tree, style)
        runner.eventCallback(CreateFormatOps(formatOps))
        val formatWriter = new FormatWriter(formatOps)
        val search = new BestFirstSearch(formatOps, range, formatWriter)
        val partial = search.getBestPath
        val formattedString = formatWriter.mkString(partial.splits)
        val correctedFormattedString =
          if ((style.lineEndings == preserve && isWindows) ||
            style.lineEndings == windows) {
            formattedString.replaceAll(UnixLineEnding, WindowsLineEnding)
          } else {
            formattedString
          }
        if (partial.reachedEOF) {
          Formatted.Success(correctedFormattedString)
        } else {
          throw Incomplete(correctedFormattedString)
        }
      }
    } catch {
      // TODO(olafur) add more fine grained errors.
      case NonFatal(e) => Formatted.Failure(e)
    }
  }

  private[this] def containsWindowsLineEndings(code: String): Boolean =
    code.contains(WindowsLineEnding)

  /** Utility method to change dialect on ScalafmtConfig.
    *
    * Binary compatibility is guaranteed between releases, unlike with ScalafmtConfig.copy.
    **/
  def configWithDialect(
      config: ScalafmtConfig,
      dialect: Dialect): ScalafmtConfig =
    config.withDialect(dialect)

  def configForSbt(
      config: ScalafmtConfig
  ): ScalafmtConfig =
    config.forSbt
}
