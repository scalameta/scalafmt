package org.scalafmt

import scala.meta.Input.stringToInput
import scala.meta.Input.stringToInput
import scala.meta.inputs.Input
import scala.util.control.NonFatal

import org.scalafmt.Error.Incomplete
import org.scalafmt.FormatEvent.CreateFormatOps
import org.scalafmt.LineEndings.preserve
import org.scalafmt.LineEndings.windows
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
    * @param runner Configuration for how the formatting should run.
    * @param range EXPERIMENTAL. Format a subset of lines.
    * @return [[FormatResult.Success]] if successful,
    *        [[FormatResult.Failure]] otherwise. If you are OK with throwing
    *        exceptions, use [[FormatResult.Success.get]] to get back a
    *        string.
    */
  def format(code: String,
             style: ScalafmtStyle = ScalafmtStyle.default,
             runner: ScalafmtRunner = ScalafmtRunner.default,
             range: Set[Range] = Set.empty[Range]): FormatResult = {
    try {
      if (code.matches("\\s*")) FormatResult.Success(System.lineSeparator())
      else {
        val isWindows = containsWindowsLineEndings(code)
        val unixCode = if (isWindows) {
          code.replaceAll(WindowsLineEnding, UnixLineEnding)
        } else {
          code
        }
        val toParse = Rewrite(Input.String(unixCode), style)
        val tree = new scala.meta.XtensionParseInputLike(toParse)
          .parse(stringToInput, runner.parser, runner.dialect)
          .get
        val formatOps = new FormatOps(tree, style, runner)
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
          FormatResult.Success(correctedFormattedString)
        } else {
          throw Incomplete(correctedFormattedString)
        }
      }
    } catch {
      // TODO(olafur) add more fine grained errors.
      case NonFatal(e) => FormatResult.Failure(e)
    }
  }

  private[this] def containsWindowsLineEndings(code: String): Boolean =
    code.contains(WindowsLineEnding)
}
