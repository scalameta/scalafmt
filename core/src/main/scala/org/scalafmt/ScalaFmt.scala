package org.scalafmt

import org.scalafmt.internal.BestFirstSearch
import org.scalafmt.internal.FormatOps
import org.scalafmt.internal.FormatWriter
import scala.util.control.NonFatal
import scala.meta.Input.stringToInput

object Scalafmt {

  /**
    * Format Scala code using scalafmt.
    *
    * @param code Code string to format.
    * @param config Configuration for formatting output.
    * @param runner Configuration for how the formatting should run.
    * @param range EXPERIMENTAL. Format a subset of lines.
    * @return [[FormatResult.Success]] if successful,
    *        [[FormatResult.Failure]] otherwise. If you are OK with throwing
    *        exceptions, use [[FormatResult.Success.get]] to get back a
    *        string.
    */
  def format(code: String,
             config: ScalafmtConfig = ScalafmtConfig.default,
             runner: ScalafmtRunner = ScalafmtRunner.default,
             range: Set[Range] = Set.empty[Range]): FormatResult = {
    try {
      val tree = new scala.meta.XtensionParseInputLike(code)
        .parse(stringToInput, runner.parser)
      val formatOps = new FormatOps(tree, config)
      val formatWriter = new FormatWriter(formatOps)
      val search = new BestFirstSearch(formatOps, range, formatWriter)
      val formattedString = formatWriter.mkString(search.getBestPath)
      FormatResult.Success(formattedString)
    } catch {
      // TODO(olafur) add more fine grained errors.
      case NonFatal(e) => FormatResult.Failure(e)
    }
  }
}
