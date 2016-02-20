package org.scalafmt

import java.util.concurrent.TimeoutException

import org.scalafmt.internal.BestFirstSearch
import org.scalafmt.internal.ScalaFmtLogger

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.meta.Source
import scala.meta.Tree
import scala.meta.parseSource
import scala.meta.parsers.common.Parse
import scala.meta.parsers.common.ParseException
import scala.util.control.NonFatal

object ScalaFmt extends ScalaFmtLogger {
  /**
    * Safely format contents of a Scala compilation unit.
    *
    * Is guaranteed to return within style.maxDuration. The original code is
    * returned in case of any failure or slowness in the formatter.
    *
    * For a more flexible method, see [[format_!()]].
    *
    * Note. Formatting sbt files is not yet supported.
    *
    * @param code The code to format.
    * @param style The coding style to use for formatting.
    * @param range EXPERIMENTAL. Format only certain ranges of the file.
    * @param maxDuration Return code if maxDuration has passed. The maximum
    *                    duration the formatter is allowed to run before
    *                    giving up. If the formatter times out, the original
    *                    source code is returned.
    * @return The code formatted if successful, the original code otherwise.
    */
  def format(code: String,
             style: ScalaStyle = ScalaStyle.Standard,
             range: Set[Range] = Set.empty[Range],
             maxDuration: Duration = Duration(400, "ms")): String = {
    try {
      val formatted = Future(format_![Source](code, style, range)(parseSource))
      Await.result(formatted, maxDuration)
    }
    catch {
      // Skip invalid code.
      case e: ParseException =>
        // Parse exception messages are huge, suppress stacktrace.
        logger.warn(s"Unable to parse code: ${e.getMessage}")
        code
      case e: TimeoutException =>
        logger.warn(s"Too slow formatting to parse code:", e)
        code
      case NonFatal(e) =>
        logger.warn(s"Unexpected error", e)
        code
    }
  }

  /**
    * Formats any kind of Scala tree structure.
    *
    * WARNING. Could run for a very long time on large input.
    *
    * Safe alternatives: [[format]].
    *
    * @param code The source code to format.
    * @param ev See [[scala.meta.parsers]] for available parsers.
    * @tparam T The type of the source code, refer to [[scala.meta.parsers.Api]]
    *           for available types.
    * @return The source code formatted.
    */
  def format_![T <: Tree](code: String,
                          style: ScalaStyle,
                          range: Set[Range] = Set.empty[Range])
                         (implicit ev: Parse[T]): String = {
    import scala.meta._
    val source = code.parse[T]
    val graphSearch = new BestFirstSearch(style, source, range)
    graphSearch.formatTree()
  }

}
