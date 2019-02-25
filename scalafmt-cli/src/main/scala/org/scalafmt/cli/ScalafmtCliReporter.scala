package org.scalafmt.cli
import java.io.PrintWriter
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicReference
import java.util.function.UnaryOperator

import org.scalafmt.Error.MisformattedFile
import org.scalafmt.interfaces.{PositionException, ScalafmtReporter}
import org.scalafmt.dynamic.ScalafmtException

import scala.util.control.NoStackTrace

class ScalafmtCliReporter(options: CliOptions) extends ScalafmtReporter {
  private val exitCode = new AtomicReference(ExitCode.Ok)

  def getExitCode: ExitCode = exitCode.get()

  override def error(
      file: Path,
      message: String
  ): Unit = {
    if (!options.ignoreWarnings) {
      if (!options.quiet) options.common.err.println(s"$message: $file")
      exitCode.getAndUpdate(new UnaryOperator[ExitCode] {
        override def apply(t: ExitCode): ExitCode =
          ExitCode.merge(ExitCode.UnexpectedError, t)
      })
    }
  }
  override def error(
      file: Path,
      e: Throwable
  ): Unit = {
    e match {
      case _: PositionException if !options.ignoreWarnings =>
        if (!options.quiet) options.common.err.println(s"${e.toString}: $file")
        exitCode.getAndUpdate(new UnaryOperator[ExitCode] {
          override def apply(t: ExitCode): ExitCode =
            ExitCode.merge(ExitCode.ParseError, t)
        })
      case MisformattedFile(_, diff) =>
        options.common.err.println(diff)
        exitCode.getAndUpdate(new UnaryOperator[ExitCode] {
          override def apply(t: ExitCode): ExitCode =
            ExitCode.merge(ExitCode.TestError, t)
        })
      case ScalafmtException(_, cause) => error(file, cause)
      case _ if !options.ignoreWarnings =>
        if (!options.quiet)
          new FailedToFormat(file.toString, e)
            .printStackTrace(options.common.err)
        exitCode.getAndUpdate(new UnaryOperator[ExitCode] {
          override def apply(t: ExitCode): ExitCode =
            ExitCode.merge(ExitCode.UnexpectedError, t)
        })
    }
  }

  override def excluded(file: Path): Unit = {
    if (options.debug) options.common.out.println(s"file excluded: $file")
  }

  override def parsedConfig(
      config: Path,
      scalafmtVersion: String
  ): Unit = {
    if (options.debug)
      options.common.out.println(s"parsed config (v$scalafmtVersion): $config")
  }

  override def downloadWriter(): PrintWriter =
    new PrintWriter(options.common.out)
}

private class FailedToFormat(filename: String, cause: Throwable)
    extends Exception(filename, cause)
    with NoStackTrace
