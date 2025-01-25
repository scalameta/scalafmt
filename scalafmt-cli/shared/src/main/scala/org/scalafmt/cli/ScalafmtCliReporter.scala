package org.scalafmt.cli

import org.scalafmt.Error._
import org.scalafmt.interfaces._

import java.io.OutputStreamWriter
import java.io.PrintWriter
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicReference

import scala.annotation.tailrec
import scala.util.control.NoStackTrace

class ScalafmtCliReporter(options: CliOptions) extends ScalafmtReporter {
  private val exitCode = new AtomicReference(ExitCode.Ok)

  def getExitCode: ExitCode = exitCode.get()

  override def error(file: Path, message: String): Unit =
    if (!options.ignoreWarnings) {
      options.common.err.println(s"$message: $file")
      exitCode.getAndUpdate(ExitCode.merge(ExitCode.UnexpectedError, _))
    }
  @tailrec
  override final def error(file: Path, e: Throwable): Unit = e match {
    case WithCode(e, _) => error(file, e)
    case _: PositionException if !options.ignoreWarnings =>
      options.common.err.println(s"${e.toString}: $file")
      exitCode.getAndUpdate(ExitCode.merge(ExitCode.ParseError, _))
    case MisformattedFile(_, diff) =>
      options.common.err.println(diff)
      exitCode.getAndUpdate(ExitCode.merge(ExitCode.TestError, _))
    case e: ScalafmtException => error(file, e.getCause)
    case _ if e.getClass.getSimpleName.contains("ScalafmtException") =>
      error(file, e.getCause)
    case _ if options.ignoreWarnings =>
    case _ =>
      new FailedToFormat(file.toString, e).printStackTrace(options.common.err)
      exitCode.getAndUpdate(ExitCode.merge(ExitCode.UnexpectedError, _))
  }

  override def excluded(file: Path): Unit = options.common.debug
    .println(s"file excluded: $file")

  override def parsedConfig(config: Path, scalafmtVersion: String): Unit =
    options.common.debug.println(s"parsed config (v$scalafmtVersion): $config")

  override def downloadWriter(): PrintWriter = options.common.info.printWriter

  override def downloadOutputStreamWriter(): OutputStreamWriter =
    new OutputStreamWriter(options.common.info.outputStream)
}

private class FailedToFormat(filename: String, cause: Throwable)
    extends Exception(filename, cause) with NoStackTrace
