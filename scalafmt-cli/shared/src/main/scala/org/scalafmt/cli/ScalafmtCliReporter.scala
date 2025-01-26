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
  private def updateExitCode(code: ExitCode): Unit =
    if (!code.isOk) exitCode.getAndUpdate(ExitCode.merge(code, _))

  override def error(file: Path, message: String): Unit =
    if (!options.ignoreWarnings) {
      options.common.err.println(s"$message: $file")
      updateExitCode(ExitCode.UnexpectedError)
    }
  override final def error(file: Path, e: Throwable): Unit =
    updateExitCode(fail(e)(file))
  @tailrec
  private[cli] final def fail(e: Throwable)(file: Path): ExitCode = e match {
    case e: MisformattedFile =>
      options.common.err.println(e.customMessage)
      ExitCode.TestError
    case _: PositionException =>
      if (options.ignoreWarnings) ExitCode.Ok
      else {
        options.common.err.println(s"${e.toString}: $file")
        ExitCode.ParseError
      }
    case _ =>
      val cause = e.getCause
      if (cause ne null) fail(cause)(file)
      else if (options.ignoreWarnings) ExitCode.Ok
      else {
        new FailedToFormat(file.toString, e).printStackTrace(options.common.err)
        ExitCode.UnexpectedError
      }
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
