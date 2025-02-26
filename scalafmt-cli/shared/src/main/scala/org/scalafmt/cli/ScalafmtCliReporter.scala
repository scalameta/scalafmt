package org.scalafmt.cli

import org.scalafmt.interfaces._

import java.io.OutputStreamWriter
import java.io.PrintWriter
import java.nio.file.Path
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference

import scala.annotation.tailrec
import scala.util.control.NoStackTrace

class ScalafmtCliReporter(options: CliOptions) extends ScalafmtReporter {
  private val exitCode = new AtomicReference(ExitCode.Ok)
  private val exitCodePerFile = new ConcurrentHashMap[String, ExitCode]()

  def getExitCode: ExitCode = exitCode.get()
  def getExitCode(file: Path): ExitCode = exitCodePerFile.get(file.toString)

  private def updateExitCode(code: ExitCode, file: Path): Unit = if (!code.isOk) {
    exitCodePerFile.put(file.toString, code)
    exitCode.getAndUpdate(ExitCode.merge(code, _))
  }

  override def error(file: Path, message: String): Unit =
    if (!options.ignoreWarnings) {
      options.common.err.println(s"$message: $file")
      updateExitCode(ExitCode.UnexpectedError, file)
    }
  override final def error(file: Path, e: Throwable): Unit =
    updateExitCode(fail(e)(file), file)
  private[cli] final def fail(e: Throwable)(file: Path): ExitCode = {
    @tailrec
    def iter(e: Throwable): ExitCode = e match {
      case _: PositionException =>
        options.common.err.println(s"${e.toString}: $file")
        ExitCode.ParseError
      case _ =>
        val cause = e.getCause
        if (cause ne null) iter(cause)
        else {
          new FailedToFormat(file.toString, e)
            .printStackTrace(options.common.err)
          ExitCode.UnexpectedError
        }
    }
    if (options.ignoreWarnings) ExitCode.Ok else iter(e)
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
