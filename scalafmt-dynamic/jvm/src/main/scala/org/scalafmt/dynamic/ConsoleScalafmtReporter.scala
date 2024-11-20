package org.scalafmt.dynamic

import org.scalafmt.interfaces.ScalafmtReporter

import java.io.OutputStreamWriter
import java.io.PrintStream
import java.io.PrintWriter
import java.nio.file.Path

object ConsoleScalafmtReporter extends ConsoleScalafmtReporter(System.err)

class ConsoleScalafmtReporter(out: PrintStream) extends ScalafmtReporter {
  override def error(file: Path, e: Throwable): Unit = {
    out.print(s"error: $file: ")
    trimStacktrace(e)
    e.printStackTrace(out)
  }

  override def error(path: Path, message: String): Unit = out
    .println(s"error: $path: $message")

  override def excluded(filename: Path): Unit = out
    .println(s"file excluded: $filename")

  override def parsedConfig(config: Path, scalafmtVersion: String): Unit = out
    .println(s"parsed config (v$scalafmtVersion): $config")

  override def downloadWriter(): PrintWriter = new PrintWriter(out)

  override def downloadOutputStreamWriter(): OutputStreamWriter =
    new OutputStreamWriter(out)

  protected def trimStacktrace(e: Throwable): Unit = ()
}
