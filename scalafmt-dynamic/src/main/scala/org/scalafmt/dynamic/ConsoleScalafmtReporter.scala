package org.scalafmt.dynamic

import java.io.PrintStream
import java.io.PrintWriter
import java.nio.file.Path
import org.scalafmt.interfaces.ScalafmtReporter

object ConsoleScalafmtReporter extends ConsoleScalafmtReporter(System.err)

class ConsoleScalafmtReporter(out: PrintStream) extends ScalafmtReporter {
  override def downloadWriter(): PrintWriter = new PrintWriter(out)
  def trimStacktrace(e: Throwable): Unit = ()
  override def excluded(filename: Path): Unit = {
    out.println(s"file excluded: $filename")
  }
  override def error(file: Path, e: Throwable): Unit = {
    out.print(s"error: $file: ")
    trimStacktrace(e)
    e.printStackTrace(out)
  }

  override def error(path: Path, message: String): Unit = {
    out.println(s"error: $path: $message")
  }

  override def parsedConfig(config: Path, scalafmtVersion: String): Unit = {
    out.println(s"parsed config (v$scalafmtVersion): $config")
  }
}
