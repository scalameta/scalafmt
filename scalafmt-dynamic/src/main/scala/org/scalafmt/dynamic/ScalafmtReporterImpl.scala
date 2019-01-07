package org.scalafmt.dynamic

import java.io.PrintStream
import java.nio.file.Path
import org.scalafmt.interfaces.ScalafmtReporter

object ScalafmtReporterImpl extends ScalafmtReporterImpl(System.out)

class ScalafmtReporterImpl(out: PrintStream) extends ScalafmtReporter {
  def trimStacktrace(e: Throwable): Unit = ()
  override def excluded(filename: Path): Unit = {
    out.println(s"file excluded: $filename")
  }
  override def error(file: Path, e: Throwable): Unit = {
    out.print(s"error: $file: ")
    trimStacktrace(e)
    e.printStackTrace(out)
  }
  override def error(message: String): Unit = {
    out.println(s"error: $message")
  }
  override def error(path: Path, message: String): Unit = {
    out.println(s"error: $path: $message")
  }

  override def parsedConfig(config: Path, scalafmtVersion: String): Unit = {
    out.println(s"parsed config (v$scalafmtVersion): $config")
  }
}
