package org.scalafmt.cli

import java.io.PrintStream
import java.nio.file.Path
import org.scalafmt.interfaces

object ScalafmtReporterImpl extends ScalafmtReporterImpl(System.out)
class ScalafmtReporterImpl(out: PrintStream)
    extends interfaces.ScalafmtReporter {
  override def excluded(filename: String): Unit = {
    out.println(s"file excluded: $filename")
  }
  override def error(e: Throwable): Unit = {
    e.printStackTrace(out)
  }
  override def error(message: String): Unit = {
    out.println(s"error: $message")
  }
  override def error(path: Path, message: String): Unit = {
    out.println(s"error: $path: $message")
  }

  override def parsedConfig(config: Path): Unit = {
    out.println(s"parsed config: $config")
  }
}
