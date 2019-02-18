package org.scalafmt.dynamic.utils

import java.io.{PrintWriter, Writer}
import java.nio.file.Path

import org.scalafmt.dynamic.utils.NoopScalafmtReporter._
import org.scalafmt.interfaces.ScalafmtReporter

class NoopScalafmtReporter extends ScalafmtReporter {
  override def error(file: Path, message: String): Unit = {}
  override def error(file: Path, e: Throwable): Unit = {}
  override def excluded(file: Path): Unit = {}
  override def parsedConfig(config: Path, scalafmtVersion: String): Unit = {}
  override def downloadWriter(): PrintWriter = NoopPrintWriter
}

private object NoopScalafmtReporter {
  val NoopWriter: Writer = new Writer {
    override def write(cbuf: Array[Char], off: Int, len: Int): Unit = {}
    override def flush(): Unit = {}
    override def close(): Unit = {}
  }
  val NoopPrintWriter = new PrintWriter(NoopWriter)
}