package org.scalafmt.util

import java.io.File

/**
  * Yet Another Logger.
  *
  * I rely heavily on logging for debugging and the standard Java logging
  * doesn't work on Scala.js, which I hope to support one day.
  */
object PrintlnLogger {

  private def log[T](t: sourcecode.Text[T],
                     logLevel: LogLevel,
                     line: sourcecode.Line,
                     file: sourcecode.File,
                     enclosing: sourcecode.Enclosing,
                     showSource: Boolean): Unit = {
    val position = f"${new File(file.value).getName}:${line.value}"
    val key =
      if (showSource) s"[${t.source}]: ${t.value}"
      else t.value
    println(f"$logLevel%-7s $position%-25s $key")
  }

  def elem(
      ts: sourcecode.Text[Any]*)(implicit line: sourcecode.Line,
                                 file: sourcecode.File,
                                 enclosing: sourcecode.Enclosing): Unit = {
    ts.foreach { t =>
      log(t, LogLevel.debug, line, file, enclosing, showSource = true)
    }
  }

  def trace[T](t: sourcecode.Text[T])(implicit line: sourcecode.Line,
                                      file: sourcecode.File,
                                      enclosing: sourcecode.Enclosing): Unit =
    Unit

  def debug[T](t: sourcecode.Text[T])(implicit line: sourcecode.Line,
                                      file: sourcecode.File,
                                      enclosing: sourcecode.Enclosing): Unit =
    log(t, LogLevel.debug, line, file, enclosing, showSource = false)

  def info[T](t: sourcecode.Text[T])(implicit line: sourcecode.Line,
                                     file: sourcecode.File,
                                     enclosing: sourcecode.Enclosing): Unit =
    log(t, LogLevel.info, line, file, enclosing, showSource = false)

  def warn[T](t: sourcecode.Text[T])(implicit line: sourcecode.Line,
                                     file: sourcecode.File,
                                     enclosing: sourcecode.Enclosing): Unit =
    log(t, LogLevel.warn, line, file, enclosing, showSource = false)

  def error[T](t: sourcecode.Text[T])(implicit line: sourcecode.Line,
                                      file: sourcecode.File,
                                      enclosing: sourcecode.Enclosing): Unit =
    log(t, LogLevel.error, line, file, enclosing, showSource = false)
}
