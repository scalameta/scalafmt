package org.scalafmt.util

import java.nio.file.Paths

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
    val position = f"${Paths.get(file.value).getFileName}:${line.value}"
    val key =
      if (showSource) s"(${t.source})= ${t.value}"
      else t.value
    println(f"$logLevel%-7s $position%-25s $key")
  }

  def trace[T](t: sourcecode.Text[T])(implicit line: sourcecode.Line,
                                      file: sourcecode.File,
                                      enclosing: sourcecode.Enclosing): Unit =
    Unit

  def debug[T](t: sourcecode.Text[T])(implicit line: sourcecode.Line,
                                      file: sourcecode.File,
                                      enclosing: sourcecode.Enclosing): Unit =
    log(t, LogLevel.Debug, line, file, enclosing, showSource = false)

  def info[T](t: sourcecode.Text[T])(implicit line: sourcecode.Line,
                                     file: sourcecode.File,
                                     enclosing: sourcecode.Enclosing): Unit =
    log(t, LogLevel.Info, line, file, enclosing, showSource = false)

  def warn[T](t: sourcecode.Text[T])(implicit line: sourcecode.Line,
                                     file: sourcecode.File,
                                     enclosing: sourcecode.Enclosing): Unit =
    log(t, LogLevel.Warn, line, file, enclosing, showSource = false)

  def error[T](t: sourcecode.Text[T])(implicit line: sourcecode.Line,
                                      file: sourcecode.File,
                                      enclosing: sourcecode.Enclosing): Unit =
    log(t, LogLevel.Error, line, file, enclosing, showSource = false)
}
