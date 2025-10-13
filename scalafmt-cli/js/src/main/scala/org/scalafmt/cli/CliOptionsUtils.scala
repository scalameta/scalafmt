package org.scalafmt.cli

import java.io.{PrintWriter, Writer}

private[scalafmt] trait CliOptionsUtils {
  def getConsoleWriter(): Option[PrintWriter] = TermUtils.getJSConsole
    .map(console =>
      new PrintWriter(new Writer {
        override def write(cbuf: Array[Char], off: Int, len: Int): Unit =
          console.log(new String(cbuf, off, len))
        override def flush(): Unit = {}
        override def close(): Unit = {}
      }),
    )
}
