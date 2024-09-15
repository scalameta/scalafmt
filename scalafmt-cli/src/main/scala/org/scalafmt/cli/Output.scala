package org.scalafmt.cli

import java.io._

object Output {

  object NoopStream extends OutputStream {
    self =>
    override def write(b: Int): Unit = ()

    override def write(b: Array[Byte]): Unit = ()

    override def write(b: Array[Byte], off: Int, len: Int): Unit = ()

    def outputStream: OutputStream = self
    val printStream = new PrintStream(self)
    val printWriter = new PrintWriter(self)
    val streamWriter = new OutputStreamWriter(self)
  }

}
