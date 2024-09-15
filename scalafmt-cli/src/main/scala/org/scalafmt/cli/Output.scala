package org.scalafmt.cli

import java.io._

object Output {

  trait StreamOrWriter {
    def outputStream: OutputStream
    def printStream: PrintStream
    def printWriter: PrintWriter
  }

  object NoopStream extends OutputStream with StreamOrWriter {
    self =>
    override def write(b: Int): Unit = ()

    override def write(b: Array[Byte]): Unit = ()

    override def write(b: Array[Byte], off: Int, len: Int): Unit = ()

    def outputStream: OutputStream = self
    val printStream = new PrintStream(self)
    val printWriter = new PrintWriter(self)
  }

  class FromStream(val obj: PrintStream) extends StreamOrWriter {
    override def outputStream: OutputStream = obj
    override def printStream: PrintStream = obj
    override def printWriter: PrintWriter = new PrintWriter(obj)
  }

}
