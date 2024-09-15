package org.scalafmt.cli

import java.io._
import java.nio.charset._

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

  class FromWriter(val obj: Writer, charset: Charset = StandardCharsets.UTF_8)
      extends OutputStream with StreamOrWriter {

    override def write(b: Int): Unit = obj.write(b & 0xf)
    override def write(b: Array[Byte]): Unit = obj.write(new String(b, charset))
    override def write(b: Array[Byte], off: Int, len: Int): Unit = obj
      .write(new String(b, off, len, charset))

    override def flush(): Unit = obj.flush()
    override def close(): Unit = obj.close()

    def outputStream: OutputStream = this
    override def printStream: PrintStream = new PrintStream(this)
    override def printWriter: PrintWriter = obj match {
      case x: PrintWriter => x
      case _ => new PrintWriter(obj)
    }
  }

}
