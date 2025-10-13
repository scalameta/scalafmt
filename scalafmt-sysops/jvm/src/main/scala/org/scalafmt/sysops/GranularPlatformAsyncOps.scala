package org.scalafmt.sysops

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.nio.channels.{AsynchronousFileChannel, CompletionHandler}
import java.nio.file.{Path, StandardOpenOption}
import java.{util => ju}

import scala.concurrent.{Future, Promise}
import scala.io.Codec

private[sysops] object GranularPlatformAsyncOps {

  def readFileAsync(path: Path)(implicit codec: Codec): Future[String] = {
    val ec = PlatformRunOps.inputExecutionContext
    Future {
      val opts = new ju.HashSet(ju.Arrays.asList(StandardOpenOption.READ))
      AsynchronousFileChannel.open(path, opts, ec)
    }(ec).flatMap { channel =>
      val promise = Promise[String]()
      new ReadContext(promise, channel).read(0)
      promise.future
    }(PlatformRunOps.parasiticExecutionContext)
  }

  def writeFileAsync(path: Path, content: String)(implicit
      codec: Codec,
  ): Future[Unit] = {
    val ec = PlatformRunOps.outputExecutionContext
    Future {
      val opts = new ju.HashSet(ju.Arrays.asList(
        StandardOpenOption.CREATE,
        StandardOpenOption.WRITE,
        StandardOpenOption.TRUNCATE_EXISTING,
      ))
      AsynchronousFileChannel.open(path, opts, ec)
    }(ec).flatMap { channel =>
      val promise = Promise[Unit]()
      val buf = ByteBuffer.wrap(content.getBytes(codec.charSet))
      new WriteContext(promise, channel, buf).write()
      promise.future
    }(PlatformRunOps.parasiticExecutionContext)
  }

  private abstract class Handler[A <: Context[_]]
      extends CompletionHandler[Integer, A] {
    override def failed(exc: Throwable, obj: A): Unit = obj.failed(exc)
  }

  private object ReadHandler extends Handler[ReadContext] {
    override def completed(res: Integer, obj: ReadContext): Unit = obj
      .read(res.intValue())
  }

  private object WriteHandler extends Handler[WriteContext] {
    override def completed(res: Integer, obj: WriteContext): Unit = obj.write()
  }

  private abstract class Context[A] {
    val promise: Promise[A]
    val channel: AsynchronousFileChannel
    def failed(exc: Throwable): Unit = {
      promise.tryFailure(exc)
      channel.close()
    }
  }

  private class ReadContext(
      val promise: Promise[String],
      val channel: AsynchronousFileChannel,
  )(implicit codec: Codec)
      extends Context[String] {
    private val arr = new Array[Byte](1024)
    private val buf = ByteBuffer.wrap(arr)
    private val os = new ByteArrayOutputStream()

    def read(count: Int): Unit =
      if (count < 0) {
        promise.trySuccess(os.toString(codec.charSet.name()))
        channel.close()
      } else {
        if (count > 0) { os.write(arr, 0, count); buf.clear() }
        try channel.read(buf, os.size(), this, ReadHandler)
        catch { case exc: Throwable => failed(exc) }
      }
  }

  private class WriteContext(
      val promise: Promise[Unit],
      val channel: AsynchronousFileChannel,
      buf: ByteBuffer,
  ) extends Context[Unit] {
    def write(): Unit =
      if (buf.hasRemaining)
        try channel.write(buf, buf.position(), this, WriteHandler)
        catch { case exc: Throwable => failed(exc) }
      else {
        promise.trySuccess(())
        channel.close()
      }
  }

}
