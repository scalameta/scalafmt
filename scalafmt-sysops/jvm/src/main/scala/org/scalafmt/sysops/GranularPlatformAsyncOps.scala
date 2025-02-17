package org.scalafmt.sysops

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.nio.channels.AsynchronousFileChannel
import java.nio.channels.CompletionHandler
import java.nio.file.Path
import java.nio.file.StandardOpenOption
import java.{util => ju}

import scala.concurrent.Future
import scala.concurrent.Promise
import scala.io.Codec
import scala.util.Try

private[sysops] object GranularPlatformAsyncOps {

  def readFileAsync(path: Path)(implicit codec: Codec): Future[String] = {
    val promise = Promise[String]()

    val buf = new Array[Byte](1024)
    val bbuf = ByteBuffer.wrap(buf)
    val os = new ByteArrayOutputStream()

    Try {
      val channel = AsynchronousFileChannel.open(
        path,
        new ju.HashSet(ju.Arrays.asList(StandardOpenOption.READ)),
        PlatformRunOps.inputExecutionContext,
      )
      val handler = new CompletionHandler[Integer, AnyRef] {
        override def completed(result: Integer, unused: AnyRef): Unit = {
          val count = result.intValue()
          if (count < 0) {
            promise.trySuccess(os.toString(codec.charSet.name()))
            channel.close()
          } else {
            if (count > 0) {
              os.write(buf, 0, count)
              bbuf.clear()
            }
            channel.read(bbuf, os.size(), null, this)
          }
        }
        override def failed(exc: Throwable, unused: AnyRef): Unit = {
          promise.tryFailure(exc)
          channel.close()
        }
      }

      channel.read(bbuf, 0, null, handler)
    }.failed.foreach(promise.tryFailure)

    promise.future
  }

  def writeFileAsync(path: Path, content: String)(implicit
      codec: Codec,
  ): Future[Unit] = {
    val promise = Promise[Unit]()
    val buf = ByteBuffer.wrap(content.getBytes(codec.charSet))

    Try {
      val channel = AsynchronousFileChannel.open(
        path,
        new ju.HashSet(ju.Arrays.asList(
          StandardOpenOption.CREATE,
          StandardOpenOption.WRITE,
          StandardOpenOption.TRUNCATE_EXISTING,
        )),
        PlatformRunOps.outputExecutionContext,
      )

      val handler = new CompletionHandler[Integer, AnyRef] {
        override def completed(result: Integer, attachment: AnyRef): Unit =
          if (buf.hasRemaining) channel.write(buf, buf.position(), null, this)
          else {
            promise.trySuccess(())
            channel.close()
          }

        override def failed(exc: Throwable, attachment: AnyRef): Unit = {
          promise.tryFailure(exc)
          channel.close()
        }
      }

      channel.write(buf, 0L, null, handler)
    }.failed.foreach(promise.tryFailure)

    promise.future
  }

}
