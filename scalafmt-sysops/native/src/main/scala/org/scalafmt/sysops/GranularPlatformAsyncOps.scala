package org.scalafmt.sysops

import java.nio.file.Files
import java.nio.file.Path

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.io.Codec

private[sysops] object GranularPlatformAsyncOps {

  import PlatformRunOps.executionContext

  def ioExecutionContext: ExecutionContext = executionContext

  def readFileAsync(path: Path)(implicit codec: Codec): Future[String] =
    Future(PlatformFileOps.readFile(path))

  def writeFileAsync(path: Path, content: String)(implicit
      codec: Codec,
  ): Future[Unit] = Future {
    val bytes = content.getBytes(codec.charSet)
    Files.write(path, bytes)
  }

}
