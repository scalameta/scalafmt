package org.scalafmt.sysops

import java.nio.file.Path

import scala.concurrent.Future
import scala.io.Codec

private[sysops] object GranularPlatformAsyncOps {

  def readFileAsync(path: Path)(implicit codec: Codec): Future[String] =
    Future(PlatformFileOps.readFile(path))(PlatformRunOps.inputExecutionContext)

  def writeFileAsync(path: Path, data: String)(implicit
      codec: Codec,
  ): Future[Unit] = Future(
    PlatformFileOps.writeFile(path, data),
  )(PlatformRunOps.outputExecutionContext)

}
