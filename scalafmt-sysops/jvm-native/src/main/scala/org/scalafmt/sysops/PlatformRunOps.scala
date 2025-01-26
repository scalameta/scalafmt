package org.scalafmt.sysops

import java.nio.file.Path
import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext
import scala.sys.process.ProcessLogger
import scala.util.Failure
import scala.util.Success
import scala.util.Try

private[scalafmt] object PlatformRunOps {

  implicit def executionContext: ExecutionContext = ExecutionContext.global

  def ioExecutionContext: ExecutionContext =
    GranularPlatformAsyncOps.ioExecutionContext

  def getSingleThreadExecutionContext: ExecutionContext = ExecutionContext
    .fromExecutor(Executors.newSingleThreadExecutor())

  def runArgv(cmd: Seq[String], cwd: Option[Path]): Try[String] = {
    val err = new StringBuilder()
    val logger = ProcessLogger(_ => (), x => err.append("\n> ").append(x))
    val argv =
      if (PlatformCompat.isNativeOnWindows) cmd.map(arg => '"' + arg + '"')
      else cmd
    Try(sys.process.Process(argv, cwd.map(_.toFile)).!!(logger)) match {
      case Failure(e) =>
        val msg =
          s"Failed to run '${cmd.mkString(" ")}'. Error:${err.result()}\n"
        Failure(new IllegalStateException(msg, e))
      case Success(x) => Success(x.trim)
    }
  }

  def exit(code: Int): Nothing = sys.exit(code)

}
