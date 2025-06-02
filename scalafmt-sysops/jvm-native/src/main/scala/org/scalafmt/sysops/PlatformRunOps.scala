package org.scalafmt.sysops

import java.nio.file.Path
import java.util.concurrent.Executors
import java.util.concurrent.SynchronousQueue
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.TimeUnit

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutorService
import scala.sys.process.ProcessLogger
import scala.util.Failure
import scala.util.Success
import scala.util.Try

private[scalafmt] object PlatformRunOps {

  implicit def executionContext: ExecutionContext = ExecutionContext.global

  private val ncores = Runtime.getRuntime.availableProcessors()

  // creates non-daemon threads
  val inputExecutionContext: ExecutionContextExecutorService = ExecutionContext
    .fromExecutorService(Executors.newFixedThreadPool(ncores))

  lazy val formatExecutionContext: ExecutionContext = {
    val queue = new SynchronousQueue[Runnable]() {
      override def offer(e: Runnable): Boolean = { put(e); true } // blocks
    }
    ExecutionContext.fromExecutorService(
      new ThreadPoolExecutor(ncores, ncores, 0L, TimeUnit.MILLISECONDS, queue),
    )
  }

  val outputExecutionContext: ExecutionContextExecutorService = ExecutionContext
    .fromExecutorService(Executors.newFixedThreadPool(ncores))

  implicit def parasiticExecutionContext: ExecutionContext =
    GranularDialectAsyncOps.parasiticExecutionContext

  def runArgv(cmd: Seq[String], cwd: Option[Path]): Try[String] = {
    val err = new StringBuilder()
    val logger = ProcessLogger(_ => (), x => err.append("\n> ").append(x))
    val argv =
      if (PlatformCompat.isNativeOnWindows) cmd.map(arg => '"' + arg + '"')
      else cmd
    try {
      val proc = sys.process.Process(argv, cwd.map(_.toFile))
      Success(proc.!!(logger).trim)
    } catch {
      case e: Throwable =>
        val msg =
          s"Failed to run '${cmd.mkString(" ")}'. Error:${err.result()}\n"
        Failure(new IllegalStateException(msg, e))
    }
  }

  def exit(code: Int): Nothing = sys.exit(code)

}
