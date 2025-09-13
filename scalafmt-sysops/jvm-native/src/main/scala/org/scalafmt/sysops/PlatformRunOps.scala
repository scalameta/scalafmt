package org.scalafmt.sysops

import java.nio.file.Path
import java.util.concurrent.Executors
import java.util.concurrent.SynchronousQueue
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.TimeUnit

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutorService
import scala.sys.process.Process
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

  def runArgv(cmd: Seq[String], cwd: Option[Path]): Try[Seq[String]] = {
    val out = Seq.newBuilder[String]
    val err = new StringBuilder()
    val logger = ProcessLogger(out += _, err.append("\n> ").append(_))
    val argv =
      if (PlatformCompat.isNativeOnWindows) cmd.map(arg => '"' + arg + '"')
      else cmd
    def failed(e: Throwable) = {
      val msg = cmd
        .addString(new StringBuilder(), "Failed to run '", " ", "'. Error: ")
        .append(err).append('\n')
      Failure(new IllegalStateException(msg.toString(), e))
    }
    try {
      val exit = Process(argv, cwd.map(_.toFile)).!(logger)
      if (exit != 0) failed(new RuntimeException("exit code " + exit))
      else Success(out.result())
    } catch { case e: Throwable => failed(e) }
  }

  def exit(code: Int): Nothing = sys.exit(code)

}
