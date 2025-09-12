package org.scalafmt.sysops

import org.scalafmt.CompatCollections.JavaConverters._

import java.lang.{StringBuilder => JStringBuilder}
import java.nio.file.Path
import java.util.concurrent.Executors
import java.util.concurrent.SynchronousQueue
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.TimeUnit

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutorService
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

  private def readerThread(
      in: java.io.InputStream,
      out: JStringBuilder,
  ): Thread = {
    val t = new Thread(() => {
      val br = new java.io.BufferedReader(new java.io.InputStreamReader(in))
      while ({
        val line = br.readLine()
        val ok = line != null
        if (ok) out.append(line).append('\n')
        ok
      }) {}
    })
    t.start()
    t
  }

  def runArgv(cmd: Seq[String], cwd: Option[Path]): Try[String] = {
    val out = new JStringBuilder()
    val err = new JStringBuilder()
    val argv =
      if (PlatformCompat.isNativeOnWindows) cmd.map(arg => '"' + arg + '"')
      else cmd
    Console.err.println(argv.mkString("run argv [", ", ", "]"))

    val pb = new ProcessBuilder(argv.asJava)
    cwd.foreach(cwd => pb.directory(cwd.toFile))
    pb.redirectInput(ProcessBuilder.Redirect.PIPE) // no stdin

    def failed(e: Throwable) = {
      val msg = cmd
        .addString(new StringBuilder(), "Failed to run '", " ", "'. Error: ")
        .append(err).append('\n')
      Failure(new IllegalStateException(msg.toString(), e))
    }
    try {
      val process = pb.start()
      val outT = readerThread(process.getInputStream, out)
      val errT = readerThread(process.getErrorStream, err)
      val exit = process.waitFor()
      outT.join()
      errT.join()
      if (exit != 0) failed(new RuntimeException("exit code " + exit))
      else Success(out.toString.trim)
    } catch {
      case e: Throwable =>
        Console.err.println(s"Failed: $e")
        failed(e)
    }
  }

  def exit(code: Int): Nothing = sys.exit(code)

}
