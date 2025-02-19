package org.scalafmt.sysops

import java.nio.file.Path

import scala.concurrent._
import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation._
import scala.util.Failure
import scala.util.Success
import scala.util.Try

private[scalafmt] object PlatformRunOps {

  implicit def executionContext: ExecutionContext =
    scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  val inputExecutionContext: ExecutionContext = executionContext

  implicit def parasiticExecutionContext: ExecutionContext =
    GranularDialectAsyncOps.parasiticExecutionContext

  def runArgv(cmd: Seq[String], cwd: Option[Path]): Try[String] = {
    val options = cwd.fold(js.Dictionary[js.Any]())(cwd =>
      js.Dictionary[js.Any]("cwd" -> cwd.toString),
    )
    val result = SpawnSync(cmd.head, cmd.tail.toJSArray, options)
    if (result.status.asInstanceOf[Int] == 0)
      Success(result.stdout.toString.trim)
    else {
      val msg =
        s"Failed to run '${cmd.mkString(" ")}'. Error:\n${result.stderr}\n"
      Failure(scala.scalajs.js.JavaScriptException(msg))
    }
  }

  def exit(code: Int): Nothing = {
    js.Dynamic.global.process.exit(code)
    throw new Throwable() // just like JVM's sys.exit
  }
}

@js.native
@JSImport("child_process", "spawnSync")
object SpawnSync extends js.Object {
  def apply(
      command: String,
      args: js.Array[String],
      options: js.Dictionary[js.Any],
  ): js.Dynamic = js.native
}
