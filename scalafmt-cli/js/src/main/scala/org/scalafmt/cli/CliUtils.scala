package org.scalafmt.cli

import scala.scalajs.js
import scala.scalajs.js.annotation._

private[scalafmt] trait CliUtils {
  protected def getDynamicRunner: Option[ScalafmtRunner] = None

  def readInputLines: Iterator[String] = throw new RuntimeException(
    "reading input is not supported; read from file instead",
  )
}

object CliUtils {

  @JSExportTopLevel("main")
  def jsMain(): Unit = {
    val args = js.Dynamic.global.process.argv.asInstanceOf[js.Array[String]]
    // Drop "node" and script path
    Cli.main(args.drop(2).toArray)
  }

}
