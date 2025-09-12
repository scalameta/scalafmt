package org.scalafmt.sysops

import scala.sys.process._

private[scalafmt] object CompatRunOps {

  def runProcessLines(
      process: ProcessBuilder,
      log: ProcessLogger,
  ): Iterable[String] = process.lineStream(log)

}
