package org.scalafmt.sysops

import scala.sys.process._

private[scalafmt] object CompatRunOps {

  def runProcess(process: ProcessBuilder, log: ProcessLogger): String = {
    val out = new StringBuilder()
    process.lazyLines(log).foreach(line => out.append(line).append("\n"))
    out.toString()
  }

  def runProcessLines(
      process: ProcessBuilder,
      log: ProcessLogger,
  ): Iterable[String] = process.lazyLines(log)

}
