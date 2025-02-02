package org.scalafmt.sysops

import scala.concurrent.ExecutionContext
import scala.scalanative.runtime.Platform

private[scalafmt] object PlatformCompat {
  def isScalaNative = true
  def prepareCommand(cmd: Seq[String]) =
    if (Platform.isWindows()) cmd.map(arg => s""""$arg"""") else cmd
  def isNativeOnWindows = Platform.isWindows()
  def relativize(base: AbsoluteFile, file: AbsoluteFile) =
    if (Platform.isWindows()) base.path.relativize(file.path)
    else base.toUri.relativize(file.toUri)

  implicit def executionContext: ExecutionContext = ExecutionContext.global

}
