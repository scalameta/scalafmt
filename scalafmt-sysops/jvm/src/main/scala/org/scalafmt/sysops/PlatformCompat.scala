package org.scalafmt.sysops

import scala.concurrent.ExecutionContext

private[scalafmt] object PlatformCompat {
  def isScalaNative = false
  def prepareCommand(cmd: Seq[String]) = cmd
  def isNativeOnWindows = false
  def relativize(base: AbsoluteFile, path: AbsoluteFile) = base.toUri
    .relativize(path.toUri)

  implicit def executionContext: ExecutionContext = ExecutionContext.global

}
