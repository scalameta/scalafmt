package org.scalafmt.sysops

import scala.concurrent.ExecutionContext

private[scalafmt] object PlatformCompat {
  def isScalaNative = false
  def prepareCommand(cmd: Seq[String]) = cmd
  def isNativeOnWindows = false

  implicit def executionContext: ExecutionContext = ExecutionContext.global

}
