package org.scalafmt.sysops

import scala.concurrent.ExecutionContext

private[scalafmt] object PlatformCompat {
  def isScalaNative = false
  def prepareCommand(cmd: Seq[String]) = cmd
  def fixPathOnNativeWindows(path: String) = path
  def isNativeOnWindows = false

  implicit def executionContext: ExecutionContext = ExecutionContext.global

}
