package org.scalafmt.sysops

import scala.concurrent.ExecutionContext
import scala.scalanative.runtime.Platform

private[scalafmt] object PlatformCompat {
  def isScalaNative = true
  def isNativeOnWindows = Platform.isWindows()

  implicit def executionContext: ExecutionContext = ExecutionContext.global

}
