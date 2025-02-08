package org.scalafmt.sysops

import scala.scalanative.runtime.Platform

private[scalafmt] object PlatformCompat {
  def isJVM = false
  def isScalaNative = true
  def isNativeOnWindows = Platform.isWindows()

  def relativize(cwd: AbsoluteFile, file: AbsoluteFile): String = {
    val usePath = PlatformCompat.isNativeOnWindows
    if (usePath) cwd.path.relativize(file.path).toString
    else cwd.toUri.relativize(file.toUri).toString
  }

}
