package org.scalafmt.sysops

private[scalafmt] object PlatformCompat {
  def isJS = true
  def isJVM = false
  def isScalaNative = false
  def isNativeOnWindows = false

  def relativize(cwd: AbsoluteFile, file: AbsoluteFile): String = {
    val path = file.toString
    val cwdStr = cwd.toString
    if (path.startsWith(cwdStr)) path.substring(cwdStr.length + 1) else path
  }
}
