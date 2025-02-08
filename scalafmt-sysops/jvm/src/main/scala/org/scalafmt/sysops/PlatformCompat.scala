package org.scalafmt.sysops

private[scalafmt] object PlatformCompat {
  def isJVM = true
  def isScalaNative = false
  def isNativeOnWindows = false

  def relativize(cwd: AbsoluteFile, file: AbsoluteFile): String = cwd.toUri
    .relativize(file.toUri).toString
}
