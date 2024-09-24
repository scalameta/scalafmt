package org.scalafmt.sysops

private[scalafmt] object PlatformCompat {
  def isScalaNative = false
  def prepareCommand(cmd: Seq[String]) = cmd
  def fixPathOnNativeWindows(path: String) = path
  def isNativeOnWindows() = false
  def relativize(base: AbsoluteFile, path: AbsoluteFile) = base.toUri
    .relativize(path.toUri)
}
