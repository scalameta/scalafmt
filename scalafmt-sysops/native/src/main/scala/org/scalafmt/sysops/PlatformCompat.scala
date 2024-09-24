package org.scalafmt.sysops

import scala.scalanative.runtime.Platform

private[scalafmt] object PlatformCompat {
  def isScalaNative = true
  def prepareCommand(cmd: Seq[String]) =
    if (Platform.isWindows()) cmd.map(arg => s""""$arg"""") else cmd
  def fixPathOnNativeWindows(path: String) =
    if (Platform.isWindows()) path.replace('/', '\\') else path
  def isNativeOnWindows() = Platform.isWindows()
  def relativize(base: AbsoluteFile, file: AbsoluteFile) =
    if (Platform.isWindows()) base.path.relativize(file.path)
    else base.toUri.relativize(file.toUri)
}
