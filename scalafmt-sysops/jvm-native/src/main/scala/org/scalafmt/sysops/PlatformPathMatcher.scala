package org.scalafmt.sysops

import java.nio.file

object PlatformPathMatcher {
  val fs: file.FileSystem = file.FileSystems.getDefault

  def apply(pattern: String): PathMatcher = {
    try fs.getPathMatcher(pattern)
    catch {
      case e: IllegalArgumentException =>
        val sb = new StringBuilder()
        sb.append("Invalid path matcher pattern: ").append(pattern)
        val err = e.getMessage
        if (null != err && err.nonEmpty) sb.append("; ").append(err)
        throw new ScalafmtSysException(sb.toString())
    }
  }.matches
}
