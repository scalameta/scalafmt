package org.scalafmt.sysops

import java.nio.file
import java.nio.file.Path

object PlatformPathMatcher {
  val fs: file.FileSystem = file.FileSystems.getDefault

  private def getNioPathMatcher(pattern: String): file.PathMatcher =
    try fs.getPathMatcher(pattern)
    catch {
      case e: IllegalArgumentException =>
        val sb = new StringBuilder()
        sb.append("Invalid path matcher pattern: ").append(pattern)
        val err = e.getMessage
        if (null != err && err.nonEmpty) sb.append("; ").append(err)
        throw new ScalafmtSysException(sb.toString())
    }

  def apply(pattern: String): PathMatcher = new PathMatcher {
    private val pathMatcher = getNioPathMatcher(pattern)
    override def matches(path: Path): Boolean = pathMatcher.matches(path)
  }
}
