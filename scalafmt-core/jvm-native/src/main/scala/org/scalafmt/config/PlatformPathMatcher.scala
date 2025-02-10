package org.scalafmt.config

import java.nio.file

object PlatformPathMatcher {
  val fs: file.FileSystem = file.FileSystems.getDefault

  def apply(pattern: String): PathMatcher = {
    try fs.getPathMatcher(pattern)
    catch {
      case _: IllegalArgumentException => throw new ScalafmtConfigException(
          s"Illegal pattern in configuration: $pattern",
        )
    }
  }.matches
}
