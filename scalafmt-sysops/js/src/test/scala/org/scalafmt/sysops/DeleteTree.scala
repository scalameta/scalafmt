package org.scalafmt.sysops

import java.nio.file.Path

object DeleteTree {
  def apply(path: Path): Unit = PlatformFileOps
    .rmdir(path = path.toString, recursive = true, force = true)
}
