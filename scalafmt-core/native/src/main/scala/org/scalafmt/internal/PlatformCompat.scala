package org.scalafmt.internal

import scala.meta.internal.io.FileIO
import scala.meta.io.AbsolutePath

import java.io.File
import java.nio.charset.StandardCharsets

import metaconfig._

object PlatformCompat {
  @inline
  def metaconfigInputFromFile(input: File) = Input
    .String(FileIO.slurp(AbsolutePath(input.toPath()), StandardCharsets.UTF_8))
}
