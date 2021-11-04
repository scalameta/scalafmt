package org.scalafmt.internal

import metaconfig._
import scala.meta.internal.io.FileIO
import scala.meta.io.AbsolutePath

import java.nio.charset.StandardCharsets
import java.io.File

object PlatformCompat {
  @inline def metaconfigInputFromFile(input: File) =
    Input.String(
      FileIO.slurp(AbsolutePath(input.toPath()), StandardCharsets.UTF_8)
    )
}
