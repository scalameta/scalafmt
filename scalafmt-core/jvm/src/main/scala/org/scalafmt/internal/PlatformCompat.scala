package org.scalafmt.internal

import metaconfig._

import java.io.File

object PlatformCompat {
  @inline def metaconfigInputFromFile(input: File) =
    Input.File(input)
}
