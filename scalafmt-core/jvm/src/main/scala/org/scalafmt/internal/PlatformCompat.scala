package org.scalafmt.internal

import java.io.File

import metaconfig._

object PlatformCompat {
  @inline
  def metaconfigInputFromFile(input: File) = Input.File(input)
}
