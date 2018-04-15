package org.scalafmt.util

import java.io.File

import scala.collection.mutable

object FormattingCache {
  private val lastFormatting = mutable.Map.empty[File, Long]

  def outdatedFormatting(file: File): Boolean =
    lastFormatting.get(file).forall(_ < file.lastModified)

  def updateFormatting(file: File, timestamp: Long): Unit =
    lastFormatting.update(file, timestamp)
}
