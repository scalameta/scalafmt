package org.scalafmt.util

/** Utils related to differences between various operating systems. */
object OsSpecific {
  val isWindows: Boolean =
    System.getProperty("os.name").toLowerCase().startsWith("windows")
  val lineSeparator: String = System.getProperty("line.separator")
}
