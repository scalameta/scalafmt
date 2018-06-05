package org.scalafmt.util

/** Utils related to differences between various operating systems. */
object OsSpecific {
  def isWindows: Boolean =
    // NOTE: org.scalameta:io implements java.io.File for Node.js so this will work correctly
    // on Node.js + Window and also in the browser.
    java.io.File.separatorChar == '\\'

  def fixSeparatorsInPathPattern(unixSpecificPattern: String): String =
    unixSpecificPattern.replace('/', java.io.File.separatorChar)

  implicit class XtensionStringAsFilename(string: String) {
    def asFilename: String = fixSeparatorsInPathPattern(string)
  }
}
