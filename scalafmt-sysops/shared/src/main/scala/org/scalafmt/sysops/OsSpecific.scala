package org.scalafmt.sysops

/** Utils related to differences between various operating systems. */
object OsSpecific {
  def isWindows: Boolean =
    // NOTE: org.scalameta:io implements java.io.File for Node.js so this will work correctly
    // on Node.js + Window and also in the browser.
    java.io.File.separatorChar == '\\'

  // We need double backslashes here because Regex needs to be escaped.
  def fixSeparatorsInPathPattern(unixSpecificPattern: String): String =
    if (isWindows) unixSpecificPattern.replace("/", "\\\\")
    else unixSpecificPattern

  implicit class XtensionStringAsFilename(private val string: String)
      extends AnyVal {
    def asFilename: String = fixSeparatorsInPathPattern(string)
    def inPathMatcherForm: String =
      if (PlatformCompat.isNativeOnWindows()) string.replace("\\\\", "/")
      else fixSeparatorsInPathPattern(string)

  }
}
