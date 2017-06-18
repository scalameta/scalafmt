package org.scalafmt.util

/** Utils related to differences between various operating systems. */
object OsSpecific {
  val isWindows: Boolean =
    System.getProperty("os.name").toLowerCase().startsWith("windows")
  val lineSeparator: String = System.getProperty("line.separator")

  def fixSeparatorsInPathPattern(unixSpecificPattern: String): String =
    if (isWindows) unixSpecificPattern.replace("/", """\\""")
    else unixSpecificPattern

  implicit class XtensionStringAsFilename(string: String) {
    def asFilename: String = fixSeparatorsInPathPattern(string)
  }
}
