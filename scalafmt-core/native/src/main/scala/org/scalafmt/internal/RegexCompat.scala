package org.scalafmt.internal

import java.util.regex.Pattern

import scala.collection.mutable

/* Before text matching (?=re), after text matching (?<=re)
 * and more are incompatible in Scala Native, so custom functions
 * have to be used.
 * Scala Native uses an implementation of:
 * https://github.com/google/re2/wiki/Syntax
 */

object RegexCompat {

  /* Replaces '\\h', which is incompatible in Scala Native.
   * Does not check correctness of the input regex string.
   */
  private def fixHorizontalSpaceInRegex(reg: String) = {

    val replacingInsideClass =
      "\t \u00A0\u1680\u180E\u2000-\u200A\u202F\u205F\u3000"

    val replacingOutsideClass = s"[$replacingInsideClass]"

    val sb = new StringBuilder()
    var isInClass = false
    var isEscaped = false

    for (char <- reg) char match {
      case '\\' if !isEscaped => isEscaped = true
      case 'h' if isEscaped =>
        sb.append(if (isInClass) replacingInsideClass else replacingOutsideClass)
        isEscaped = false
      case '[' if !isEscaped =>
        sb.append('[')
        isInClass = true
      case ']' if !isEscaped =>
        sb.append(']')
        isInClass = false
      case other =>
        if (isEscaped) {
          isEscaped = false
          sb.append('\\')
        }
        sb.append(other)
    }
    sb.toString()
  }

  @inline
  private def pat(str: String, flags: Int = 0): Pattern = Pattern
    .compile(fixHorizontalSpaceInRegex(str), flags)

  // "slc" stands for single-line comment
  val slcLine = pat("^/\\/\\/*\\h*(.*?)\\h*\\r*$")

  val slcDelim = pat("\\h+")

  // "mlc" stands for multi-line comment
  private val mlcLineDelimPat = "\\r*\\n\\h*(?:[*]+\\h*)?"
  val mlcHeader = pat(s"^/\\*\\h*(?:$mlcLineDelimPat)?")
  val mlcLineDelim = pat(s"\\h*$mlcLineDelimPat")

  val mlcParagraphEnd = pat("[.:!?=]\\h*\\r*$")

  val mlcParagraphBeg = pat("^(?:[-*@=]|\\d+[.:])")

  val leadingAsteriskSpace = pat("\\h*\\r*\\n(\\h*)(?:[*][^*])?")

  val docstringLine = pat("^(?:\\h*\\*)?(\\h*)(.*?)\\h*$", Pattern.MULTILINE)

  val emptyLines = fixHorizontalSpaceInRegex("\\h*(\n\\h*\\*?\\h*)*")

  val emptyDocstring = pat(s"^/\\*\\*$emptyLines\\*/$$")

  val onelineDocstring = {
    val oneline = fixHorizontalSpaceInRegex("[^*\n\\h](?:[^\n]*[^\n\\h])?")
    pat(s"^/\\*\\*$emptyLines($oneline)$emptyLines\\*/$$")
  }

  val docstringLeadingSpace = pat("^\\h+")

  @inline
  def compileStripMarginPattern(pipe: Char) = pat(s"\\h*\\r*\\n(\\h*\\$pipe)?")

  @inline
  def compileStripMarginPatternWithLineContent(pipe: Char) =
    pat(s"\\n(\\h*\\$pipe)?([^\\r\\n]*)")

  val stripMarginPatternWithLineContent =
    compileStripMarginPatternWithLineContent('|')

  private val leadingPipeSpace = compileStripMarginPattern('|')

  @inline
  def getStripMarginPattern(pipe: Char) =
    if (pipe == '|') leadingPipeSpace else compileStripMarginPattern(pipe)

  // replaces baseText.split("(?={beforeText})")
  def splitByBeforeTextMatching(
      baseText: String,
      beforeText: String,
  ): Array[String] = {
    val matcher = Pattern.compile(beforeText).matcher(baseText)
    matcher.region(1, baseText.length)

    val res = new mutable.ArrayBuilder.ofRef[String]
    var currPosition = 0
    while (matcher.find()) {
      val start = matcher.start()
      res += baseText.substring(currPosition, start)
      currPosition = start
    }
    res += baseText.substring(currPosition, baseText.length)

    res.result()
  }
}
