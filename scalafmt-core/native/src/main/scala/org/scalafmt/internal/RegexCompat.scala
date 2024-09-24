package org.scalafmt.internal

import java.util.regex.Pattern

import scala.util.matching.Regex

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

    @inline
    val replacingInsideClass =
      "\t \u00A0\u1680\u180E\u2000-\u200A\u202F\u205F\u3000"

    @inline
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
  val trailingSpace = Pattern
    .compile(fixHorizontalSpaceInRegex("\\h+$"), Pattern.MULTILINE)

  // "slc" stands for single-line comment
  @inline
  val slcLine = Pattern
    .compile(fixHorizontalSpaceInRegex("^/\\/\\/*\\h*(.*?)\\h*$"))

  @inline
  val slcDelim = Pattern.compile(fixHorizontalSpaceInRegex("\\h+"))

  // "mlc" stands for multi-line comment
  @inline
  val mlcHeader = Pattern
    .compile(fixHorizontalSpaceInRegex("^/\\*\\h*(?:\n\\h*[*]*\\h*)?"))

  @inline
  val mlcLineDelim = Pattern
    .compile(fixHorizontalSpaceInRegex("\\h*\n\\h*[*]*\\h*"))

  @inline
  val mlcParagraphEnd = Pattern.compile("[.:!?=]$")

  @inline
  val mlcParagraphBeg = Pattern.compile("^(?:[-*@=]|\\d+[.:])")

  @inline
  val leadingAsteriskSpace = Pattern
    .compile(fixHorizontalSpaceInRegex("\n\\h*[*][^*]"), Pattern.MULTILINE)

  @inline
  val docstringLine = Pattern.compile(
    fixHorizontalSpaceInRegex("^(?:\\h*\\*)?(\\h*)(.*?)\\h*$"),
    Pattern.MULTILINE,
  )

  @inline
  val emptyLines = fixHorizontalSpaceInRegex("\\h*(\n\\h*\\*?\\h*)*")

  @inline
  val emptyDocstring = Pattern.compile(s"^/\\*\\*$emptyLines\\*/$$")

  @inline
  val onelineDocstring = {
    val oneline = fixHorizontalSpaceInRegex("[^*\n\\h](?:[^\n]*[^\n\\h])?")
    Pattern.compile(s"^/\\*\\*$emptyLines($oneline)$emptyLines\\*/$$")
  }

  @inline
  val docstringLeadingSpace = Pattern.compile(fixHorizontalSpaceInRegex("^\\h+"))

  @inline
  def compileStripMarginPattern(pipe: Char) = Pattern
    .compile(fixHorizontalSpaceInRegex(s"\n+\\h*?\\$pipe"), Pattern.MULTILINE)

  @inline
  def compileStripMarginPatternWithLineContent(pipe: Char) = Pattern.compile(
    fixHorizontalSpaceInRegex(s"\n(\\h*\\$pipe)?([^\n]*)"),
    Pattern.MULTILINE,
  )

  val stripMarginPatternWithLineContent =
    compileStripMarginPatternWithLineContent('|')

  // see: https://ammonite.io/#Save/LoadSession
  @inline
  val ammonitePattern: Regex = "(?:\\s*\\n@)".r

  @inline
  val stripMarginPattern = Pattern.compile(
    fixHorizontalSpaceInRegex("\n(\\h*\\|)?([^\n]*)"),
    Pattern.MULTILINE,
  )

  // startAfterPattern and endBeforePattern should be unique in basePattern
  // basePattern = startAfterPattern + matched pattern + endBeforePattern
  private def replaceAll(
      basePattern: Pattern,
      startAfterPattern: Pattern,
      endBeforePattern: Pattern,
      baseText: String,
      replacingText: String,
  ): String = {
    val sb = new java.lang.StringBuilder()
    val matcher = basePattern.matcher(baseText)
    var currPosition = 0
    while (matcher.find()) {
      val start = matcher.start()
      val end = matcher.end()

      sb.append(baseText, currPosition, start)

      val subtext = baseText.substring(start, end)
      val startAfterMatcher = startAfterPattern.matcher(subtext)
      val endBeforeMatcher = endBeforePattern.matcher(subtext)

      startAfterMatcher.find()
      endBeforeMatcher.find()

      sb.append(startAfterMatcher.group())
      sb.append(replacingText)
      sb.append(endBeforeMatcher.group())

      currPosition = end
    }

    sb.append(baseText, currPosition, baseText.length())
    sb.toString()
  }

  @inline
  private val leadingPipeSpace = compileStripMarginPattern('|')

  @inline
  private def getStripMarginPattern(pipe: Char) =
    if (pipe == '|') leadingPipeSpace else compileStripMarginPattern(pipe)

  private val startAfterForReplaceAllStripMargin = Pattern.compile("\n+")
  @inline
  def replaceAllStripMargin(
      text: String,
      spaces: String,
      pipe: Char,
  ): String = {
    val endBefore = Pattern.compile(s"\\$pipe")
    replaceAll(
      getStripMarginPattern(pipe),
      startAfterForReplaceAllStripMargin,
      endBefore,
      text,
      spaces,
    )
  }

  private val startAfterForReplaceAllLeadingAsterisk = Pattern.compile("\n")
  private val endBeforeForReplaceAllLeadingAsterisk = Pattern
    .compile("([*][^*])")
  @inline
  def replaceAllLeadingAsterisk(trimmed: String, spaces: String): String =
    replaceAll(
      leadingAsteriskSpace,
      startAfterForReplaceAllLeadingAsterisk,
      endBeforeForReplaceAllLeadingAsterisk,
      trimmed,
      spaces,
    )

  // replaces baseText.split("(?={beforeText})")
  @inline
  def splitByBeforeTextMatching(
      baseText: String,
      beforeText: String,
  ): Array[String] = {
    val beforeTextPattern = Pattern.compile(beforeText)
    val matcher = beforeTextPattern.matcher(baseText)

    val res = new scala.collection.mutable.ArrayBuffer[String]()
    var currPosition = 0
    while (matcher.find()) {
      val start = matcher.start()
      if (start != 0) res.append(baseText.substring(currPosition, start))

      currPosition = start
    }
    res.append(baseText.substring(currPosition, baseText.size))

    res.toArray
  }
}
