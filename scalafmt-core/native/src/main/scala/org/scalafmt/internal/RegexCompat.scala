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
    .compile(RegexCompat.fixHorizontalSpaceInRegex("\\h+$"), Pattern.MULTILINE)

  // "slc" stands for single-line comment
  @inline
  val slcLine = Pattern
    .compile(RegexCompat.fixHorizontalSpaceInRegex("^/\\/\\/*\\h*(.*?)\\h*$"))

  @inline
  val slcDelim = Pattern.compile(RegexCompat.fixHorizontalSpaceInRegex("\\h+"))

  // "mlc" stands for multi-line comment
  @inline
  val mlcHeader = Pattern.compile(
    RegexCompat.fixHorizontalSpaceInRegex("^/\\*\\h*(?:\n\\h*[*]*\\h*)?"),
  )

  @inline
  val mlcLineDelim = Pattern
    .compile(RegexCompat.fixHorizontalSpaceInRegex("\\h*\n\\h*[*]*\\h*"))

  @inline
  val mlcParagraphEnd = Pattern.compile("[.:!?=]$")

  @inline
  val mlcParagraphBeg = Pattern.compile("^(?:[-*@=]|\\d+[.:])")

  @inline
  val leadingAsteriskSpace = Pattern.compile(
    RegexCompat.fixHorizontalSpaceInRegex("\n\\h*[*][^*]"),
    Pattern.MULTILINE,
  )

  @inline
  val docstringLine = Pattern.compile(
    RegexCompat.fixHorizontalSpaceInRegex("^(?:\\h*\\*)?(\\h*)(.*?)\\h*$"),
    Pattern.MULTILINE,
  )

  @inline
  val emptyLines = RegexCompat.fixHorizontalSpaceInRegex("\\h*(\n\\h*\\*?\\h*)*")

  @inline
  val emptyDocstring = Pattern.compile(s"^/\\*\\*$emptyLines\\*/$$")

  @inline
  val onelineDocstring = {
    val oneline = RegexCompat
      .fixHorizontalSpaceInRegex("[^*\n\\h](?:[^\n]*[^\n\\h])?")
    Pattern.compile(RegexCompat.fixHorizontalSpaceInRegex(
      s"^/\\*\\*$emptyLines($oneline)$emptyLines\\*/$$",
    ))
  }

  @inline
  val docstringLeadingSpace = Pattern
    .compile(RegexCompat.fixHorizontalSpaceInRegex("^\\h+"))

  @inline
  def compileStripMarginPattern(pipe: Char) = Pattern.compile(
    RegexCompat.fixHorizontalSpaceInRegex(s"\n+\\h*?\\$pipe"),
    Pattern.MULTILINE,
  )

  @inline
  def compileStripMarginPatternWithLineContent(pipe: Char) = Pattern.compile(
    RegexCompat.fixHorizontalSpaceInRegex(s"\n(\\h*\\$pipe)?([^\n]*)"),
    Pattern.MULTILINE,
  )

  val stripMarginPatternWithLineContent =
    compileStripMarginPatternWithLineContent('|')

  // see: https://ammonite.io/#Save/LoadSession
  @inline
  val ammonitePattern: Regex = "(?:\\s*\\n@)".r

  @inline
  val stripMarginPattern = Pattern.compile(
    RegexCompat.fixHorizontalSpaceInRegex("\n(\\h*\\|)?([^\n]*)"),
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
    val sb = new StringBuilder()
    val matcher = basePattern.matcher(baseText)
    var currPosition = 0
    while (matcher.find()) {
      val start = matcher.start()
      val end = matcher.end()

      sb.append(baseText.substring(currPosition, start))

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

    sb.append(baseText.substring(currPosition))
    sb.toString()
  }

  private val startAfterForReplaceAllStripMargin = Pattern.compile("\n+")
  def replaceAllStripMargin(
      stripMarginPattern: Pattern,
      text: String,
      spaces: String,
      pipe: Char,
  ): String = {
    val endBefore = Pattern.compile(s"\\$pipe")
    replaceAll(
      stripMarginPattern,
      startAfterForReplaceAllStripMargin,
      endBefore,
      text,
      spaces,
    )
  }

  private val startAfterForReplaceAllLeadingAsterisk = Pattern.compile("\n")
  private val endBeforeForReplaceAllLeadingAsterisk = Pattern
    .compile("([*][^*])")
  def replaceAllLeadingAsterisk(
      leadingAsteriskSpace: Pattern,
      trimmed: String,
      spaces: String,
  ): String = replaceAll(
    leadingAsteriskSpace,
    startAfterForReplaceAllLeadingAsterisk,
    endBeforeForReplaceAllLeadingAsterisk,
    trimmed,
    spaces,
  )

  private val whitespacePatternForSplitByAmmonitePattern = Pattern.compile("\\s")
  def splitByAmmonitePattern(code: String): Array[String] = {
    val actualMatches = ammonitePattern.findAllMatchIn(code)
      .filter(regexMatch =>
        regexMatch.end < code.length &&
          whitespacePatternForSplitByAmmonitePattern
            .matcher(Character.toString(code.charAt(regexMatch.end))).find(),
      ).toArray

    val res = new scala.collection.mutable.ArrayBuffer[String]()
    var currPosition = 0
    for (actualMatch <- actualMatches) {
      if (currPosition != actualMatch.start) res
        .append(code.substring(currPosition, actualMatch.start))
      currPosition = actualMatch.end
    }
    res.append(code.substring(currPosition))
    res.toArray
  }

  // replaces baseText.split("(?={beforeText})")
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
      // val end = matcher.end()
      if (start != 0) res.append(baseText.substring(currPosition, start))

      currPosition = start
    }
    res.append(baseText.substring(currPosition, baseText.size))

    res.toArray
  }
}
