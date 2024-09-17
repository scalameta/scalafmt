package org.scalafmt.internal

import java.util.regex.Pattern

import scala.util.matching.Regex

object RegexCompat {

  @inline
  val trailingSpace = Pattern.compile("\\h++$", Pattern.MULTILINE)

  // "slc" stands for single-line comment
  @inline
  val slcLine = Pattern.compile("^/\\/\\/*+\\h*+(.*?)\\h*+$")

  @inline
  val slcDelim = Pattern.compile("\\h++")

  // "mlc" stands for multi-line comment
  @inline
  val mlcHeader = Pattern.compile("^/\\*\\h*+(?:\n\\h*+[*]*+\\h*+)?")

  @inline
  val mlcLineDelim = Pattern.compile("\\h*+\n\\h*+[*]*+\\h*+")

  @inline
  val mlcParagraphEnd = Pattern.compile("[.:!?=]$")

  @inline
  val mlcParagraphBeg = Pattern.compile("^(?:[-*@=]|\\d++[.:])")

  @inline
  val leadingAsteriskSpace = Pattern.compile("(?<=\n)\\h*+(?=[*][^*])")

  @inline
  val docstringLine = Pattern
    .compile("^(?:\\h*+\\*)?(\\h*+)(.*?)\\h*+$", Pattern.MULTILINE)

  @inline
  val emptyLines = "\\h*+(\n\\h*+\\*?\\h*+)*"

  @inline
  val emptyDocstring = Pattern.compile(s"^/\\*\\*$emptyLines\\*/$$")

  @inline
  val onelineDocstring = {
    val oneline = "[^*\n\\h](?:[^\n]*[^\n\\h])?"
    Pattern.compile(s"^/\\*\\*$emptyLines($oneline)$emptyLines\\*/$$")
  }

  @inline
  val docstringLeadingSpace = Pattern.compile("^\\h++")

  @inline
  def compileStripMarginPattern(pipe: Char) = Pattern
    .compile(s"(?<=\n)\\h*+(?=\\$pipe)")

  @inline
  def compileStripMarginPatternWithLineContent(pipe: Char) = Pattern
    .compile(s"\n(\\h*+\\$pipe)?([^\n]*+)")

  // see: https://ammonite.io/#Save/LoadSession
  @inline
  private val ammonitePattern: Regex = "(?:\\s*\\n@(?=\\s))+".r

  @inline
  val stripMarginPattern = compileStripMarginPattern('|')
  val stripMarginPatternWithLineContent =
    compileStripMarginPatternWithLineContent('|')

  @inline
  def replaceAllStripMargin(
      stripMarginPattern: Pattern,
      text: String,
      spaces: String,
      pipe: Char,
  ): String = stripMarginPattern.matcher(text).replaceAll(spaces)

  @inline
  def replaceAllLeadingAsterisk(
      leadingAsteriskSpace: Pattern,
      trimmed: String,
      spaces: String,
  ): String = leadingAsteriskSpace.matcher(trimmed).replaceAll(spaces)

  @inline
  def splitByAmmonitePattern(code: String): Array[String] = ammonitePattern
    .split(code)

  @inline
  def splitByBeforeTextMatching(
      baseText: String,
      beforeText: String,
  ): Array[String] = baseText.split(s"(?=$beforeText)")
}
