package org.scalafmt.internal

import java.util.regex.Pattern

private[scalafmt] object RegexCompat {

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
  private val leadingAsteriskSpace = Pattern.compile("(?<=\n)\\h*+(?=[*][^*])")

  @inline
  val docstringLine = Pattern
    .compile("^(?:\\h*+\\*)?(\\h*+)(.*?)\\h*+$", Pattern.MULTILINE)

  @inline
  private val emptyLines = "\\h*+(\n\\h*+\\*?\\h*+)*"

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

  @inline
  val stripMarginPattern = compileStripMarginPattern('|')

  @inline
  val stripMarginPatternWithLineContent =
    compileStripMarginPatternWithLineContent('|')

  @inline
  private val leadingPipeSpace = compileStripMarginPattern('|')

  @inline
  private def getStripMarginPattern(pipe: Char) =
    if (pipe == '|') leadingPipeSpace else compileStripMarginPattern(pipe)

  @inline
  def replaceAllStripMargin(text: String, spaces: String, pipe: Char): String =
    getStripMarginPattern(pipe).matcher(text).replaceAll(spaces)

  @inline
  def replaceAllLeadingAsterisk(trimmed: String, spaces: String): String =
    leadingAsteriskSpace.matcher(trimmed).replaceAll(spaces)

  // Replaces baseText.split("(?={beforeText})")
  @inline
  def splitByBeforeTextMatching(
      baseText: String,
      beforeText: String,
  ): Array[String] = baseText.split(s"(?=$beforeText)")
}
