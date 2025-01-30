package org.scalafmt.internal

import java.util.regex.Pattern

private[scalafmt] object RegexCompat {

  val trailingSpace = Pattern.compile("\\h+\\r*$", Pattern.MULTILINE)

  // "slc" stands for single-line comment
  val slcLine = Pattern.compile("^/\\/\\/*+\\h*+(.*?)\\h*+\\r*+$")

  val slcDelim = Pattern.compile("\\h++")

  // "mlc" stands for multi-line comment
  private val mlcLineDelimPat = "\\r*\\n\\h*+(?:[*]++\\h*+)?"
  val mlcHeader = Pattern.compile(s"^/\\*\\h*+(?:$mlcLineDelimPat)?")
  val mlcLineDelim = Pattern.compile(s"\\h*+$mlcLineDelimPat")

  val mlcParagraphEnd = Pattern.compile("[.:!?=]\\h*+\\r*+$")

  val mlcParagraphBeg = Pattern.compile("^(?:[-*@=]|\\d++[.:])")

  private val leadingAsteriskSpace = Pattern.compile("(?<=\n)\\h*+(?=[*][^*])")

  val docstringLine = Pattern
    .compile("^(?:\\h*+\\*)?(\\h*+)(.*?)\\h*+$", Pattern.MULTILINE)

  private val emptyLines = "\\h*+(\n\\h*+\\*?\\h*+)*"

  val emptyDocstring = Pattern.compile(s"^/\\*\\*$emptyLines\\*/$$")

  val onelineDocstring = {
    val oneline = "[^*\n\\h](?:[^\n]*[^\n\\h])?"
    Pattern.compile(s"^/\\*\\*$emptyLines($oneline)$emptyLines\\*/$$")
  }

  val docstringLeadingSpace = Pattern.compile("^\\h++")

  @inline
  def compileStripMarginPattern(pipe: Char) = Pattern
    .compile(s"(?<=\n)\\h*+(?=\\$pipe)")

  @inline
  def compileStripMarginPatternWithLineContent(pipe: Char) = Pattern
    .compile(s"\n(\\h*+\\$pipe)?([^\n]*+)")

  val stripMarginPatternWithLineContent =
    compileStripMarginPatternWithLineContent('|')

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
