package org.scalafmt.internal

import java.util.regex.Pattern

private[scalafmt] object RegexCompat {

  @inline
  private def pat(str: String, flags: Int = 0): Pattern = Pattern
    .compile(str, flags)

  // "slc" stands for single-line comment
  val slcLine = pat("^/\\/\\/*\\h*(.*?)\\h*\\r*$")

  val slcDelim = pat("\\h+")

  // "mlc" stands for multi-line comment
  private val mlcLineDelimPat = "\\r*\\n\\h*(?:[*]+\\h*)?"
  val mlcHeader = pat(s"^/\\*\\h*(?:$mlcLineDelimPat)?")
  val mlcLineDelim = pat(s"\\h*$mlcLineDelimPat")

  val mlcParagraphEnd = pat("[.:!?=]\\h*\\r*$")

  val mlcParagraphBeg = pat("^(?:[-*@=]|\\d+[.:])")

  val leadingAsteriskSpace = pat("\\h*\\r*\\n(\\h*)[*]?")

  val docstringLine = pat("^(\\h*)([*]\\h*)?.*$", Pattern.MULTILINE)

  private val emptyLines = "\\h*(\\r*\\n\\h*\\*?\\h*)*"

  val emptyDocstring = pat(s"^/\\*\\*$emptyLines\\*/\\h*\\r*$$")

  val onelineDocstring = {
    val oneline = "[^*\\s\\h](?:.*?[^\\s\\h])?"
    pat(s"^/\\*\\*$emptyLines($oneline)$emptyLines\\*/\\h*\\r*$$")
  }

  val docstringLeadingSpace = pat("^\\h+")

  @inline
  def compileStripMarginPattern(pipe: Char) = pat(s"\\r*\\n(\\h*\\$pipe)?")

  @inline
  def compileStripMarginPatternWithLineContent(pipe: Char) =
    pat(s"\\n(\\h*\\$pipe)?([^\\r\\n]*)")

  val stripMarginPatternWithLineContent =
    compileStripMarginPatternWithLineContent('|')

  private val leadingPipeSpace = compileStripMarginPattern('|')

  @inline
  def getStripMarginPattern(pipe: Char) =
    if (pipe == '|') leadingPipeSpace else compileStripMarginPattern(pipe)

  // Replaces baseText.split("(?={beforeText})")
  @inline
  def splitByBeforeTextMatching(
      baseText: String,
      beforeText: String,
  ): Array[String] = baseText.split(s"(?=$beforeText)")
}
