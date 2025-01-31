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

  val leadingAsteriskSpace = Pattern.compile("\\h*\\r*\\n(\\h*+)(?:[*][^*])?")

  val docstringLine = Pattern
    .compile("^(\\h*+)([*]\\h*+)?.*$", Pattern.MULTILINE)

  private val emptyLines = "\\h*(\\r*\\n\\h*+\\*?\\h*+)*"

  val emptyDocstring = Pattern.compile(s"^/\\*\\*$emptyLines\\*/\\h*+\\r*+$$")

  val onelineDocstring = {
    val oneline = "[^*\\s\\h](?:.*?[^\\s\\h])?"
    Pattern.compile(s"^/\\*\\*$emptyLines($oneline)$emptyLines\\*/\\h*+\\r*+$$")
  }

  val docstringLeadingSpace = Pattern.compile("^\\h++")

  @inline
  private def compileStripMarginPattern(pipe: Char) = Pattern
    .compile(s"\\h*\\r*\\n(\\h*+\\$pipe)?")

  @inline
  def compileStripMarginPatternWithLineContent(pipe: Char) = Pattern
    .compile(s"\\n(\\h*+\\$pipe)?([^\\r\\n]*+)")

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
