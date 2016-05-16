package org.scalafmt.internal

/**
  * The decision made by [[Router]].
  *
  * Used by [[Policy]] to enforce non-local formatting.
  */
case class Decision(formatToken: FormatToken, splits: Seq[Split]) {

  def noNewlines: Decision =
    Decision(formatToken, splits.filter(_.modification.isNewline))

  def safeNoNewlines(implicit line: sourcecode.Line): Decision = {
    val filtered = splits.filter(_.modification.isNewline)
    if (filtered.nonEmpty) Decision(formatToken, filtered)
    else Decision(formatToken, Seq(Split(Newline, 0)))
  }
}
