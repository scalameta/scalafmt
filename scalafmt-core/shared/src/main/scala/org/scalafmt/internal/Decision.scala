package org.scalafmt.internal

/**
  * The decision made by [[Router]].
  *
  * Used by [[Policy]] to enforce non-local formatting.
  */
case class Decision(formatToken: FormatToken, splits: Seq[Split]) {
  import org.scalafmt.util.TokenOps._

  def noNewlines: Decision =
    Decision(formatToken, splits.filterNot(_.modification.isNewline))

  def onlyNewlines(implicit line: sourcecode.Line): Decision = {
    val filtered = splits.filter(_.modification.isNewline)
    if (filtered.nonEmpty) Decision(formatToken, filtered)
    else Decision(formatToken, Seq(Split(Newline, 0)))
  }

  def forceNewline(implicit line: sourcecode.Line): Decision = {
    if (isAttachedSingleLineComment(formatToken.right, formatToken.between))
      this
    else {
      Decision(formatToken, splits.filter(_.modification.isNewline))
    }
  }
}
