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

  def onlyNewlinesWithFallback(default: => Split): Decision = {
    val filtered = onlyNewlineSplits
    Decision(formatToken, if (filtered.nonEmpty) filtered else Seq(default))
  }

  def forceNewline: Decision =
    if (isAttachedSingleLineComment(formatToken))
      this
    else
      onlyNewlinesWithoutFallback

  def onlyNewlinesWithoutFallback: Decision =
    Decision(formatToken, onlyNewlineSplits)

  private def onlyNewlineSplits: Seq[Split] =
    splits.filter(_.modification.isNewline)

}
