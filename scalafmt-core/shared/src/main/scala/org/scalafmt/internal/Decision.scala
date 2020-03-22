package org.scalafmt.internal

/**
  * The decision made by [[Router]].
  *
  * Used by [[Policy]] to enforce non-local formatting.
  */
case class Decision(formatToken: FormatToken, splits: Seq[Split]) {
  import org.scalafmt.util.TokenOps._

  def noNewlines: Seq[Split] =
    splits.filterNot(_.isNL)

  def onlyNewlinesWithFallback(default: => Split): Seq[Split] = {
    val filtered = onlyNewlineSplits
    if (filtered.nonEmpty) filtered else Seq(default)
  }

  def forceNewline: Seq[Split] =
    if (isAttachedSingleLineComment(formatToken))
      splits
    else
      onlyNewlinesWithoutFallback

  def onlyNewlinesWithoutFallback: Seq[Split] =
    onlyNewlineSplits

  private def onlyNewlineSplits: Seq[Split] =
    splits.filter(_.isNL)

  def withSplits(splits: Seq[Split]): Decision = copy(splits = splits)

}
