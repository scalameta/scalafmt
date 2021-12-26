package org.scalafmt.internal

/** The decision made by [[Router]].
  *
  * Used by [[Policy]] to enforce non-local formatting.
  */
case class Decision(formatToken: FormatToken, splits: Seq[Split]) {
  import org.scalafmt.util.TokenOps._

  @inline def noNewlines: Seq[Split] =
    Decision.noNewlineSplits(splits)

  @inline def onlyNewlinesWithFallback(default: => Split): Seq[Split] =
    Decision.onlyNewlinesWithFallback(splits, default)

  def forceNewline: Seq[Split] =
    if (isAttachedSingleLineComment(formatToken))
      splits
    else
      onlyNewlinesWithoutFallback

  def onlyNewlinesWithoutFallback: Seq[Split] =
    onlyNewlineSplits

  @inline private def onlyNewlineSplits: Seq[Split] =
    Decision.onlyNewlineSplits(splits)

  def withSplits(splits: Seq[Split]): Decision = copy(splits = splits)

}

object Decision {

  @inline def noNewlineSplits(s: Seq[Split]): Seq[Split] =
    filterNewlineSplits(s, false)

  @inline def onlyNewlineSplits(s: Seq[Split]): Seq[Split] =
    filterNewlineSplits(s, true)

  @inline def filterNewlineSplits(s: Seq[Split], isNL: Boolean): Seq[Split] =
    s.filter(_.isNL == isNL)

  def onlyNewlinesWithFallback(s: Seq[Split], fb: => Split): Seq[Split] = {
    val filtered = onlyNewlineSplits(s)
    if (filtered.nonEmpty) filtered else Seq(fb)
  }

}
