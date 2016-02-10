package org.scalafmt

/**
  * The decision made by [[Formatter]].
  *
  * Used by [[Policy]] to enforce non-local formatting.
  */
case class Decision(formatToken: FormatToken, split: List[Split])