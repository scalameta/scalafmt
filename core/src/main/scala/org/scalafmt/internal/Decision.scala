package org.scalafmt.internal

import org.scalafmt._

/**
  * The decision made by [[Router]].
  *
  * Used by [[Policy]] to enforce non-local formatting.
  */
case class Decision(formatToken: FormatToken, splits: Seq[Split])