package org.scalafmt.cli

private[scalafmt] trait TermUtils {

  // Copy/pasted over from coursier, but not used in scalafmt
  protected def formatTimestamp(ts: Long): String = ???

  def noConsole = false
}
