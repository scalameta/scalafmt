package org.scalafmt.cli

import java.sql.Timestamp

private[scalafmt] trait TermUtils {

  // Copy/pasted over from coursier, but unused in scalafmt
  private val format = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
  protected def formatTimestamp(ts: Long): String = format
    .format(new Timestamp(ts))

  def noConsole = System.console() == null
}
