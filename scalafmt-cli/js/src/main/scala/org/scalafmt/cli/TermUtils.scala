package org.scalafmt.cli

import scala.scalajs.js
import scala.scalajs.js.Date

private[scalafmt] trait TermUtils {
  protected def formatTimestamp(ts: Long): String = new Date(ts).toISOString()
  def noConsole = TermUtils.getJSConsole.isEmpty
}

private[scalafmt] object TermUtils {
  def getJSConsole =
    if (js.typeOf(js.Dynamic.global.console) == "undefined") None
    else Option(js.Dynamic.global.console)
}
