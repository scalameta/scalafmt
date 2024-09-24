package org.scalafmt.cli

import java.io.PrintWriter

private[scalafmt] trait CliOptionsUtils {
  def getConsoleWriter(): Option[PrintWriter] = None
}
