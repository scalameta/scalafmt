package org.scalafmt.cli

import java.io.PrintWriter

class CliOptionsUtils {
  def getConsoleWriter(): Option[PrintWriter] = Option(System.console())
    .map(_.writer)
}
