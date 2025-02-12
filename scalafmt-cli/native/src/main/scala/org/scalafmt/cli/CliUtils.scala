package org.scalafmt.cli

import scala.io.Source

private[scalafmt] trait CliUtils {
  protected def getDynamicRunner: Option[ScalafmtRunner] = None

  def readInputLines: Iterator[String] = Source.stdin.getLines()
}
