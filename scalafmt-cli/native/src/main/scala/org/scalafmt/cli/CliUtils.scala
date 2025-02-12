package org.scalafmt.cli

private[scalafmt] trait CliUtils {
  protected def getDynamicRunner: Option[ScalafmtRunner] = None
}
