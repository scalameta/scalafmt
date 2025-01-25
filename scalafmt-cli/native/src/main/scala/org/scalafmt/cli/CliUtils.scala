package org.scalafmt.cli

private[scalafmt] trait CliUtils {
  protected val isScalaNative: Boolean = true

  protected def getDynamicRunner: Option[ScalafmtRunner] = None
}
