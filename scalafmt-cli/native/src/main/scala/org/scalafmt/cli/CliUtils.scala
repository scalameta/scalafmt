package org.scalafmt.cli

trait CliUtils {
  protected val isNative: Boolean = true

  protected def returnDynamicRunner(): Either[String, ScalafmtRunner] = {
    assert(false, "Code path should be unreachable.")
    ???
  }
}
