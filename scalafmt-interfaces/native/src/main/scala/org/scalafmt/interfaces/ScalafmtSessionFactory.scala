package org.scalafmt.interfaces

import java.nio.file.Path

trait ScalafmtSessionFactory {

  /** Create a ScalafmtSession to format a batch of files using fixed
    * configuration.
    */
  def createSession(config: Path): ScalafmtSession
}
