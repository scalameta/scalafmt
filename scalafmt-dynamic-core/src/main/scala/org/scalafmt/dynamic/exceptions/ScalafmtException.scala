package org.scalafmt.dynamic.exceptions

import org.scalafmt.interfaces

import scala.util.control.NoStackTrace

case class ScalafmtException(message: String, cause: Throwable)
    extends interfaces.ScalafmtException(message, cause) with NoStackTrace
