package org.scalafmt.interfaces

class ScalafmtException(message: String, cause: Throwable)
    extends RuntimeException(message, cause, true, false)
