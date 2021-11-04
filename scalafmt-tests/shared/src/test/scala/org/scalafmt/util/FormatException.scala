package org.scalafmt.util

case class FormatException(exc: Throwable, code: String) extends Exception(exc)
