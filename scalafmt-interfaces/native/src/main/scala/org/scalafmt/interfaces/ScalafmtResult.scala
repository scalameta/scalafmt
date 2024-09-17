package org.scalafmt.interfaces;

final class ScalafmtResult(val value: String, val exception: Throwable) {
  def this(value: String) = this(value, null)
  def this(exception: Throwable) = this(null, exception)
}
