package org.scalafmt.dynamic.exceptions

case class ScalafmtConfigException(e: String) extends Exception(e)
