package org.scalafmt

class ScalaFmtException(msg: String) extends Exception(msg)
case object TooManyIndentPops extends ScalaFmtException("Too many Indent Pop.")

