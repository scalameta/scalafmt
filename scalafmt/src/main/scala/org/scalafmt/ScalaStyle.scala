package org.scalafmt

trait ScalaStyle {
  def maxColumn: Int = 80
}
case object Standard extends ScalaStyle
case object ScalaFmtTesting extends ScalaStyle {
  override def maxColumn = 40
}
