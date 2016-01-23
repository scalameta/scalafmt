package org.scalafmt

trait ArgumentHandling

case object BinPacking extends ArgumentHandling

case object OneArgOneLine extends ArgumentHandling

trait ScalaStyle {
  def maxColumn: Int = 80

  def argumentHandling: ArgumentHandling = OneArgOneLine
}

case object Standard extends ScalaStyle

case object UnitTestStyle extends ScalaStyle {
  override def maxColumn = 40
}
