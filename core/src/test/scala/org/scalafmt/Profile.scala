package org.scalafmt

object Profile {
  def main(args: Array[String]) {
    val tests = ManualTests.tests
    val fmt = new ScalaFmt(ManualTestStyle)
    tests.foreach { test =>
      println(test.filename)
      fmt.format_![scala.meta.Source](test.original)(scala.meta.parsers.parseSource)
    }
  }
}
