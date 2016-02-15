package org.scalafmt

import java.nio.file.Files
import java.nio.file.Paths

object Benchmark  {
  def main (args: Array[String]) {
    val filename = "src/main/scala/org/scalafmt/Formatter.scala"
    val code = new String(Files.readAllBytes(Paths.get(filename)))
    val formatted = ScalaFmt.format(code, Standard)
    println(formatted)
  }
}
