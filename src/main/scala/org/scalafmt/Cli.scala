package org.scalafmt

import java.io.File

import scala.io.Source

case class Config(stdin: Boolean = true,
                  file: Option[File] = None, // Must be set if stdin is false.
                  rangeFrom: Option[Int] = None,
                  rangeTo: Option[Int] = None)

object Cli extends App {
  val parser = new scopt.OptionParser[Config]("scalafmt") {
    head("scalafmt", "0.1")
    opt[Int]("from") action { (from, c) =>
      c.copy(rangeFrom = Some(from))
    } text "from must be an int."
    opt[Int]("to") action { (to, c) =>
      c.copy(rangeTo = Some(to))
    } text "to must be an int."
    opt[File]('f', "file") action { (file, c) =>
      c.copy(file = Some(file))
    } text "std must be bool."
  }

  val code = Source.fromInputStream(System.in).getLines().mkString("\n")
  val fmt = new ScalaFmt(Standard)
  val output = fmt.formatSource(code)
  println(output)
}
