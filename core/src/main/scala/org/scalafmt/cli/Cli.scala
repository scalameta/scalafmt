package org.scalafmt.cli

import java.io.File

import org.scalafmt.ScalaFmt
import org.scalafmt.ScalaStyle
import org.scalafmt.internal.ScalaFmtLogger


object Cli extends ScalaFmtLogger {
  case class Config(file: Option[File], inPlace: Boolean, range: Option[Range])
  lazy val parser = new scopt.OptionParser[Config]("scalafmt") {
    head("scalafmt", "0.1")
    opt[(Int,
      Int)]("range") action {
      case ((from, to), c) => c.copy(range = Some(Range(from - 1, to - 1)))
    } text "only format line range from=to"
    opt[File]('f', "file") action {
      (file, c) =>
        c.copy(file = Some(file))
    } text "if not provided, reads from stdin"
    opt[Unit]('i', "in-place") action {
      (_, c) =>
        c.copy(inPlace =
          true)
    } text "write output to file, does nothing if file is not specified"
  }

  def getCode(config: Config): String =
    config.file match {
      case Some(file) =>
        new String(java.nio.file.Files.readAllBytes(java.nio.file.Paths
          .get(file.toURI)))
      case _ =>
        scala.io.Source.fromInputStream(System.in).getLines().mkString("\n")
    }

  def run(config: Config): Unit = {
    val code = getCode(config)
    val output = ScalaFmt.format(code, ScalaStyle.Standard, config.range)
    config match {
      case Config(Some(filename), true, _) =>
        val path = java.nio.file.Paths.get(filename.toURI)
        java.nio.file.Files.write(path, output.getBytes)
      case _ => println(output)
    }
  }

  def main(args: Array[String]) {
    parser
      .parse(args, Config(None, inPlace = false, None))
      .foreach(run)
  }
}