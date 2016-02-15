package org.scalafmt

import java.io.File
import scala.meta. _

case class Config(file: Option[File], inPlace: Boolean, range: Option[Range])

object Cli extends App with ScalaFmtLogger {
  val parser = new scopt.OptionParser[Config]("scalafmt") {
    head("scalafmt", "0.1")
    opt[( Int,
        Int)]("range") action {
      case ((from, to), c) => c.copy(range = Some(Range(from - 1, to - 1)))
    } text "only format line range from=to"
    opt[File]('f', "file") action {
      ( file, c) =>
      c.copy(file = Some(file))
    } text "if not provided, reads from stdin"
    opt[Unit]('i', "in-place") action {
      ( _, c) =>
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
  parser.parse(args,
               Config(None,
                      inPlace =
                      false,
                      None)) match {
    case Some(config) =>
      val code = getCode(config)
      val fmt = new ScalaFmt(Standard)
      val output = fmt.format[scala.meta.Source](code, config.range)
      config match {
        case Config(Some(filename), true, _) =>
          val path = java.nio.file.Paths.get(filename.toURI)
          java.nio.file.Files.write(path, output.getBytes)
        case _ => println(output)
      }
    case _ =>
  }
}