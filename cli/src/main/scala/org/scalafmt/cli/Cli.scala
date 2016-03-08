package org.scalafmt.cli

import java.io.File
import java.nio.file.Files
import java.nio.file.Paths

import org.scalafmt.ScalaFmt
import org.scalafmt.ScalaStyle
import org.scalafmt.Versions
import org.scalafmt.internal.Debug
import org.scalafmt.internal.ScalaFmtLogger
import org.scalafmt.util.FilesUtil

object Cli extends ScalaFmtLogger {

  case class Config(file: Option[File] = None,
                    inPlace: Boolean = false,
                    range: Set[Range] = Set.empty[Range])

  sealed abstract class InputMethod(val code: String)

  case class StdinCode(override val code: String) extends InputMethod(code)

  case class FileContents(filename: String, override val code: String)
      extends InputMethod(code)

  lazy val parser = new scopt.OptionParser[Config]("scalafmt") {
    head("scalafmt", Versions.scalafmt)
    opt[File]('f', "file") action { (file, c) =>
      c.copy(file = Some(file))
    } text "can be directory, in which case all *.scala files are formatted. If not provided, reads from stdin."
    opt[Unit]('i', "in-place") action { (_, c) =>
      c.copy(inPlace = true)
    } text "write output to file, does nothing if file is not specified"
    help("help") text "prints this usage text"
    note("""
        |Examples:
        |
        |  // write formatted contents to file.
        |  scalafmt -i -f Code.scala
        |
        |  // format all files in current directory, write new contents to each file.
        |  scalafmt -i -f .
        |
        |  // print formatted contents of file to stdout.
        |  scalafmt -f Code.scala
        |
        |  // read scala code from stdin and print formatted contents to stdout.
        |  scalafmt
        |
        |Please file bugs to https://github.com/olafurpg/scalafmt/issues
      """.stripMargin)
    opt[(Int, Int)]("range") hidden () action {
      case ((from, to), c) => c.copy(range = c.range + Range(from - 1, to - 1))
    } text "(experimental) only format line range from=to"
  }

  def getCode(config: Config): Seq[InputMethod] =
    config.file match {
      case Some(file) =>
        FilesUtil.listFiles(file).withFilter(_.endsWith(".scala")).map {
          filename =>
            val contents = FilesUtil.readFile(filename)
            FileContents(filename, contents)
        }
      case _ =>
        val contents =
          scala.io.Source.fromInputStream(System.in).getLines().mkString("\n")
        Seq(StdinCode(contents))
    }

  def run(config: Config): Unit = {
    val inputMethods = getCode(config)
    inputMethods.zipWithIndex.foreach {
      case (inputMethod, i) =>
        val start = System.nanoTime()
        val formatted = ScalaFmt.format(inputMethod.code)
        inputMethod match {
          case FileContents(filename, _) if config.inPlace =>
            if (inputMethod.code != formatted) {
              FilesUtil.writeFile(filename, formatted)
            }
            val elapsed = Debug.ns2ms(System.nanoTime() - start)
            logger.info(
                f"${i + 1}%3s/${inputMethods.length} file:$filename%-50s (${elapsed}ms)")
          case _ => println(formatted)
        }
    }
  }

  def getConfig(args: Array[String]): Option[Config] = {
    parser.parse(args, Config(None, inPlace = false, Set.empty[Range]))
  }

  def main(args: Array[String]): Unit = {
    getConfig(args).foreach(run)
  }
}
