package org.scalafmt.cli

import java.io.File
import java.util.concurrent.TimeUnit

import org.scalafmt.Error.MisformattedFile
import org.scalafmt.FormatResult
import org.scalafmt.Scalafmt
import org.scalafmt.ScalafmtStyle
import org.scalafmt.Versions
import org.scalafmt.util.FileOps
import org.scalafmt.util.LoggerOps
import scopt.Read

object Cli {
  import LoggerOps._
  val usageExamples =
    """
      |// get help
      |scalafmt --help
      |
      |// print formatted contents of file to stdout.
      |scalafmt -f Code.scala
      |
      |// write formatted contents to file.
      |scalafmt -i -f Code1.scala,Code2.scala
      |
      |// read style options from a configuration file
      |$ cat .scalafmt
      |--maxColumn 120
      |--javaDocs // This is a comment.
      |$ scalafmt --config .scalafmt -i -f Code.scala
      |
      |// format all files in current directory, write new contents to each file.
      |scalafmt -i -f .
      |
      |// read scala code from stdin and print formatted contents to stdout.
      |scalafmt
    """.stripMargin

  case class Config(files: Seq[File],
                    configFile: Option[File],
                    inPlace: Boolean,
                    testing: Boolean,
                    style: ScalafmtStyle,
                    range: Set[Range]) {
    require(!(inPlace && testing), "inPlace and testing can't both be true")
  }
  object Config {
    val default = Config(Seq.empty[File],
                         None,
                         inPlace = false,
                         testing = false,
                         style = ScalafmtStyle.default,
                         Set.empty[Range])
  }

  sealed abstract class InputMethod(val code: String)
  case class StdinCode(override val code: String) extends InputMethod(code)
  case class FileContents(filename: String, override val code: String)
      extends InputMethod(code)

  implicit val styleReads: Read[ScalafmtStyle] = Read.reads { styleName =>
    ScalafmtStyle.availableStyles.getOrElse(styleName, {
      throw new IllegalArgumentException(
          s"Unknown style name $styleName. Expected one of ${ScalafmtStyle.availableStyles.keys}")
    })
  }

  lazy val parser = new scopt.OptionParser[Config]("scalafmt") {

    def printHelpAndExit(ignore: Unit, c: Config): Config = {
      showUsage
      sys.exit
      c
    }

    head("scalafmt", Versions.nightly)
    opt[Seq[File]]('f', "files") action { (files, c) =>
      c.copy(files = files)
    } text "can be directory, in which case all *.scala files are formatted. " +
    "If not provided, reads from stdin."
    opt[File]('c', "config") action { (file, c) =>
      c.copy(configFile = Some(file))
    } text "read style flags, see \"Style configuration option\", from this" +
    " config file. The file can contain comments starting with //"
    opt[Unit]('i', "in-place") action { (_, c) =>
      c.copy(inPlace = true)
    } text "write output to file, does nothing if file is not specified"
    opt[Unit]("test") action { (_, c) =>
      c.copy(testing = true)
    } text "test for mis-formatted code, exits with status 1 on failure."
    opt[Unit]('v', "version") action printHelpAndExit text "print version "
    opt[Unit]('h', "help") action printHelpAndExit text "prints this usage text"
    opt[(Int, Int)]("range").hidden() action {
      case ((from, to), c) =>
        c.copy(range = c.range + Range(from - 1, to - 1))
    } text "(experimental) only format line range from=to"

    // Style configs
    note(s"\nStyle configuration options:")
    opt[ScalafmtStyle]('s', "style") action { (style, c) =>
      c.copy(style = style)
    } text s"base style, must be one of: ${ScalafmtStyle.availableStyles.keys}"
    opt[Int]("maxColumn") action { (col, c) =>
      c.copy(style = c.style.copy(maxColumn = col))
    } text s"See ScalafmtConfig scaladoc."
    opt[Int]("continuationIndentCallSite") action { (n, c) =>
      c.copy(style = c.style.copy(continuationIndentCallSite = n))
    } text s"See ScalafmtConfig scaladoc."
    opt[Int]("continuationIndentDefnSite") action { (n, c) =>
      c.copy(style = c.style.copy(continuationIndentDefnSite = n))
    } text s"See ScalafmtConfig scaladoc."
    opt[Unit]("scalaDocs") action { (_, c) =>
      c.copy(style = c.style.copy(scalaDocs = true))
    } text s"See ScalafmtConfig scaladoc."
    opt[Unit]("javaDocs") action { (_, c) =>
      c.copy(style = c.style.copy(scalaDocs = false))
    } text s"Sets scalaDocs to false. See ScalafmtConfig scaladoc."
    opt[Boolean]("alignStripMarginStrings") action { (bool, c) =>
      c.copy(style = c.style.copy(alignStripMarginStrings = bool))
    } text s"See ScalafmtConfig scaladoc."
    opt[Seq[String]]("alignTokens") action { (tokens, c) =>
      c.copy(style = c.style.copy(alignTokens = tokens.toSet))
    } text s"See ScalafmtConfig scaladoc."
    note(s"""
            |Examples:
            |
            |$usageExamples
            |
            |Please file bugs to https://github.com/olafurpg/scalafmt/issues
      """.stripMargin)
  }

  def getCode(config: Config): Seq[InputMethod] = {
    if (config.files.isEmpty) {
      val contents =
        scala.io.Source.fromInputStream(System.in).getLines().mkString("\n")
      Seq(StdinCode(contents))
    } else {
      config.files.flatMap { file =>
        FileOps.listFiles(file).withFilter(_.endsWith(".scala")).map {
          filename =>
            val contents = FileOps.readFile(filename)
            FileContents(filename, contents)
        }
      }
    }
  }

  def run(config: Config): Unit = {
    val inputMethods = getCode(config)
    inputMethods.zipWithIndex.foreach {
      case (inputMethod, i) =>
        val start = System.nanoTime()
        Scalafmt.format(inputMethod.code, style = config.style) match {
          case FormatResult.Success(formatted) =>
            inputMethod match {
              case FileContents(filename, _) if config.inPlace =>
                val elapsed = TimeUnit.MILLISECONDS
                  .convert(System.nanoTime() - start, TimeUnit.NANOSECONDS)
                logger.info(
                    f"${i + 1}%3s/${inputMethods.length} file:$filename%-50s (${elapsed}ms)")
                if (inputMethod.code != formatted) {
                  FileOps.writeFile(filename, formatted)
                }
              case FileContents(filename, _) if config.testing =>
                if (inputMethod.code != formatted) {
                  throw MisformattedFile(new File(filename))
                }
              case _ =>
                println(formatted)
            }
          case _ =>
        }
    }
  }

  def parseConfigFile(contents: String): Option[Config] = {
    val args = contents
      .replaceAll("//.*$", "") // Allow comments
      .split("\\s")
    parser.parse(args, Config.default)
  }

  def getConfig(args: Array[String]): Option[Config] = {
    parser.parse(args, Config.default) match {
      case Some(c) if c.configFile.isDefined =>
        parseConfigFile(FileOps.readFile(c.configFile.get))
          .map(x => c.copy(style = x.style))
      case x => x
    }
  }

  def main(args: Array[String]): Unit = {
    getConfig(args).foreach(run)
  }
}
