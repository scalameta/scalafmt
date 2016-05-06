package org.scalafmt.cli

import java.io.File
import java.util.concurrent.TimeUnit

import org.scalafmt.AlignToken
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
      |// format with predefined custom style
      |scalafmt --style defaultWithAlign -f Code.scala
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

    def printAndExit(inludeUsage: Boolean)(ignore: Unit, c: Config): Config = {
      if (inludeUsage) showUsage
      else showHeader
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
    opt[Unit]('v', "version") action printAndExit(inludeUsage = false) text "print version "
    opt[Unit]('h', "help") action printAndExit(inludeUsage = true) text "prints this usage text"
    opt[(Int, Int)]("range").hidden() action {
      case ((from, to), c) =>
        val offset = if (from == to) 0 else -1
        c.copy(range = c.range + Range(from - 1, to + offset))
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
    } text s"(deprecated) Use assumeStandardLibraryStripMargin, will be removed in 0.3."
    opt[Boolean]("assumeStandardLibraryStripMargin") action { (bool, c) =>
      c.copy(style = c.style.copy(alignStripMarginStrings = bool))
    } text s"See ScalafmtConfig scaladoc."
    opt[Boolean]("spacesInImportCurlyBraces") action { (bool, c) =>
      c.copy(style = c.style.copy(spacesInImportCurlyBrackets = bool))
    } text s"See ScalafmtConfig scaladoc."
    opt[Boolean]("allowNewlineBeforeColonInMassiveReturnTypes") action {
      (bool, c) =>
        c.copy(style = c.style
                .copy(allowNewlineBeforeColonInMassiveReturnTypes = bool))
    } text s"See ScalafmtConfig scaladoc."
    opt[Seq[String]]("alignTokens") action { (tokens, c) =>
      val alignsTokens = tokens.map { token =>
        val splitted = token.split(";", 2)
        if (splitted.length != 2)
          throw new IllegalArgumentException("align token must contain ;")
        AlignToken(splitted(0), splitted(1))
      }
      c.copy(style = c.style.copy(alignTokens = alignsTokens.toSet))
    } text s"""(experimental). Comma separated sequence of tokens to align by. Each
              |        token is a ; separated pair of strings where the first string
              |        is the string literal value of the token to align by and the
              |        second string is a regular expression matching the class
              |        name of the scala.meta.Tree that "owns" that token.
              |
              |        Examples:
              |
              |        1. => in pattern matching
              |        =>;Case
              |
              |        2. Align by -> tuples.
              |        ->;Term.ApplyInfix
              |
              |        NOTE. the closest owner of -> is actually Term.Name,
              |        but in case Term.Name we match against the parent of Term.Name.
              |
              |        3. Assignment of def var/val/def
              |        =;Defn.(Va(l|r)|Def)
              |
              |        4. Comment owned by whatever tree
              |        //;.*
              |
              |        To use all default alignment rules, use --style defaultWithAlign.
              |        To pick your favirite alignment rules, set --alignTokens <value> to:
              |        =>;Case,=;Defn.(Va(l|r)|Def),->;Term.ApplyInfix,//;.*
              |
              |        It's best to play around with scala.meta in a console to
              |        understand which regexp you should use for the owner.
              |""".stripMargin
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
        Scalafmt.format(inputMethod.code,
                        style = config.style,
                        range = config.range) match {
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
      .replaceAll("#(?!;).*", "") // Comments start with #
      .split("\\s+")
      .filterNot(_.isEmpty)
    parser.parse(args, Config.default)
  }

  def getConfig(args: Array[String]): Option[Config] = {
    parser.parse(args, Config.default) match {
      case Some(c) if c.configFile.exists(_.isFile) =>
        parseConfigFile(FileOps.readFile(c.configFile.get))
          .map(x => c.copy(style = x.style))
      case x => x
    }
  }

  def main(args: Array[String]): Unit = {
    getConfig(args).foreach(run)
  }
}
