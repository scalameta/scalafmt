package org.scalafmt.cli

import scala.util.Try
import scala.util.control.NonFatal

import java.io.File
import java.util.Date
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import org.scalafmt.AlignToken
import org.scalafmt.Error.MisformattedFile
import org.scalafmt.FormatResult
import org.scalafmt.Scalafmt
import org.scalafmt.ScalafmtOptimizer
import org.scalafmt.ScalafmtRunner
import org.scalafmt.ScalafmtStyle
import org.scalafmt.Versions
import org.scalafmt.macros.Macros
import org.scalafmt.util.FileOps
import org.scalafmt.util.LoggerOps
import scopt.OptionParser
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
      |--javaDocs # This is a comment.
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
                    debug: Boolean,
                    style: ScalafmtStyle,
                    runner: ScalafmtRunner,
                    range: Set[Range]) {
    require(!(inPlace && testing), "inPlace and testing can't both be true")
  }
  object Config {
    val default = Config(Seq.empty[File],
                         None,
                         inPlace = false,
                         testing = false,
                         debug = false,
                         style = ScalafmtStyle.default,
                         runner = ScalafmtRunner.default,
                         range = Set.empty[Range])
  }

  case class DebugError(filename: String, error: Throwable)

  sealed abstract class InputMethod(val code: String)
  case class StdinCode(override val code: String) extends InputMethod(code)
  case class FileContents(filename: String, override val code: String)
      extends InputMethod(code)

  implicit val styleReads: Read[ScalafmtStyle] = Read.reads { styleName =>
    ScalafmtStyle.availableStyles.getOrElse(styleName.toLowerCase, {
      throw new IllegalArgumentException(
          s"Unknown style name $styleName. Expected one of ${ScalafmtStyle.activeStyles.keys}")
    })
  }

  private def gimmeStrPairs(tokens: Seq[String]): Seq[(String, String)] = {
    tokens.map { token =>
      val splitted = token.split(";", 2)
      if (splitted.length != 2)
        throw new IllegalArgumentException("pair must contain ;")
      (splitted(0), splitted(1))
    }
  }

  def buildInfo =
    s"""build commit: ${Macros.gitCommit.getOrElse("").take(10)}
       |build time: ${new Date(Macros.buildTimeMs)}""".stripMargin

  lazy val scoptParser: OptionParser[Config] =
    new scopt.OptionParser[Config]("scalafmt") {

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
      opt[Unit]("debug") action { (_, c) =>
        c.copy(debug = true)
      } text "print out debug information"
      opt[Unit]("statement") action { (_, c) =>
        c.copy(runner =
              c.runner.copy(parser = scala.meta.parsers.Parse.parseStat))
      } text "parse the input as a statement instead of compilation unit"
      opt[Unit]("bestEffortInDeeplyNestedCode") action { (_, c) =>
        c.copy(runner = c.runner.copy(optimizer =
                  ScalafmtOptimizer.default.copy(bestEffortEscape = true)))
      } text "(experimental) If set, scalafmt will make a best-effort to format deeply nested code instead of failing with SearchStateExplodedException."
      opt[Unit]('v', "version") action printAndExit(inludeUsage = false) text "print version "
      opt[Unit]("build-info") action {
        case (_, c) =>
          println(buildInfo)
          sys.exit
      } text "prints build information"
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
      } text s"base style, must be one of: ${ScalafmtStyle.activeStyles.keys}"
      opt[Int]("maxColumn") action { (col, c) =>
        c.copy(style = c.style.copy(maxColumn = col))
      } text s"See ScalafmtStyle scaladoc."
      opt[Int]("continuationIndentCallSite") action { (n, c) =>
        c.copy(style = c.style.copy(continuationIndentCallSite = n))
      } text s"See ScalafmtStyle scaladoc."
      opt[Int]("continuationIndentDefnSite") action { (n, c) =>
        c.copy(style = c.style.copy(continuationIndentDefnSite = n))
      } text s"See ScalafmtStyle scaladoc."
      opt[Boolean]("reformatDocstrings") action { (b, c) =>
        c.copy(style = c.style.copy(reformatDocstrings = b))
      } text s"See ScalafmtStyle scaladoc."
      opt[Unit]("scalaDocs") action { (_, c) =>
        c.copy(style = c.style.copy(scalaDocs = true))
      } text s"See ScalafmtStyle scaladoc."
      opt[Unit]("javaDocs") action { (_, c) =>
        c.copy(style = c.style.copy(scalaDocs = false))
      } text s"Sets scalaDocs to false. See ScalafmtStyle scaladoc."
      opt[Boolean]("alignStripMarginStrings") action { (bool, c) =>
        c.copy(style = c.style.copy(alignStripMarginStrings = bool))
      } text s"(deprecated) Use assumeStandardLibraryStripMargin, will be removed in 0.3."
      opt[Boolean]("assumeStandardLibraryStripMargin") action { (bool, c) =>
        c.copy(style = c.style.copy(alignStripMarginStrings = bool))
      } text s"See ScalafmtStyle scaladoc."
      opt[Boolean]("alignByOpenParenCallSite") action { (bool, c) =>
        c.copy(style = c.style.copy(alignByOpenParenCallSite = bool))
      } text s"See ScalafmtStyle scaladoc."
      opt[Boolean]("alignByOpenParenDefnSite") action { (bool, c) =>
        c.copy(style = c.style.copy(alignByOpenParenDefnSite = bool))
      } text s"See ScalafmtStyle scaladoc."
      opt[Boolean]("alignByArrowEnumeratorGenerator") action { (bool, c) =>
        c.copy(style = c.style.copy(alignByArrowEnumeratorGenerator = bool))
      } text s"See ScalafmtStyle scaladoc."
      opt[Boolean]("binPackParentConstructors") action { (bool, c) =>
        c.copy(style = c.style.copy(binPackParentConstructors = bool))
      } text s"See ScalafmtStyle scaladoc."
      opt[Boolean]("spacesInImportCurlyBraces") action { (bool, c) =>
        c.copy(style = c.style.copy(spacesInImportCurlyBraces = bool))
      } text s"See ScalafmtStyle scaladoc."
      opt[Boolean]("danglingParentheses") action { (bool, c) =>
        c.copy(style = c.style.copy(configStyleArguments = !bool,
                                    danglingParentheses = bool))
      } text s"See ScalafmtConfig scaladoc. --alignByOpenParenCallSite false is recommended."
      opt[Boolean]("spaceAfterTripleEquals") action { (bool, c) =>
        c.copy(style = c.style.copy(spaceAfterTripleEquals = bool))
      } text s"See ScalafmtConfig scaladoc."
      opt[Boolean]("allowNewlineBeforeColonInMassiveReturnTypes") action {
        (bool, c) =>
          c.copy(style = c.style.copy(
                  allowNewlineBeforeColonInMassiveReturnTypes = bool))
      } text s"See ScalafmtStyle scaladoc."
      opt[Boolean]("unindentTopLevelOperators") action { (bool, c) =>
        c.copy(style = c.style.copy(unindentTopLevelOperators = bool))
      } text s"See ScalafmtConfig scaladoc."
      opt[Boolean]("breakUpLongImportSelectors") action { (bool, c) =>
        c.copy(style = c.style.copy(breakUpLongImportSelectors = bool))
      } text s"See ScalafmtConfig scaladoc."
      opt[String]("indentOperatorsIncludeFilter") action { (str, c) =>
        c.copy(style = c.style.copy(indentOperatorsIncludeFilter = str.r))
      } text s"See ScalafmtConfig scaladoc."
      opt[String]("indentOperatorsExcludeFilter") action { (str, c) =>
        c.copy(style = c.style.copy(indentOperatorsExcludeFilter = str.r))
      } text s"See ScalafmtConfig scaladoc."
      opt[Boolean]("indentOperators") action { (bool, c) =>
        val (include, exclude) = {
          if (bool)
            (ScalafmtStyle.indentOperatorsIncludeDefault,
             ScalafmtStyle.indentOperatorsExcludeDefault)
          else
            (ScalafmtStyle.indentOperatorsIncludeAkka,
             ScalafmtStyle.indentOperatorsExcludeAkka)
        }
        c.copy(style = c.style.copy(indentOperatorsIncludeFilter = include,
                                    indentOperatorsExcludeFilter = exclude))
      } text s"See ScalafmtConfig scaladoc."
      opt[Seq[String]]("rewriteTokens") action { (str, c) =>
        val rewriteTokens = Map(gimmeStrPairs(str): _*)
        c.copy(style = c.style.copy(rewriteTokens = rewriteTokens))
      } text s"""(experimental) Same syntax as alignTokens. For example,
              |
              |        --rewriteTokens ⇒;=>,←;<-
              |
              |        will rewrite unicode arrows to their ascii equivalents.""".stripMargin
      opt[Boolean]("alignMixedOwners") action { (bool, c) =>
        c.copy(style = c.style.copy(alignMixedOwners = bool))
      } text s"See ScalafmtConfig scaladoc."
      opt[Seq[String]]("alignTokens") action { (tokens, c) =>
        val alignsTokens =
          gimmeStrPairs(tokens).map((AlignToken.apply _).tupled)
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

      opt[Unit]("spaceBeforeContextBoundColon") action { (_, c) =>
        c.copy(style = c.style.copy(spaceBeforeContextBoundColon = true))
      } text s"See ScalafmtConfig scaladoc."

      note(s"""
            |Examples:
            |
            |$usageExamples
            |
            |Please file bugs to https://github.com/olafurpg/scalafmt/issues
      """.stripMargin)
    }
  lazy val parser = scoptParser

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
    val errorBuilder = Seq.newBuilder[DebugError]
    val counter = new AtomicInteger()
    inputMethods.par.foreach {
      case inputMethod =>
        val start = System.nanoTime()
        Scalafmt.format(inputMethod.code,
                        style = config.style,
                        range = config.range,
                        runner = config.runner) match {
          case FormatResult.Success(formatted) =>
            inputMethod match {
              case FileContents(filename, _) if config.inPlace =>
                val elapsed = TimeUnit.MILLISECONDS
                  .convert(System.nanoTime() - start, TimeUnit.NANOSECONDS)
                val i = counter.incrementAndGet()
                logger.info(
                    f"${i + 1}%3s/${inputMethods.length} file:$filename%-50s (${elapsed}ms)")
                if (inputMethod.code != formatted) {
                  FileOps.writeFile(filename, formatted)
                }
              case FileContents(filename, _) if config.testing =>
                if (inputMethod.code != formatted) {
                  throw MisformattedFile(new File(filename))
                }
              case _ if !config.debug =>
                println(formatted)
              case _ =>
            }
          case e if config.debug =>
            inputMethod match {
              case FileContents(filename, _) =>
                try e.get
                catch {
                  case NonFatal(error) =>
                    errorBuilder += DebugError(filename, error)
                    logger.error(s"Error in $filename")
                    error.printStackTrace()
                }
              case _ =>
            }
          case _ if !config.inPlace =>
            println(inputMethod.code)
          case _ =>
        }
    }
    if (config.debug) {
      val errors = errorBuilder.result()
      if (errors.nonEmpty) {
        val list = errors.map(x => s"${x.filename}: ${x.error}")
        logger.error(s"""Found ${errors.length} errors:
             |${list.mkString("\n")}
             |""".stripMargin)

      }

    }
  }

  def parseConfigFile(contents: String): Option[Config] = {
    val args = contents
      .replaceAll("#(?!;).*", "") // Comments start with #
      .split("\\s+")
      .filterNot(_.isEmpty)
    scoptParser.parse(args, Config.default)
  }

  def getConfig(args: Array[String]): Option[Config] = {
    scoptParser.parse(args, Config.default) match {
      case Some(c) if c.configFile.exists(_.isFile) =>
        parseConfigFile(FileOps.readFile(c.configFile.get)).map(x =>
              c.copy(style = x.style))
      case x => x
    }
  }

  def main(args: Array[String]): Unit = {
    getConfig(args).foreach(run)
  }
}
