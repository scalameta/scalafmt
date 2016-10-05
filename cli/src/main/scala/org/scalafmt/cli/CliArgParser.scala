package org.scalafmt.cli

import java.io.File
import java.util.Date

import org.scalafmt.Versions
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.config.hocon.Hocon2Class
import org.scalafmt.util.BuildTime
import org.scalafmt.util.FileOps
import org.scalafmt.util.GitCommit
import scopt.OptionParser

object CliArgParser {
  @GitCommit val gitCommit: String = ???
  @BuildTime val buildTimeMs: Long = ???
  val usageExamples =
    """|scalafmt        # format all files in current git repo, reads configuration
       |                # from .scalafmt.conf in the root directory, if the file exists.
       |scalafmt --test # throw error if any file in current project is mis-formatted.
       |scalafmt --stdin                           # read from stdin and print to stdout
       |scalafmt --stdin --assume-filename foo.sbt # required to format .sbt files
       |scalafmt -f Code.scala,Code2.scala # print formatted contents to stdout.
       |scalafmt -i -f Code1.scala         # write formatted contents to file.
       |scalafmt -i -f . --exclude target  # format all files in directory excluding target
       |scalafmt --config "style=IntelliJ" # define custom style as a flag
       |scalafmt --config .scalafmt.conf   # read custom style from file
       |""".stripMargin

  val scoptParser: OptionParser[CliOptions] =
    new scopt.OptionParser[CliOptions]("scalafmt") {

      def printAndExit(inludeUsage: Boolean)(ignore: Unit,
                                             c: CliOptions): CliOptions = {
        if (inludeUsage) showUsage
        else showHeader
        sys.exit
        c
      }

      def readConfigFromFile(file: String, c: CliOptions): CliOptions = {
        val contents =
          if (file.startsWith("\""))
            file.stripPrefix("\"").stripSuffix("\"")
          else FileOps.readFile(file)
        Hocon2Class
          .gimmeClass[ScalafmtConfig](contents, c.config.reader, None) match {
          case Right(style) => c.copy(config = style)
          case Left(e) => throw e
        }
      }

      head("scalafmt", Versions.nightly)
      opt[Unit]('h', "help")
        .action(printAndExit(inludeUsage = true))
        .text("prints this usage text")
      opt[Unit]('v', "version")
        .action(printAndExit(inludeUsage = false))
        .text("print version ")
      opt[Seq[File]]('f', "files")
        .action((files, c) =>
          c.copy(
            config = c.config.copy(
              project = c.config.project.copy(
                files = c.config.project.files ++ files.map(_.getPath),
                git = false // if you want to define both, write it in --config
              )
            )
        ))
        .text(
          "file or directory, in which case all *.scala files are formatted.")
      opt[Seq[String]]("exclude")
        .action(
          (files, c) =>
            c.copy(
              config = c.config.copy(
                project = c.config.project.copy(
                  excludeFilter = files
                )
              )
          ))
        .text(
          "file or directory, in which case all *.scala files are formatted.")
      opt[String]('c', "config")
        .action(readConfigFromFile)
        .text(
          "either a file or configuration wrapped in quotes \" with no spaces.")
      opt[Unit]('i', "in-place")
        .action((_, c) => c.copy(inPlace = true))
        .text("write output to file, does nothing if file is not specified")
      opt[Unit]("test")
        .action((_, c) => c.copy(testing = true))
        .text("test for mis-formatted code, exits with status 1 on failure.")
      opt[File]("migrate2hocon")
        .action((file, c) => c.copy(migrate = Some(file)))
        .text("""migrate .scalafmt CLI style configuration to hocon style configuration in .scalafmt.conf""")
      opt[Unit]("build-info")
        .action({
          case (_, c) =>
            println(buildInfo)
            sys.exit
        })
        .text("prints build information")
      opt[(Int, Int)]("range")
        .hidden()
        .action({
          case ((from, to), c) =>
            val offset = if (from == to) 0 else -1
            c.copy(range = c.range + Range(from - 1, to + offset))
        })
        .text("(experimental) only format line range from=to")

      note(s"""|Examples:
               |$usageExamples
               |Please file bugs to https://github.com/olafurpg/scalafmt/issues
      """.stripMargin)
    }
  def buildInfo =
    s"""build commit: $gitCommit
       |build time: ${new Date(buildTimeMs)}""".stripMargin
}
