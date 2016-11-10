package org.scalafmt.cli

import java.io.File
import java.util.Date

import org.scalafmt.Versions
import org.scalafmt.config.Config
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.config.hocon.Hocon2Class
import org.scalafmt.util.AbsoluteFile
import org.scalafmt.util.BuildTime
import org.scalafmt.util.FileOps
import org.scalafmt.util.GitCommit
import org.scalafmt.util.logger
import scopt.OptionParser

object CliArgParser {
  @GitCommit val gitCommit: String = ???
  @BuildTime val buildTimeMs: Long = ???

  val usageExamples =
    """|scalafmt  # Format all files in the current project, with config determined:
       |          # 1. .scalafmt.conf inside root directory of current git repo
       |          #    IF the following setting is enabled: project.git = true
       |          # 2. .scalafmt.conf file in current directory
       |          # 3. default style
       |          # from .scalafmt.conf in the root directory, if the file exists.
       |scalafmt --test # same as without --test, except
       |                # 1. throws an exception on the first mis-formatted file
       |                # 2. does not write to any files.
       |scalafmt --stdin                           # read from stdin and print to stdout
       |scalafmt --stdin --assume-filename foo.sbt # required to format .sbt files
       |
       |scalafmt -f Code.scala             # print formatted contents to stdout.
       |scalafmt -i -f Code1.scala,A.scala # write formatted contents to file.
       |scalafmt -i -f . --exclude target  # format all files in directory excluding target
       |scalafmt --config .scalafmt.conf   # read custom style from file
       |scalafmt --config "style=IntelliJ" # define custom style as a flag, must be quoted.""".stripMargin

  val scoptParser: OptionParser[CliOptions] =
    new scopt.OptionParser[CliOptions]("scalafmt") {
      override def showUsageOnError = false

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
          else
            FileOps.readFile(
              AbsoluteFile.fromFile(new File(file), c.common.workingDirectory))

        Config.fromHocon(contents) match {
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
      opt[Seq[File]]('f', "files").action { (files, c) =>
        c.copy(
          customFiles =
            AbsoluteFile.fromFiles(files, c.common.workingDirectory))
      }.text(
        "file or directory, in which case all *.scala files are formatted.")
      opt[Seq[String]]("exclude")
        .action((excludes, c) => c.copy(customExcludes = excludes))
        .text(
          "file or directory, in which case all *.scala files are formatted.")
      opt[String]('c', "config")
        .action(readConfigFromFile)
        .text(
          "either a file or configuration wrapped in quotes \" with no spaces.")
      opt[Unit]("stdin")
        .action((_, c) => c.copy(stdIn = true))
        .text("read from stdin and print to stdout")
      opt[Unit]("diff")
        .action((_, c) => c.copy(diff = true, inPlace = true))
        .text("read diff output from stdin and" +
          "print to stdout with only diff formatted")
      opt[String]("assume-filename")
        .action((filename, c) => c.copy(assumeFilename = filename))
        .text("required to format .sbt files with --stdin flag.")
      opt[Unit]('i', "in-place")
        .action((_, c) => c.copy(inPlace = true))
        .text("write output to file, does nothing if file is not specified")
      opt[Unit]("test")
        .action((_, c) => c.copy(testing = true))
        .text("test for mis-formatted code, exits with status 1 on failure.")
      opt[File]("migrate2hocon")
        .action((file, c) =>
          c.copy(migrate =
            Some(AbsoluteFile.fromFile(file, c.common.workingDirectory))))
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
