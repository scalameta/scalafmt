package org.scalafmt.cli

import org.scalafmt.Versions

import java.nio.file.Path
import java.nio.file.Paths
import java.util.Date

import scopt.OptionParser

object CliArgParser {

  implicit val readPath: scopt.Read[Path] = scopt.Read.reads(Paths.get(_))

  val usageExamples: String =
    """|scalafmt # Format all files in the current project, configuration is determined in this order:
       |         # 1. .scalafmt.conf file in current directory
       |         # 2. .scalafmt.conf inside root directory of current git repo
       |         # 3. no configuration, default style
       |scalafmt --test # throw exception on mis-formatted files, won't write to files.
       |scalafmt --mode diff # Format all files that were edited in git diff against master branch.
       |scalafmt --mode changed # Format files listed in `git status` (latest changes against previous commit.
       |scalafmt --diff-branch 2.x # same as --diff, except against branch 2.x
       |scalafmt --stdin # read from stdin (doesn't imply --stdout)
       |scalafmt --stdin --assume-filename foo.sbt < foo.sbt # required when using --stdin to format .sbt files.
       |scalafmt Code1.scala A.scala       # write formatted contents to file.
       |scalafmt --stdout Code.scala       # print formatted contents to stdout.
       |scalafmt --exclude target          # format all files in directory excluding target
       |scalafmt --config .scalafmt.conf   # read custom style from file.
       |scalafmt --config-str "style=IntelliJ" # define custom style as a flag, must be quoted."""
      .stripMargin

  val scoptParser: OptionParser[CliOptions] =
    new scopt.OptionParser[CliOptions]("scalafmt") {
      override def showUsageOnError: Option[Boolean] = Some(false)

      private def printAndExit(
          includeUsage: Boolean,
      )(ignore: Unit, c: CliOptions): CliOptions = {
        if (includeUsage) displayToOut(usage) else displayToOut(header)
        sys.exit
        c
      }

      private def readConfig(contents: String, c: CliOptions): CliOptions = c
        .copy(configStr = Some(contents))

      head("scalafmt", Versions.nightly)
      opt[Unit]('h', "help").action(printAndExit(includeUsage = true))
        .text("prints this usage text")
      opt[Unit]('v', "version").action(printAndExit(includeUsage = false))
        .text("print version ")

      arg[Path]("<file>...").optional().unbounded().action((file, c) =>
        c.addFile(file),
      ).text(
        """|file, or directory (in which all *.scala files are to be formatted);
           |if starts with '@', refers to path listing files to be formatted
           |(with "@-" referring to standard input as a special case)"""
          .stripMargin,
      )

      opt[Seq[Path]]('f', "files").action((files, c) => c.withFiles(files))
        .hidden() // this option isn't needed anymore. Simply pass the files as
        // arguments. Keeping for backwards compatibility
        .text("file or directory, in which case all *.scala files are formatted. Deprecated: pass files as arguments")

      opt[Unit]('i', "in-place")
        .action((_, c) => writeMode(c, WriteMode.Override)).hidden() // this option isn't needed anymore. Simply don't pass
        // --stdout. Keeping for backwards compatibility
        .text("format files in-place (default)")

      opt[Unit]("stdout").action((_, c) => writeMode(c, WriteMode.Stdout))
        .text("write formatted files to stdout")

      opt[Seq[String]]("exclude").unbounded()
        .action((excludes, c) => c.copy(customExcludes = excludes))
        .text("file or directory, when missing all *.scala files are formatted.")
      opt[Unit]("respect-project-filters")
        .action((_, c) => c.copy(respectProjectFilters = true)).text(
          "use project filters even when specific files to format are provided",
        )
      opt[Path]('c', "config").action((file, c) => c.copy(config = Some(file)))
        .text("a file path to .scalafmt.conf.")
      opt[String]("config-str").action(readConfig)
        .text("configuration defined as a string")
      opt[Unit]("stdin").action((_, c) => c.copy(stdIn = true))
        .text("read from stdin and print to stdout")
      opt[Unit]("no-stderr").action((_, c) => c.copy(noStdErr = true))
        .text("don't use strerr for messages, output to stdout")
      opt[String]("assume-filename")
        .action((filename, c) => c.copy(assumeFilename = filename))
        .text("when using --stdin, use --assume-filename to hint to scalafmt that the input is an .sbt file.")
      opt[Unit]("reportError").action((_, c) => c.copy(error = true))
        .text("exit with status 1 if any mis-formatted code found.")
      opt[Unit]("test").action((_, c) =>
        writeMode(c, WriteMode.Test).copy(error = true),
      ).text("test for mis-formatted code only, exits with status 1 on failure.")
      opt[Unit]("check").action((_, c) =>
        writeMode(c, WriteMode.Test).copy(error = true, check = true),
      ).text("test for mis-formatted code only, exits with status 1 on first failure.")
      opt[FileFetchMode]("mode").action((m, c) => c.copy(mode = Option(m))).text(
        s"""|Sets the files to be formatted fetching mode.
            |Options:
            |${FileFetchMode.help}
            |""".stripMargin,
      )
      opt[Unit]("diff").action((_, c) => c.copy(mode = Option(DiffFiles("master"))))
        .text(
          s"""|Format files listed in `git diff` against master.
              |Deprecated: use --mode diff instead""".stripMargin,
        )
      opt[String]("diff-branch")
        .action((branch, c) => c.copy(mode = Option(DiffFiles(branch)))).text(
          s"""|Format files listed in `git diff` against given git ref.
              |Deprecated: use --mode diff-ref=<ref> instead""".stripMargin,
        )
      opt[Unit]("build-info").action { (_, _) => println(buildInfo); sys.exit }
        .text("prints build information")
      opt[Unit]("quiet").action((_, c) => c.copy(quiet = true))
        .text("don't print out stuff to console.")
      opt[Unit]("debug").action((_, c) => c.copy(debug = true))
        .text("print out diagnostics to console.")
      opt[Unit]("non-interactive").action((_, c) => c.copy(nonInteractive = true))
        .text("disable fancy progress bar, useful in ci or sbt plugin.")
      opt[Unit]("list")
        .action((_, c) => writeMode(c, WriteMode.List).copy(error = true))
        .text("list files that are different from scalafmt formatting")
      opt[(Int, Int)]("range").hidden().action { case ((from, to), c) =>
        val offset = if (from == to) 0 else -1
        c.copy(range = c.range + Range(from - 1, to + offset))
      }.text("(experimental) only format line range from=to")

      note(
        s"""|Examples:
            |$usageExamples
            |Please file bugs to https://github.com/scalameta/scalafmt/issues
            |""".stripMargin,
      )

      checkConfig(c =>
        if (c.config.isDefined && c.configStr.isDefined)
          failure("may not specify both --config and --config-str")
        else success,
      )
    }
  def buildInfo =
    s"""|build commit: ${Versions.commit}
        |build time: ${new Date(Versions.timestamp.toLong)}""".stripMargin

  private def writeMode(c: CliOptions, writeMode: WriteMode): CliOptions = c
    .writeModeOpt.fold(c.copy(writeModeOpt = Some(writeMode))) { x =>
      if (x != writeMode)
        throw new Conflict(s"writeMode changing from $x to $writeMode")
      c
    }

  class Conflict(err: String) extends Exception(err)
}
