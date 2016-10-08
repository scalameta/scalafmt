package org.scalafmt.cli

import scala.meta.Dialect
import scala.meta.dialects.Sbt0137
import scala.util.matching.Regex

import java.io.File
import java.io.OutputStreamWriter
import java.util.concurrent.atomic.AtomicInteger

import org.scalafmt.Error.UnableToParseCliOptions
import org.scalafmt.Formatted
import org.scalafmt.Scalafmt
import org.scalafmt.config.Config
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.FileOps
import org.scalafmt.util.LogLevel

object Cli {
  def run(options: CliOptions): Unit = {
    if (options.migrate.nonEmpty) runMigrate(options)
    else runFormat(options)
  }

  def getConfig(args: Array[String]): Option[CliOptions] = {
    val init = CliOptions.auto(CliOptions.default)
    CliArgParser.scoptParser.parse(args, init)
  }

  def main(args: Array[String]): Unit = {
    getConfig(args) match {
      case Some(x) => run(x)
      case None => throw UnableToParseCliOptions
    }
  }

  private def mkRegexp(filters: Seq[String]): Regex =
    filters match {
      case Nil => "$a".r // will never match anything
      case head :: Nil => head.r
      case _ => filters.mkString("(", "|", ")").r
    }

  private def getFilesFromProject(options: CliOptions): Seq[String] = {
    import options.config.project._
    val include = mkRegexp(includeFilters)
    val exclude = mkRegexp(excludeFilters)

    def matches(path: String): Boolean =
      include.findFirstIn(path).isDefined &&
        exclude.findFirstIn(path).isEmpty

    val gitFiles: Seq[String] = if (git) options.gitOps.lsTree else Nil
    val otherFiles: Seq[String] =
      files.flatMap(x => FileOps.listFiles(x))
    val x = gitFiles.map(x => matches(x) -> x)
    (otherFiles ++ gitFiles).filter(matches)
  }

  private def getInputMethods(options: CliOptions): Seq[InputMethod] = {
    if (options.stdIn) {
      Seq(InputMethod.StdinCode(options.assumeFilename, options.common.in))
    } else {
      val projectFiles = getFilesFromProject(options)
      val toFormat =
        if (projectFiles.isEmpty) {
          FileOps.listFiles(options.common.workingDirectory)
        } else projectFiles
      toFormat
        .withFilter(x => x.endsWith(".scala") || x.endsWith(".sbt"))
        .map(InputMethod.FileContents.apply)
    }
  }

  private def handleFile(inputMethod: InputMethod, options: CliOptions): Unit = {
    val input = inputMethod.readInput
    val formatResult =
      Scalafmt.format(input, options.config, options.range)
    formatResult match {
      case Formatted.Success(formatted) =>
        inputMethod.write(formatted, input, options)
      case Formatted.Failure(e) =>
        if (options.config.runner.fatalWarnings) {
          throw e
        } else {
          options.common.err.println(
            s"${LogLevel.warn} Error in ${inputMethod.filename}: $e"
          )
        }
    }
  }

  private def runMigrate(options: CliOptions): Unit = {
    options.migrate.foreach { oldStyleConfigFile =>
      val original = FileOps.readFile(oldStyleConfigFile)
      val modified = LegacyCli.migrate(original)
      val newFile = new File(oldStyleConfigFile.getAbsolutePath + ".conf")
      FileOps.writeFile(newFile.getAbsolutePath, modified)
      println("Wrote migrated config to file: " + newFile.getPath)
      println(
        "NOTE. This automatic migration is a best-effort, " +
          "please file an issue if it does not work as advertised.")
      println("-------------------------")
      println(modified)
    }
  }

  private def termDisplayMessage = "Running scalafmt..."

  private def newTermDisplay(options: CliOptions,
                             inputMethods: Seq[InputMethod]): TermDisplay = {
    val workingDirectory = new File(options.common.workingDirectory)
    val termDisplay = new TermDisplay(new OutputStreamWriter(System.out))
    if (!options.stdIn && inputMethods.length > 5) termDisplay.init()
    termDisplay.startTask(termDisplayMessage, workingDirectory)
    termDisplay.taskLength(termDisplayMessage, inputMethods.length, 0)
    termDisplay
  }

  private def runFormat(options: CliOptions): Unit = {
    val inputMethods = getInputMethods(options)
    val counter = new AtomicInteger()
    val sbtConfig = options.copy(
      config = options.config.copy(
        runner = options.config.runner.copy(
          dialect = Sbt0137
        )))
    val termDisplay = newTermDisplay(options, inputMethods)
    inputMethods.par.foreach { inputMethod =>
      val inputConfig = if (inputMethod.isSbt) sbtConfig else options
      handleFile(inputMethod, inputConfig)
      termDisplay.taskProgress(termDisplayMessage, counter.incrementAndGet())
    }
    termDisplay.stop()
  }

}
