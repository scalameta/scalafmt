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
import org.scalafmt.config.ProjectFiles
import org.scalafmt.util.FileOps
import org.scalafmt.util.GitOps
import org.scalafmt.util.LogLevel
import org.scalafmt.util.logger

object Cli {
  case class DebugError(filename: String, error: Throwable)

  def mkRegexp(filters: Seq[String]): Regex =
    filters match {
      case Nil => "$a".r // will never match anything
      case head :: Nil => head.r
      case _ => filters.mkString("(", "|", ")").r
    }

  def getFilesFromProject(projectFiles: ProjectFiles): Seq[String] = {
    val include = mkRegexp(projectFiles.includeFilter)
    val exclude = mkRegexp(projectFiles.excludeFilter)

    def matches(path: String): Boolean =
      include.findFirstIn(path).isDefined &&
        exclude.findFirstIn(path).isEmpty

    val gitFiles: Seq[String] = if (projectFiles.git) GitOps.lsTree else Nil
    val otherFiles: Seq[String] =
      projectFiles.files.flatMap(x => FileOps.listFiles(x))
    val res = (otherFiles ++ gitFiles).filter(matches)
//    logger.elem(projectFiles.includeFilter, projectFiles.files, gitFiles, res)
    res
  }

  def getInputMethods(options: CliOptions): Seq[InputMethod] = {
    if (options.stdIn) {
      Seq(InputMethod.StdinCode(options.assumeFilename, options.common.in))
    } else {
      getFilesFromProject(options.config.project)
        .withFilter(
          x => x.endsWith(".scala") || x.endsWith(".sbt")
        )
        .map(InputMethod.FileContents.apply)
    }
  }

  def handleFile(inputMethod: InputMethod, options: CliOptions): Unit = {
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

  def runMigrate(options: CliOptions): Unit = {
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

  val termDisplayMessage = "Running scalafmt..."

  def newTermDisplay(options: CliOptions,
                     inputMethods: Seq[InputMethod]): TermDisplay = {
    val workingDirectory = new File(options.common.workingDirectory)
    val termDisplay = new TermDisplay(new OutputStreamWriter(System.out))
    if (options.inPlace) termDisplay.init()
    termDisplay.startTask(termDisplayMessage, workingDirectory)
    termDisplay.taskLength(termDisplayMessage, inputMethods.length, 0)
    termDisplay
  }

  def runFormat(options: CliOptions): Unit = {
    val inputMethods = getInputMethods(options)
    logger.elem(inputMethods)
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

  def run(options: CliOptions): Unit = {
    if (options.migrate.nonEmpty) runMigrate(options)
    else runFormat(options)
  }

  def getConfig(args: Array[String]): Option[CliOptions] = {
    CliArgParser.scoptParser.parse(args, CliOptions.default)
  }

  def runOn(options: CliOptions): Unit = {}

  def main(args: Array[String]): Unit = {
    getConfig(args) match {
      case Some(x) => run(x)
      case None => throw UnableToParseCliOptions
    }
  }
}
