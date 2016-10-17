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
import com.martiansoftware.nailgun.NGContext
import org.scalafmt.util.RegexOps

object Cli {
  def nailMain(nGContext: NGContext): Unit = {
    mainWithOptions(
      nGContext.getArgs,
      CliOptions.default.copy(
        common = CliOptions.default.common.copy(
          workingDirectory =
            new File(nGContext.getWorkingDirectory).getAbsoluteFile,
          out = nGContext.out,
          in = nGContext.in,
          err = nGContext.err
        )
      )
    )

  }
  def main(args: Array[String]): Unit = {
    mainWithOptions(args, CliOptions.default)
  }

  def mainWithOptions(args: Array[String], options: CliOptions): Unit = {
    getConfig(args, options) match {
      case Some(x) => run(x)
      case None => throw UnableToParseCliOptions
    }
  }

  def run(options: CliOptions): Unit = {
    if (options.migrate.nonEmpty) runMigrate(options)
    else runFormat(options)
  }

  def getConfig(args: Array[String], init: CliOptions): Option[CliOptions] = {
    CliArgParser.scoptParser.parse(args, init).map(CliOptions.auto(init))
  }

  def canFormat(path: String): Boolean =
    path.endsWith(".scala") || path.endsWith(".sbt")

  def expandCustomFiles(workingDir: File, files: Seq[File]): Seq[String] =
    files.map(makeAbsolute(workingDir)).flatMap {
      case f if f.isDirectory =>
        FileOps.listFiles(f).filter(canFormat)
      // we don't filter out custom files, even if they don't exist or contain
      // weird suffixes.
      case f => Seq(f.getAbsolutePath)
    }

  def makeAbsolute(workingDir: File)(file: File): File =
    if (file.isAbsolute) file
    else new File(workingDir, file.getPath)

  /** Returns file paths defined via options.{customFiles,customExclude} */
  def getFilesFromCliOptions(options: CliOptions): Seq[String] = {
    // Ensure all paths are absolute
    val absolute =
      options.customFiles.map(makeAbsolute(options.common.workingDirectory))
    val exclude = RegexOps.mkRegexp(options.customExcludes)
    expandCustomFiles(options.common.workingDirectory, options.customFiles)
      .filter(x => exclude.findFirstIn(x).isEmpty)
  }

  /** Returns file paths defined via options.project */
  private def getFilesFromProject(options: CliOptions): Seq[String] = {
    val project = options.config.project
    val include = RegexOps.mkRegexp(project.includeFilters)
    val exclude = RegexOps.mkRegexp(project.excludeFilters)

    def matches(path: String): Boolean =
      include.findFirstIn(path).isDefined &&
        exclude.findFirstIn(path).isEmpty
    val projectFiles: Seq[String] = (
      if (project.git) {
        options.gitOps.lsTree
      } else {
        FileOps.listFiles(options.common.workingDirectory)
      }
    ).filter(canFormat)
    val customFiles: Seq[String] =
      expandCustomFiles(options.common.workingDirectory,
                        project.files.map(new File(_)))
    (customFiles ++ projectFiles).filter(matches)
  }

  private def getInputMethods(options: CliOptions): Seq[InputMethod] = {
    if (options.stdIn) {
      Seq(InputMethod.StdinCode(options.assumeFilename, options.common.in))
    } else {
      val projectFiles: Seq[String] =
        if (options.customFiles.nonEmpty) getFilesFromCliOptions(options)
        else getFilesFromProject(options)
      projectFiles.map(InputMethod.FileContents.apply)
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

  def newTermDisplay(options: CliOptions,
                     inputMethods: Seq[InputMethod],
                     msg: String): TermDisplay = {
    val termDisplay = new TermDisplay(
      new OutputStreamWriter(options.common.err))
    if ((options.inPlace || options.testing) && inputMethods.length > 5) {
      termDisplay.init()
      termDisplay.startTask(msg, options.common.workingDirectory)
      termDisplay.taskLength(msg, inputMethods.length, 0)
    }
    termDisplay
  }

  private def runFormat(options: CliOptions): Unit = {
    val inputMethods = getInputMethods(options)
    val counter = new AtomicInteger()
    val termDisplayMessage =
      if (options.testing) "Looking for unformatted files..."
      else "Reformatting..."

    val sbtOptions = options.copy(
      config = options.config.copy(
        runner = options.config.runner.copy(
          dialect = Sbt0137
        )))
    val termDisplay = newTermDisplay(options, inputMethods, termDisplayMessage)
    val N = inputMethods.length
    inputMethods.par.foreach { inputMethod =>
      val inputConfig = if (inputMethod.isSbt) sbtOptions else options
      handleFile(inputMethod, inputConfig)
      termDisplay.taskProgress(termDisplayMessage, counter.incrementAndGet())
    }
    termDisplay.stop()
    if (options.testing) {
      options.common.out.println("All files are formatted with scalafmt :)")
    }
  }

}
