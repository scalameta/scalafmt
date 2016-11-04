package org.scalafmt.cli

import scala.meta.Dialect
import scala.meta.dialects.Sbt0137
import scala.util.matching.Regex

import java.io.File
import java.io.InputStream
import java.io.OutputStream
import java.io.OutputStreamWriter
import java.io.PrintStream
import java.util.concurrent.atomic.AtomicInteger

import org.scalafmt.Error.UnableToParseCliOptions
import org.scalafmt.Formatted
import org.scalafmt.Scalafmt
import org.scalafmt.config.Config
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.FileOps
import org.scalafmt.util.LogLevel
import com.martiansoftware.nailgun.NGContext
import org.scalafmt.util.AbsoluteFile

object Cli {
  def nailMain(nGContext: NGContext): Unit = {
    val workingDirectory =
      AbsoluteFile.fromPath(nGContext.getWorkingDirectory).getOrElse {
        throw new IllegalStateException(s"Expected absolute path, " +
          s"obtained nGContext.getWorkingDirectory = ${nGContext.getWorkingDirectory}")
      }
    mainWithOptions(
      nGContext.getArgs,
      CliOptions.default.copy(
        common = CliOptions.default.common.copy(
          workingDirectory = workingDirectory,
          out = nGContext.out,
          in = nGContext.in,
          err = nGContext.err
        )
      )
    )
  }

  def main(args: Array[String],
           in: InputStream,
           out: PrintStream,
           err: PrintStream,
           workingDirectory: String): Unit = {
    val options = CliOptions.default.copy(
      common = CommonOptions(
        in = in,
        out = out,
        err = err,
        workingDirectory = AbsoluteFile.fromPath(workingDirectory).get
      )
    )
    mainWithOptions(args, options)
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

  private def mkRegexp(filters: Seq[String]): Regex =
    filters match {
      case Nil => "$a".r // will never match anything
      case head :: Nil => head.r
      case _ => filters.mkString("(", "|", ")").r
    }

  def canFormat(path: AbsoluteFile): Boolean =
    canFormat(path.path)

  def canFormat(path: String): Boolean =
    path.endsWith(".scala") || path.endsWith(".sbt")

  def expandCustomFiles(workingDir: AbsoluteFile,
                        files: Seq[AbsoluteFile]): Seq[AbsoluteFile] =
    files.flatMap {
      case f if f.jfile.isDirectory =>
        FileOps.listFiles(f).filter(canFormat)
      // we don't filter out custom files, even if they don't exist or contain
      // weird suffixes.
      case f => Seq(f)
    }

  /** Returns file paths defined via options.{customFiles,customExclude} */
  def getFilesFromCliOptions(options: CliOptions): Seq[AbsoluteFile] = {
    val exclude = mkRegexp(options.customExcludes)
    expandCustomFiles(options.common.workingDirectory, options.customFiles)
      .filter(x => exclude.findFirstIn(x.path).isEmpty)
  }

  /** Returns file paths defined via options.project */
  private def getFilesFromProject(options: CliOptions): Seq[AbsoluteFile] = {
    val project = options.config.project
    val include = mkRegexp(project.includeFilters)
    val exclude = mkRegexp(project.excludeFilters)

    def matches(file: AbsoluteFile): Boolean = {
      val path = file.path
      include.findFirstIn(path).isDefined &&
      exclude.findFirstIn(path).isEmpty
    }
    val projectFiles: Seq[AbsoluteFile] = (
      if (project.git) {
        options.gitOps.lsTree
      } else {
        FileOps.listFiles(options.common.workingDirectory)
      }
    ).filter(canFormat)
    val customFiles: Seq[AbsoluteFile] =
      expandCustomFiles(
        options.common.workingDirectory,
        AbsoluteFile.fromFiles(project.files.map(new File(_)),
                               options.common.workingDirectory))
    (customFiles ++ projectFiles).filter(matches)
  }

  private def getInputMethods(options: CliOptions): Seq[InputMethod] = {
    if (options.stdIn) {
      Seq(InputMethod.StdinCode(options.assumeFilename, options.common.in))
    } else {
      val projectFiles: Seq[AbsoluteFile] =
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
      val newFile = new File(oldStyleConfigFile.path + ".conf")
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
      termDisplay.startTask(msg, options.common.workingDirectory.jfile)
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
