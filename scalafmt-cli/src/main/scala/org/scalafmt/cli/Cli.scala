package org.scalafmt.cli

import com.martiansoftware.nailgun.NGContext
import java.io.InputStream
import java.io.OutputStreamWriter
import java.io.PrintStream
import java.util.concurrent.atomic.AtomicInteger
import org.scalafmt.Error.NoMatchingFiles
import org.scalafmt.Error.UnableToParseCliOptions
import org.scalafmt.Formatted
import org.scalafmt.Scalafmt
import org.scalafmt.util.AbsoluteFile
import org.scalafmt.util.FileOps
import org.scalafmt.util.LogLevel

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

  def main(
      args: Array[String],
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

  def getConfig(args: Array[String], init: CliOptions): Option[CliOptions] = {
    CliArgParser.scoptParser.parse(args, init).map(CliOptions.auto(args, init))
  }

  private def canFormat(path: String): Boolean =
    path.endsWith(".scala") || path.endsWith(".sbt") || path.endsWith(".sc")

  /** Returns file paths defined via options.{customFiles,customExclude} */
  private def getFilesFromCliOptions(options: CliOptions): Seq[AbsoluteFile] = {

    def canFormat(f: AbsoluteFile) = options.filterMatcher.matches(f)

    options.fileFetchMode match {
      case m @ (GitFiles | RecursiveSearch) =>
        val fetchFiles: AbsoluteFile => Seq[AbsoluteFile] =
          if (m == GitFiles) options.gitOps.lsTree(_)
          else FileOps.listFiles(_)

        options.files.flatMap {
          case d if d.jfile.isDirectory => fetchFiles(d).filter(canFormat)
          // DESNOTE(2017-05-19, pjrt): A plain, fully passed file will (try to) be
          // formatted regardless of what it is or where it is.
          case f => Seq(f)
        }
      case DiffFiles(branch) =>
        options.gitOps.diff(branch).filter(canFormat)
    }

  }

  private def getInputMethods(options: CliOptions): Seq[InputMethod] = {
    if (options.stdIn) {
      Seq(InputMethod.StdinCode(options.assumeFilename, options.common.in))
    } else {
      val projectFiles: Seq[AbsoluteFile] = getFilesFromCliOptions(options)
      projectFiles.map(InputMethod.FileContents.apply)
    }
  }

  private def handleFile(
      inputMethod: InputMethod,
      options: CliOptions): Unit = {
    val input = inputMethod.readInput(options)
    val formatResult =
      Scalafmt.format(input, options.config, options.range)
    formatResult match {
      case Formatted.Success(formatted) =>
        inputMethod.write(formatted, input, options)
      case Formatted.Failure(e) =>
        if (options.config.runner.fatalWarnings) {
          throw e
        } else if (options.config.runner.ignoreWarnings) {
          // do nothing
        } else {
          options.common.err.println(
            s"${LogLevel.warn} Error in ${inputMethod.filename}: $e"
          )
        }
    }
  }

  def newTermDisplay(
      options: CliOptions,
      inputMethods: Seq[InputMethod],
      msg: String): TermDisplay = {
    val termDisplay = new TermDisplay(
      new OutputStreamWriter(options.info),
      fallbackMode =
        options.nonInteractive ||
          TermDisplay.defaultFallbackMode
    )
    if (!options.quiet &&
      (options.inPlace || options.testing) &&
      inputMethods.length > 5) {
      termDisplay.init()
      termDisplay.startTask(msg, options.common.workingDirectory.jfile)
      termDisplay.taskLength(msg, inputMethods.length, 0)
    }
    termDisplay
  }

  def run(options: CliOptions): Unit = {
    val inputMethods = getInputMethods(options)
    if (inputMethods.isEmpty && options.diff.isEmpty && !options.stdIn)
      throw NoMatchingFiles
    val counter = new AtomicInteger()
    val termDisplayMessage =
      if (options.testing) "Looking for unformatted files..."
      else "Reformatting..."
    if (options.debug) {
      val pwd = options.common.workingDirectory.jfile.getPath
      val out = options.info
      out.println("Working directory: " + pwd)
      out.println("Formatting files: " + inputMethods.toList)
//      out.println(
//        "Configuration: \n" + Config
//          .toHocon(options.config.fields)
//          .mkString("\n"))
    }

    val sbtOptions = options.copy(
      config = options.config.copy(runner = options.config.runner.forSbt))
    val termDisplay = newTermDisplay(options, inputMethods, termDisplayMessage)
    inputMethods.par.foreach { inputMethod =>
      val inputConfig =
        if (inputMethod.isSbt || inputMethod.isSc) sbtOptions else options
      handleFile(inputMethod, inputConfig)
      termDisplay.taskProgress(termDisplayMessage, counter.incrementAndGet())
    }
    termDisplay.stop()
    if (options.testing) {
      options.common.out.println("All files are formatted with scalafmt :)")
    }
  }

}
