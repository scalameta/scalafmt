package org.scalafmt.cli

import com.martiansoftware.nailgun.NGContext
import java.io.{InputStream, OutputStreamWriter, PrintStream}
import java.nio.file.Paths
import java.util.concurrent.atomic.AtomicInteger

import org.scalafmt.Error.MisformattedFile
import org.scalafmt.Error.NoMatchingFiles
import org.scalafmt.interfaces.Scalafmt
import org.scalafmt.util.AbsoluteFile
import org.scalafmt.util.FileOps

import scala.meta.internal.tokenizers.PlatformTokenizerCache
import scala.util.control.NoStackTrace

object Cli {
  def nailMain(nGContext: NGContext): Unit = {
    val workingDirectory =
      AbsoluteFile.fromPath(nGContext.getWorkingDirectory).getOrElse {
        throw new IllegalStateException(s"Expected absolute path, " +
          s"obtained nGContext.getWorkingDirectory = ${nGContext.getWorkingDirectory}")
      }
    val exit = mainWithOptions(
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
    nGContext.exit(exit.code)
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

  private def throwIfError(exit: ExitCode): Unit = {
    if (exit != ExitCode.Ok) {
      throw new RuntimeException(exit.toString) with NoStackTrace
    }
  }

  def main(args: Array[String]): Unit = {
    val exit = mainWithOptions(args, CliOptions())
    sys.exit(exit.code)
  }

  def exceptionThrowingMain(args: Array[String]): Unit = {
    val exit = mainWithOptions(args, CliOptions.default)
    throwIfError(exit)
  }

  def mainWithOptions(args: Array[String], options: CliOptions): ExitCode = {
    getConfig(args, options) match {
      case Some(x) => run(x)
      case None => ExitCode.CommandLineArgumentError
    }
  }

  def getConfig(args: Array[String], init: CliOptions): Option[CliOptions] = {
    CliArgParser.scoptParser.parse(args, init).map(CliOptions.auto(args, init))
  }

  /** Returns file paths defined via options.{customFiles,customExclude} */
  private def getFilesFromCliOptions(options: CliOptions): Seq[AbsoluteFile] = {
    val files = options.fileFetchMode match {
      case m @ (GitFiles | RecursiveSearch) =>
        val fetchFiles: AbsoluteFile => Seq[AbsoluteFile] =
          if (m == GitFiles) options.gitOps.lsTree(_)
          else FileOps.listFiles(_)

        options.files.flatMap {
          case d if d.jfile.isDirectory => fetchFiles(d)
          case f => Seq(f)
        }
      case DiffFiles(branch) =>
        options.gitOps.diff(branch)
    }
    val excludeRegexp = options.excludeFilterRegexp
    files.filter { f =>
      excludeRegexp.findFirstIn(f.path).isEmpty
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
      scalafmtInstance: Scalafmt,
      options: CliOptions): Unit = {
    val input = inputMethod.readInput(options)

    // DESNOTE(2017-05-19, pjrt): A plain, fully passed file will (try to) be
    // formatted regardless of what it is or where it is.
    val shouldRespectFilters =
      AbsoluteFile.fromPath(inputMethod.filename).forall { file =>
        !options.customFiles.contains(file)
      }
    val formatResult = scalafmtInstance
      .withRespectProjectFilters(shouldRespectFilters)
      .format(
        options.configPath,
        Paths.get(inputMethod.filename),
        input
      )
    inputMethod.write(formatResult, input, options)
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

  private[cli] def run(options: CliOptions): ExitCode = {

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
    }

    val reporter = new ScalafmtCliReporter(options)
    val scalafmtInstance = Scalafmt
      .create(this.getClass.getClassLoader)
      .withReporter(reporter)

    // DESNOTE(2017-05-19, pjrt): A plain, fully passed file will (try to) be
    // formatted regardless of what it is or where it is.
    val fqpns = inputMethods.filter { input =>
      AbsoluteFile.fromPath(input.filename).forall { file =>
        options.customFiles.contains(file)
      }
    }
    val scalafmtInstanceIgnoreFilters =
      if (fqpns.isEmpty) scalafmtInstance
      else
        Scalafmt
          .create(this.getClass.getClassLoader)
          .withReporter(reporter)
          .withRespectProjectFilters(false)

    val termDisplay = newTermDisplay(options, inputMethods, termDisplayMessage)

    inputMethods.foreach { inputMethod =>
      val instance =
        if (fqpns.contains(inputMethod)) scalafmtInstanceIgnoreFilters
        else scalafmtInstance
      try handleFile(inputMethod, instance, options)
      catch {
        case e: MisformattedFile => reporter.error(e.file.toPath, e)
      }
      PlatformTokenizerCache.megaCache.clear()
      termDisplay.taskProgress(termDisplayMessage, counter.incrementAndGet())
    }

    val exit = reporter.getExitCode
    termDisplay.completedTask(termDisplayMessage, exit.isOk)
    termDisplay.stop()
    if (options.testing) {
      if (exit.isOk) {
        options.common.out.println("All files are formatted with scalafmt :)")
      } else if (exit.is(ExitCode.TestError)) {
        options.common.out.println(
          "error: --test failed"
        )
        options.onTestFailure.foreach(options.common.out.println)
      } else {
        options.common.out.println(s"error: $exit")
      }
    }
    if (options.testing &&
      !options.fatalWarnings &&
      !exit.is(ExitCode.TestError)) {
      // Ignore parse errors etc.
      ExitCode.Ok
    } else {
      exit
    }
  }
}
