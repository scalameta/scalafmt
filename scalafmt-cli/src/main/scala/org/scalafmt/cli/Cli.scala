package org.scalafmt.cli

import com.martiansoftware.nailgun.NGContext
import java.io.InputStream
import java.io.OutputStreamWriter
import java.io.PrintStream
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicReference
import java.util.function.UnaryOperator
import org.scalafmt.Error.MisformattedFile
import org.scalafmt.Error.NoMatchingFiles
import org.scalafmt.Formatted
import org.scalafmt.Scalafmt
import org.scalafmt.util.AbsoluteFile
import org.scalafmt.util.FileOps
import scala.meta.internal.tokenizers.PlatformTokenizerCache
import scala.meta.parsers.ParseException
import scala.meta.tokenizers.TokenizeException
import scala.util.control.NoStackTrace

object Cli {
  def nailMain(nGContext: NGContext): Unit = {
    val workingDirectory =
      AbsoluteFile.fromPath(nGContext.getWorkingDirectory).getOrElse {
        throw new IllegalStateException(
          s"Expected absolute path, " +
            s"obtained nGContext.getWorkingDirectory = ${nGContext.getWorkingDirectory}"
        )
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
      workingDirectory: String
  ): Unit = {
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
    val exit = mainWithOptions(args, CliOptions.default)
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
      options: CliOptions
  ): ExitCode = {
    try unsafeHandleFile(inputMethod, options)
    catch {
      case MisformattedFile(_, diff) =>
        options.common.err.println(diff)
        ExitCode.TestError
    }
  }

  private def unsafeHandleFile(
      inputMethod: InputMethod,
      options: CliOptions
  ): ExitCode = {
    val input = inputMethod.readInput(options)
    val formatResult = Scalafmt.format(
      input,
      options.config,
      options.range,
      inputMethod.filename
    )
    formatResult match {
      case Formatted.Success(formatted) =>
        inputMethod.write(formatted, input, options)
        ExitCode.Ok
      case Formatted.Failure(e) =>
        if (options.config.runner.ignoreWarnings) {
          ExitCode.Ok // do nothing
        } else {
          e match {
            case e @ (_: ParseException | _: TokenizeException) =>
              options.common.err.println(e.toString)
              ExitCode.ParseError
            case _ =>
              new FailedToFormat(inputMethod.filename, e)
                .printStackTrace(options.common.out)
              ExitCode.UnexpectedError
          }
        }
    }
  }

  def newTermDisplay(
      options: CliOptions,
      inputMethods: Seq[InputMethod],
      msg: String
  ): TermDisplay = {
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

  def run(options: CliOptions): ExitCode = {
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

    val sbtOptions = options.copy(
      config = options.config.copy(runner = options.config.runner.forSbt)
    )
    val termDisplay = newTermDisplay(options, inputMethods, termDisplayMessage)
    val exitCode = new AtomicReference(ExitCode.Ok)
    inputMethods.par.foreach { inputMethod =>
      val inputConfig =
        if (inputMethod.isSbt || inputMethod.isSc) sbtOptions else options
      val code = handleFile(inputMethod, inputConfig)
      exitCode.getAndUpdate(new UnaryOperator[ExitCode] {
        override def apply(t: ExitCode): ExitCode =
          ExitCode.merge(code, t)
      })
      PlatformTokenizerCache.megaCache.clear()
      termDisplay.taskProgress(termDisplayMessage, counter.incrementAndGet())
    }
    termDisplay.completedTask(termDisplayMessage, exitCode.get.isOk)
    termDisplay.stop()
    val exit = exitCode.get()
    if (options.testing) {
      if (exit.isOk) {
        options.common.out.println("All files are formatted with scalafmt :)")
      } else if (exit.is(ExitCode.TestError)) {
        options.common.out.println(
          "error: --test failed"
        )
        if (options.config.onTestFailure.nonEmpty) {
          options.common.out.println(options.config.onTestFailure)
        }
      } else {
        options.common.out.println(s"error: $exit")
      }
    }
    if (options.testing &&
        !options.config.runner.fatalWarnings &&
        !exit.is(ExitCode.TestError)) {
      // Ignore parse errors etc.
      ExitCode.Ok
    } else {
      exit
    }
  }

  private class FailedToFormat(filename: String, cause: Throwable)
      extends Exception(filename, cause)
      with NoStackTrace

}
