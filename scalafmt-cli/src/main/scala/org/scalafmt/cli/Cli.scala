package org.scalafmt.cli

import com.martiansoftware.nailgun.NGContext
import java.io.{InputStream, OutputStreamWriter, PrintStream}
import java.nio.file.Paths
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.function.UnaryOperator

import metaconfig.Configured
import org.scalafmt.Error.MisformattedFile
import org.scalafmt.Error.NoMatchingFiles
import org.scalafmt.config.{FilterMatcher, ScalafmtConfig}
import org.scalafmt.{Formatted, Scalafmt, Versions}
import org.scalafmt.interfaces.{Scalafmt => ScalafmtInterface}
import org.scalafmt.util.{AbsoluteFile, FileOps, OsSpecific}

import scala.meta.internal.tokenizers.PlatformTokenizerCache
import scala.meta.parsers.ParseException
import scala.meta.tokenizers.TokenizeException
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
  private def getFilesFromCliOptions(
      options: CliOptions,
      filter: Option[FilterMatcher]): Seq[AbsoluteFile] = {
    def canFormat(f: AbsoluteFile): Boolean =
      filter.map(_.matches(f)).getOrElse(true)
    val files = options.fileFetchMode match {
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
    val excludeRegexp = options.excludeFilterRegexp
    files.filter { f =>
      excludeRegexp.findFirstIn(f.path).isEmpty
    }
  }

  private def getInputMethods(
      options: CliOptions,
      filter: Option[FilterMatcher]): Seq[InputMethod] = {
    if (options.stdIn) {
      Seq(InputMethod.StdinCode(options.assumeFilename, options.common.in))
    } else {
      val projectFiles: Seq[AbsoluteFile] =
        getFilesFromCliOptions(options, filter)
      projectFiles.map(InputMethod.FileContents.apply)
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

  private[cli] def run(options: CliOptions): ExitCode = {
    val termDisplayMessage =
      if (options.testing) "Looking for unformatted files..."
      else "Reformatting..."
    if (options.debug) {
      val pwd = options.common.workingDirectory.jfile.getPath
      val out = options.info
      out.println("Working directory: " + pwd)
    }

    // Run format using
    // - `scalafmt-dynamic` if the specified `version` setting doesn't match build version.
    // - `scalafmt-core` if the specified `version` setting match with build version
    //   (or if the `version` is not specified).
    val exit = options.version match {
      case None => runScalafmt(options, termDisplayMessage)
      case Some(v) if v == Versions.version =>
        runScalafmt(options, termDisplayMessage)
      case _ => runDynamic(options, termDisplayMessage)
    }

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

  private def runDynamic(
      options: CliOptions,
      termDisplayMessage: String
  ): ExitCode = {
    def handleFile(
        inputMethod: InputMethod,
        scalafmtInstance: ScalafmtInterface,
        options: CliOptions
    ): Unit = {
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

    val inputMethods = getInputMethods(options, None)
    if (inputMethods.isEmpty && options.diff.isEmpty && !options.stdIn)
      throw NoMatchingFiles

    val counter = new AtomicInteger()
    val reporter = new ScalafmtCliReporter(options)
    val scalafmtInstance = ScalafmtInterface
      .create(this.getClass.getClassLoader)
      .withReporter(reporter)

    // Path names fully qualified by `customFiles`.
    // If there exists fqpns, create another instance of `scalafmt-dynamic`
    // that ignores `excludeFilters` because fully qualified files will (try to) be
    // formatted regardless of what it is or where it is (and excludeFilter).
    val fqpns = inputMethods.filter { input =>
      AbsoluteFile.fromPath(input.filename).forall { file =>
        options.customFiles.contains(file)
      }
    }
    val scalafmtInstanceIgnoreFilters =
      if (fqpns.isEmpty) scalafmtInstance
      else
        ScalafmtInterface
          .create(this.getClass.getClassLoader)
          .withReporter(reporter)
          .withRespectProjectFilters(false)

    val termDisplay = newTermDisplay(options, inputMethods, termDisplayMessage)

    inputMethods.foreach { inputMethod =>
      val instance =
        // Use scalafmt-dynamic that ignores exclude filters for fully qualified paths
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

    exit
  }

  private def runScalafmt(
      options: CliOptions,
      termDisplayMessage: String
  ): ExitCode = {
    def handleFile(
        inputMethod: InputMethod,
        options: CliOptions,
        config: ScalafmtConfig): ExitCode = {
      try unsafeHandleFile(inputMethod, options, config)
      catch {
        case MisformattedFile(_, diff) =>
          options.common.err.println(diff)
          ExitCode.TestError
      }
    }
    def unsafeHandleFile(
        inputMethod: InputMethod,
        options: CliOptions,
        config: ScalafmtConfig
    ): ExitCode = {
      val input = inputMethod.readInput(options)
      val scalafmtConfig =
        if (inputMethod.isSbt || inputMethod.isSc) config.forSbt
        else config
      val formatResult = Scalafmt.format(
        input,
        scalafmtConfig,
        options.range,
        inputMethod.filename
      )
      formatResult match {
        case Formatted.Success(formatted) =>
          inputMethod.write(formatted, input, options)
          ExitCode.Ok
        case Formatted.Failure(e) =>
          if (scalafmtConfig.runner.ignoreWarnings) {
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

    options.scalafmtConfig match {
      case Configured.NotOk(e) =>
        if (!options.quiet) options.common.err.println(s"${e.msg}")
        ExitCode.UnexpectedError
      case Configured.Ok(scalafmtConf) =>
        val filterMatcher: FilterMatcher = FilterMatcher(
          scalafmtConf.project.includeFilters
            .map(OsSpecific.fixSeparatorsInPathPattern),
          (scalafmtConf.project.excludeFilters ++ options.customExcludes)
            .map(OsSpecific.fixSeparatorsInPathPattern)
        )

        val inputMethods = getInputMethods(options, Some(filterMatcher))
        if (inputMethods.isEmpty && options.diff.isEmpty && !options.stdIn)
          throw NoMatchingFiles

        val counter = new AtomicInteger()
        val termDisplay =
          newTermDisplay(options, inputMethods, termDisplayMessage)
        val exitCode = new AtomicReference(ExitCode.Ok)
        inputMethods.par.foreach { inputMethod =>
          val code = handleFile(inputMethod, options, scalafmtConf)
          exitCode.getAndUpdate(new UnaryOperator[ExitCode] {
            override def apply(t: ExitCode): ExitCode =
              ExitCode.merge(code, t)
          })
          PlatformTokenizerCache.megaCache.clear()
          termDisplay.taskProgress(
            termDisplayMessage,
            counter.incrementAndGet())
        }
        termDisplay.completedTask(termDisplayMessage, exitCode.get.isOk)
        termDisplay.stop()
        exitCode.get()
    }
  }
}
