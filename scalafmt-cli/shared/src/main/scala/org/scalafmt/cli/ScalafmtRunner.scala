package org.scalafmt.cli

import org.scalafmt.Error
import org.scalafmt.sysops._

import java.nio.file.Path

import scala.concurrent._
import scala.util.{Failure, Success, Try}

trait ScalafmtRunner {
  private[cli] def run(
      options: CliOptions,
      termDisplayMessage: String,
  ): Future[ExitCode]

  private def newTermDisplay(
      options: CliOptions,
      inputMethods: Seq[InputMethod],
      msg: String,
  ): Option[TermDisplay] = {
    val numInputs = inputMethods.length
    if (options.writeMode != WriteMode.Stdout && numInputs > 5) {
      val termDisplay = new TermDisplay(
        options.common.info.printWriter,
        msg,
        numInputs,
        fallbackMode = options.nonInteractive || TermDisplay.defaultFallbackMode,
      )
      termDisplay.start()
      Some(termDisplay)
    } else None
  }

  protected def getInputMethods(
      options: CliOptions,
      filter: Path => Boolean,
      skipDir: Path => Boolean = _ => false,
  ): Seq[InputMethod] =
    if (options.stdIn)
      Seq(InputMethod.StdinCode(options.assumeFilename, options.common.in))
    else {
      val projectFiles: Seq[AbsoluteFile] =
        getFilesFromCliOptions(options, filter, skipDir)
      if (projectFiles.isEmpty && options.mode.isEmpty)
        throw Error.NoMatchingFiles
      options.common.debug
        .print(s"Files to be formatted:\n${projectFiles.mkString("\n")}\n")
      projectFiles.map(InputMethod.FileContents.apply)
    }

  /** Returns file paths defined via options.{customFiles,customExclude} */
  private[this] def getFilesFromCliOptions(
      options: CliOptions,
      canFormat: Path => Boolean,
      skipDir: Path => Boolean,
  ): Seq[AbsoluteFile] = {
    val gitOps = options.gitOps
    val finder = options.fileFetchMode match {
      case GitFiles => new BatchPathFinder.GitFiles(gitOps)(canFormat)
      case RecursiveSearch =>
        new BatchPathFinder.DirFiles(options.cwd)(canFormat, skipDir)
      case DiffFiles(branch) =>
        new BatchPathFinder.GitBranchFiles(gitOps, branch)(canFormat)
      case DiffBase(refOpt) => diffBaseRef(options, gitOps, refOpt) match {
          case Some(base) =>
            new BatchPathFinder.GitBranchFiles(gitOps, base)(canFormat)
          case None => // unresolvable base or config changed: format all tracked
            new BatchPathFinder.GitFiles(gitOps)(canFormat)
        }
      case ChangedFiles => new BatchPathFinder.GitDirtyFiles(gitOps)(canFormat)
    }
    val files = finder.findMatchingFiles(
      options.respectProjectFilters,
      options.customFilesOpt.getOrElse(Seq.empty): _*,
    )
    val excludeRegexp = options.excludeFilterRegexp.pattern
    files.filter(f => !excludeRegexp.matcher(f.toString()).find())
  }

  /** The fork-point to diff against for [[DiffBase]], or None to signal "format
    * all tracked files" — when the base can't be resolved, or the scalafmt
    * config changed since the fork (a config change can reformat anything).
    */
  private[this] def diffBaseRef(
      options: CliOptions,
      gitOps: GitOps,
      refOpt: Option[String],
  ): Option[String] = refOpt.orElse(gitOps.upstreamBranch)
    .flatMap(gitOps.mergeBase).filter { base =>
      val configFile = options.canonicalConfigFile.flatMap(_.toOption)
      val configChanged = configFile
        .exists(p => gitOps.changedSince(base, AbsoluteFile(p)))
      if (configChanged) options.common.debug
        .println("scalafmt config changed since fork; formatting all files")
      !configChanged
    }

  protected def runInputs(
      options: CliOptions,
      inputMethods: Seq[InputMethod],
      termDisplayMessage: String,
  )(f: (String, Path) => Either[ExitCode, String]): Future[ExitCode] = {
    val termDisplay = newTermDisplay(options, inputMethods, termDisplayMessage)

    implicit val ec = PlatformRunOps.parasiticExecutionContext
    val (formatContext, writeContext) =
      if (options.asyncFormat) (
        PlatformRunOps.formatExecutionContext,
        PlatformRunOps.outputExecutionContext,
      )
      else (ec, ec)

    implicit class FutureExt[A](private val obj: Future[A]) extends AnyRef {
      def td(f: (TermDisplay, Try[A]) => Unit): Future[A] = termDisplay
        .fold(obj)(td => obj.transform { r => f(td, r); r })
    }
    def asExit(r: Try[ExitCode]): ExitCode = r match {
      case Failure(ex) =>
        ex.printStackTrace(options.common.err)
        ExitCode.UnexpectedError
      case Success(x) => x
    }

    val completed = Promise[ExitCode]()

    val tasks = List.newBuilder[Future[Try[ExitCode]]]
    inputMethods.foreach { inputMethod =>
      if (!completed.isCompleted) {
        val read = inputMethod.readInput(options)
        val format = read.td { case (td, r) => td.doneRead(ok = r.isSuccess) }
          .map(code => f(code, inputMethod.path).map((code, _)))(formatContext)
          .td { case (td, r) => td.doneFormat(ok = r.toOption.exists(_.isRight)) }
        val future = format.flatMap {
          case Left(exitCode) => exitCode.future
          case Right((code, formattedCode)) => inputMethod
              .write(formattedCode, code, options)
        }(writeContext).transform { r =>
          val ok = r == Success(ExitCode.Ok)
          termDisplay.foreach(_.doneWrite(ok = ok))
          if (!ok && options.check) completed.trySuccess(asExit(r))
          Success(r)
        }
        tasks += future
      }
    }

    Future.foldLeft(tasks.result())(ExitCode.Ok) { case (res, r) =>
      ExitCode.merge(res, asExit(r))
    }.onComplete(completed.tryComplete)

    completed.future.td { case (td, _) => td.end() }
  }

}
