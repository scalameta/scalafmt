package org.scalafmt.cli

import java.io.OutputStreamWriter

import org.scalafmt.util.AbsoluteFile

trait ScalafmtRunner {
  private[cli] def run(
      options: CliOptions,
      termDisplayMessage: String
  ): ExitCode

  protected def newTermDisplay(
      options: CliOptions,
      inputMethods: Seq[InputMethod],
      msg: String
  ): TermDisplay = {
    val termDisplay = new TermDisplay(
      new OutputStreamWriter(options.common.info),
      fallbackMode = options.nonInteractive || TermDisplay.defaultFallbackMode
    )
    if (options.writeMode != WriteMode.Stdout && inputMethods.length > 5) {
      termDisplay.init()
      termDisplay.startTask(msg, options.common.workingDirectory.jfile)
      termDisplay.taskLength(msg, inputMethods.length, 0)
    }
    termDisplay
  }

  protected def getInputMethods(
      options: CliOptions,
      filter: AbsoluteFile => Boolean
  ): Seq[InputMethod] = {
    if (options.stdIn) {
      Seq(InputMethod.StdinCode(options.assumeFilename, options.common.in))
    } else {
      val projectFiles: Seq[AbsoluteFile] =
        getFilesFromCliOptions(options, filter)
      options.common.debug
        .print(s"Files to be formatted:\n${projectFiles.mkString("\n")}\n")
      projectFiles.map(InputMethod.FileContents.apply)
    }
  }

  /** Returns file paths defined via options.{customFiles,customExclude} */
  private[this] def getFilesFromCliOptions(
      options: CliOptions,
      canFormat: AbsoluteFile => Boolean
  ): Seq[AbsoluteFile] = {
    def filesWithFetch(
        filesToProcess: Seq[AbsoluteFile],
        fetchFiles: AbsoluteFile => Seq[AbsoluteFile]
    ): Seq[AbsoluteFile] =
      filesToProcess.flatMap {
        case d if d.jfile.isDirectory => fetchFiles(d).filter(canFormat)
        // DESNOTE(2017-05-19, pjrt): A plain, fully passed file will (try to) be
        // formatted regardless of what it is or where it is.
        // NB: Unless respectProjectFilters is also specified.
        case f if options.respectProjectFilters && !canFormat(f) => Seq.empty
        case f => Seq(f)
      }

    val files = options.fileFetchMode match {
      case m @ (GitFiles | RecursiveSearch) =>
        filesWithFetch(
          options.files,
          if (m == GitFiles) options.gitOps.lsTree
          else _.listFiles
        )

      case DiffFiles(branch) =>
        options.customFilesOpt.fold(
          options.gitOps.diff(branch, None).filter(canFormat)
        )(filesWithFetch(_, x => options.gitOps.diff(branch, Some(x))))

      case ChangedFiles =>
        options.gitOps.status.filter(canFormat)
    }
    val excludeRegexp = options.excludeFilterRegexp
    files.filter { f => excludeRegexp.findFirstIn(f.path).isEmpty }
  }
}
