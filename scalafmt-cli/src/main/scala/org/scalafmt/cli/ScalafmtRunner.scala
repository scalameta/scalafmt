package org.scalafmt.cli

import java.io.OutputStreamWriter

import org.scalafmt.util.{AbsoluteFile, FileOps}

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
    val files = options.fileFetchMode match {
      case m @ (GitFiles | RecursiveSearch) =>
        val fetchFiles: AbsoluteFile => Seq[AbsoluteFile] =
          if (m == GitFiles) options.gitOps.lsTree
          else FileOps.listFiles

        options.files.flatMap {
          case d if d.jfile.isDirectory => fetchFiles(d).filter(canFormat)
          // DESNOTE(2017-05-19, pjrt): A plain, fully passed file will (try to) be
          // formatted regardless of what it is or where it is.
          // NB: Unless respectProjectFilters is also specified.
          case f if options.respectProjectFilters && !canFormat(f) => Seq.empty
          case f => Seq(f)
        }

      case DiffFiles(branch) =>
        options.gitOps.diff(branch).filter(canFormat)

      case ChangedFiles =>
        options.gitOps.status.filter(canFormat)
    }
    val excludeRegexp = options.excludeFilterRegexp
    files.filter { f => excludeRegexp.findFirstIn(f.path).isEmpty }
  }
}
