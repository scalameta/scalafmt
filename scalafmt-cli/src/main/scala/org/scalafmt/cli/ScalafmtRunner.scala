package org.scalafmt.cli

import java.io.OutputStreamWriter
import java.nio.file.Path

import org.scalafmt.sysops.AbsoluteFile
import org.scalafmt.sysops.GitOps.Implicit

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
      termDisplay.startTask(msg, options.cwd.jfile)
      termDisplay.taskLength(msg, inputMethods.length, 0)
    }
    termDisplay
  }

  protected def getInputMethods(
      options: CliOptions,
      filter: Path => Boolean
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
      canFormat: Path => Boolean
  ): Seq[AbsoluteFile] = {
    val gitOps = options.gitOps
    val files = options.fileFetchMode match {
      case GitFiles =>
        options.customFilesOpt.fold(gitOps.getFiles(canFormat))(
          gitOps.getFiles(_, options.respectProjectFilters, canFormat)
        )

      case RecursiveSearch =>
        options.customFilesOpt.fold(gitOps.getDirFiles(canFormat))(
          gitOps.getDirFiles(_, options.respectProjectFilters, canFormat)
        )

      case DiffFiles(branch) =>
        options.customFilesOpt.fold(gitOps.getDiffFiles(branch, canFormat))(
          gitOps.getDiffFiles(branch, options.respectProjectFilters, canFormat)
        )

      case ChangedFiles =>
        gitOps.getChangedFiles(canFormat)
    }
    val excludeRegexp = options.excludeFilterRegexp
    files.filter { f => excludeRegexp.findFirstIn(f.toString()).isEmpty }
  }
}
