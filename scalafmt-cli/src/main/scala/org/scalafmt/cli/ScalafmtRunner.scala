package org.scalafmt.cli

import java.io.OutputStreamWriter
import java.nio.file.Path

import org.scalafmt.Error
import org.scalafmt.sysops.AbsoluteFile
import org.scalafmt.sysops.BatchPathFinder

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
    if (
      options.writeMode != WriteMode.Stdout && inputMethods.lengthCompare(5) > 0
    ) {
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
      if (projectFiles.isEmpty && options.mode.isEmpty)
        throw Error.NoMatchingFiles
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
    val finder = options.fileFetchMode match {
      case GitFiles =>
        new BatchPathFinder.GitFiles(gitOps)(canFormat)
      case RecursiveSearch =>
        new BatchPathFinder.DirFiles(options.cwd)(canFormat)
      case DiffFiles(branch) =>
        new BatchPathFinder.GitBranchFiles(gitOps, branch)(canFormat)
      case ChangedFiles =>
        new BatchPathFinder.GitDirtyFiles(gitOps)(canFormat)
    }
    val files = finder.findMatchingFiles(
      options.respectProjectFilters,
      options.customFilesOpt.getOrElse(Seq.empty): _*
    )
    val excludeRegexp = options.excludeFilterRegexp.pattern
    files.filter { f => !excludeRegexp.matcher(f.toString()).find() }
  }
}
