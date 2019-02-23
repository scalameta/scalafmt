package org.scalafmt.cli
import java.io.OutputStreamWriter

import org.scalafmt.config.FilterMatcher
import org.scalafmt.util.{AbsoluteFile, FileOps}

trait ScalafmtRunner {
  private[cli] def run(
      options: CliOptions,
      termDisplayMessage: String
  ): ExitCode

  protected def newTermDisplay(
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

  protected def getInputMethods(
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

  /** Returns file paths defined via options.{customFiles,customExclude} */
  private[this] def getFilesFromCliOptions(
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
}
