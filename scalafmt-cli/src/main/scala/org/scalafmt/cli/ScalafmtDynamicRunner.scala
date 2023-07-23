package org.scalafmt.cli

import java.nio.file.Path
import java.util.concurrent.atomic.AtomicReference

import org.scalafmt.CompatCollections.ParConverters._
import org.scalafmt.Error
import org.scalafmt.dynamic.ScalafmtDynamicError
import org.scalafmt.interfaces.Scalafmt
import org.scalafmt.interfaces.ScalafmtSession
import org.scalafmt.sysops.FileOps

import util.control.Breaks._

object ScalafmtDynamicRunner extends ScalafmtRunner {
  override private[cli] def run(
      options: CliOptions,
      termDisplayMessage: String
  ): ExitCode = {
    val reporter = new ScalafmtCliReporter(options)
    val scalafmtInstance = Scalafmt
      .create(this.getClass.getClassLoader)
      .withReporter(reporter)
      .withRespectProjectFilters(false)

    val session =
      try {
        scalafmtInstance.createSession(options.configPath)
      } catch {
        case _: ScalafmtDynamicError.ConfigError =>
          return reporter.getExitCode // XXX: returning
      }

    val sessionMatcher = session.matchesProjectFilters _
    val filterMatcher: Path => Boolean =
      options.customFilesOpt.fold(sessionMatcher) { customFiles =>
        val customMatcher = FileOps.getFileMatcher(customFiles.map(_.path))
        x => customMatcher(x) && sessionMatcher(x)
      }
    val inputMethods = getInputMethods(options, filterMatcher)

    val termDisplay = newTermDisplay(options, inputMethods, termDisplayMessage)

    val exitCode = new AtomicReference(ExitCode.Ok)
    breakable {
      inputMethods.par.foreach { inputMethod =>
        try {
          val code = handleFile(inputMethod, session, options)
          exitCode.getAndUpdate(ExitCode.merge(code, _))
        } catch {
          case e: Error.MisformattedFile =>
            reporter.error(e.file, e)
            if (options.check) break
        }
        termDisplay.taskProgress(termDisplayMessage)
      }
    }

    val exit = ExitCode.merge(exitCode.get, reporter.getExitCode)

    termDisplay.completedTask(termDisplayMessage, exit.isOk)
    termDisplay.stop()

    exit
  }

  private[this] def handleFile(
      inputMethod: InputMethod,
      session: ScalafmtSession,
      options: CliOptions
  ): ExitCode = {
    val input = inputMethod.readInput(options)

    val formatResult = session.format(inputMethod.path, input)
    inputMethod.write(formatResult, input, options)
  }

}
