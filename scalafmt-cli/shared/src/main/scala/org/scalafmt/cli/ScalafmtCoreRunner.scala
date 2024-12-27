package org.scalafmt.cli

import org.scalafmt.CompatCollections.ParConverters._
import org.scalafmt.Error
import org.scalafmt.Formatted
import org.scalafmt.Scalafmt
import org.scalafmt.Versions
import org.scalafmt.config.ProjectFiles
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.config.ScalafmtConfigException

import scala.meta.parsers.ParseException
import scala.meta.tokenizers.TokenizeException

import java.util.concurrent.atomic.AtomicReference

import scala.annotation.tailrec

import util.control.Breaks

object ScalafmtCoreRunner extends ScalafmtRunner {
  override private[cli] def run(
      options: CliOptions,
      termDisplayMessage: String,
  ): ExitCode = options.scalafmtConfig.fold { e =>
    options.common.err.println(s"${e.msg}")
    ExitCode.UnexpectedError
  } { scalafmtConf =>
    options.common.debug.println(s"parsed config (v${Versions.version})")
    val filterMatcher =
      try ProjectFiles.FileMatcher(scalafmtConf.project, options.customExcludes)
      catch {
        case e: ScalafmtConfigException =>
          options.common.err.println(e.getMessage())
          return ExitCode.UnexpectedError // RETURNING EARLY!
      }

    val inputMethods = getInputMethods(options, filterMatcher.matchesPath)
    val adjustedScalafmtConf = {
      if (scalafmtConf.needGitAutoCRLF) options.gitOps.getAutoCRLF else None
    }.fold(scalafmtConf)(scalafmtConf.withGitAutoCRLF)

    val termDisplay = newTermDisplay(options, inputMethods, termDisplayMessage)
    val exitCode = new AtomicReference(ExitCode.Ok)
    Breaks.breakable(inputMethods.compatPar.foreach { inputMethod =>
      val code = handleFile(inputMethod, options, adjustedScalafmtConf)
      exitCode.getAndUpdate(ExitCode.merge(code, _))
      if (options.check && !code.isOk) Breaks.break
      termDisplay.taskProgress(termDisplayMessage)
    })
    termDisplay.completedTask(termDisplayMessage, exitCode.get.isOk)
    termDisplay.stop()
    exitCode.get()
  }

  private[this] def handleFile(
      inputMethod: InputMethod,
      options: CliOptions,
      config: ScalafmtConfig,
  ): ExitCode =
    try unsafeHandleFile(inputMethod, options, config)
    catch {
      case Error.MisformattedFile(_, diff) =>
        options.common.err.println(diff)
        ExitCode.TestError
    }

  private[this] def unsafeHandleFile(
      inputMethod: InputMethod,
      options: CliOptions,
      scalafmtConfig: ScalafmtConfig,
  ): ExitCode = {
    val input = inputMethod.readInput(options)
    val filename = inputMethod.path.toString
    @tailrec
    def handleError(e: Throwable): ExitCode = e match {
      case Error.WithCode(e, _) => handleError(e)
      case _: ParseException | _: TokenizeException =>
        options.common.err.println(e.toString)
        ExitCode.ParseError
      case e =>
        new FailedToFormat(filename, e).printStackTrace(options.common.err)
        ExitCode.UnexpectedError
    }
    Scalafmt.formatCode(input, scalafmtConfig, options.range, filename)
      .formatted match {
      case Formatted.Success(x) => inputMethod.write(x, input, options)
      case x: Formatted.Failure =>
        if (scalafmtConfig.runner.ignoreWarnings) ExitCode.Ok // do nothing
        else handleError(x.e)
    }
  }
}
