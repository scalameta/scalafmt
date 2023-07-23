package org.scalafmt.cli

import java.util.concurrent.atomic.AtomicReference
import util.control.Breaks

import org.scalafmt.Error
import org.scalafmt.{Formatted, Scalafmt, Versions}
import org.scalafmt.config.{ProjectFiles, ScalafmtConfig}
import org.scalafmt.CompatCollections.ParConverters._

import scala.meta.parsers.ParseException
import scala.meta.tokenizers.TokenizeException

object ScalafmtCoreRunner extends ScalafmtRunner {
  override private[cli] def run(
      options: CliOptions,
      termDisplayMessage: String
  ): ExitCode =
    options.scalafmtConfig.fold { e =>
      options.common.err.println(s"${e.msg}")
      ExitCode.UnexpectedError
    } { scalafmtConf =>
      options.common.debug.println(s"parsed config (v${Versions.version})")
      val filterMatcher = ProjectFiles.FileMatcher(
        scalafmtConf.project,
        options.customExcludes
      )
      val inputMethods = getInputMethods(options, filterMatcher.matchesPath)

      val termDisplay =
        newTermDisplay(options, inputMethods, termDisplayMessage)
      val exitCode = new AtomicReference(ExitCode.Ok)
      Breaks.breakable {
        inputMethods.par.foreach { inputMethod =>
          val code = handleFile(inputMethod, options, scalafmtConf)
          exitCode.getAndUpdate(ExitCode.merge(code, _))
          if (options.check && !code.isOk) Breaks.break
          termDisplay.taskProgress(termDisplayMessage)
        }
      }
      termDisplay.completedTask(termDisplayMessage, exitCode.get.isOk)
      termDisplay.stop()
      exitCode.get()
    }

  private[this] def handleFile(
      inputMethod: InputMethod,
      options: CliOptions,
      config: ScalafmtConfig
  ): ExitCode = {
    try unsafeHandleFile(inputMethod, options, config)
    catch {
      case Error.MisformattedFile(_, diff) =>
        options.common.err.println(diff)
        ExitCode.TestError
    }
  }

  private[this] def unsafeHandleFile(
      inputMethod: InputMethod,
      options: CliOptions,
      scalafmtConfig: ScalafmtConfig
  ): ExitCode = {
    val input = inputMethod.readInput(options)
    val filename = inputMethod.path.toString
    val formatResult =
      Scalafmt.formatCode(input, scalafmtConfig, options.range, filename)
    formatResult.formatted match {
      case Formatted.Success(formatted) =>
        inputMethod.write(formatted, input, options)
      case _: Formatted.Failure if scalafmtConfig.runner.ignoreWarnings =>
        ExitCode.Ok // do nothing
      case Formatted.Failure(e @ (_: ParseException | _: TokenizeException)) =>
        options.common.err.println(e.toString)
        ExitCode.ParseError
      case Formatted.Failure(e) =>
        new FailedToFormat(filename, e).printStackTrace(options.common.out)
        ExitCode.UnexpectedError
    }
  }
}
