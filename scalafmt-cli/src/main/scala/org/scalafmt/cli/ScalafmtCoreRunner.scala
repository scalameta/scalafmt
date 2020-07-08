package org.scalafmt.cli

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.function.UnaryOperator
import util.control.Breaks

import metaconfig.Configured
import org.scalafmt.Error.{MisformattedFile, NoMatchingFiles}
import org.scalafmt.{Formatted, Scalafmt, Versions}
import org.scalafmt.config.{FilterMatcher, ScalafmtConfig}
import org.scalafmt.CompatCollections.ParConverters._
import org.scalafmt.util.OsSpecific

import scala.meta.internal.tokenizers.PlatformTokenizerCache
import scala.meta.parsers.ParseException
import scala.meta.tokenizers.TokenizeException

object ScalafmtCoreRunner extends ScalafmtRunner {
  override private[cli] def run(
      options: CliOptions,
      termDisplayMessage: String
  ): ExitCode = {
    options.scalafmtConfig match {
      case Configured.NotOk(e) =>
        options.common.err.println(s"${e.msg}")
        ExitCode.UnexpectedError
      case Configured.Ok(scalafmtConf) =>
        options.common.debug.println(s"parsed config (v${Versions.version})")
        val filterMatcher: FilterMatcher = FilterMatcher(
          scalafmtConf.project.includeFilters
            .map(OsSpecific.fixSeparatorsInPathPattern),
          (scalafmtConf.project.excludeFilters ++ options.customExcludes)
            .map(OsSpecific.fixSeparatorsInPathPattern)
        )

        val inputMethods = getInputMethods(options, filterMatcher.matchesFile)
        if (inputMethods.isEmpty && options.mode.isEmpty && !options.stdIn)
          throw NoMatchingFiles

        val counter = new AtomicInteger()
        val termDisplay =
          newTermDisplay(options, inputMethods, termDisplayMessage)
        val exitCode = new AtomicReference(ExitCode.Ok)
        Breaks.breakable {
          inputMethods.par.foreach { inputMethod =>
            val code = handleFile(inputMethod, options, scalafmtConf)
            exitCode.getAndUpdate(new UnaryOperator[ExitCode] {
              override def apply(t: ExitCode): ExitCode =
                ExitCode.merge(code, t)
            })
            if (options.check && !code.isOk) Breaks.break
            PlatformTokenizerCache.megaCache.clear()
            termDisplay.taskProgress(
              termDisplayMessage,
              counter.incrementAndGet()
            )
          }
        }
        termDisplay.completedTask(termDisplayMessage, exitCode.get.isOk)
        termDisplay.stop()
        exitCode.get()
    }
  }

  private[this] def handleFile(
      inputMethod: InputMethod,
      options: CliOptions,
      config: ScalafmtConfig
  ): ExitCode = {
    try unsafeHandleFile(inputMethod, options, config)
    catch {
      case MisformattedFile(_, diff) =>
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
    val formatResult = Scalafmt.formatCode(
      input,
      scalafmtConfig,
      options.range,
      inputMethod.filename
    )
    formatResult.formatted match {
      case Formatted.Success(formatted) =>
        inputMethod.write(formatted, input, options)
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
}
