package org.scalafmt.cli

import org.scalafmt.Error
import org.scalafmt.Formatted
import org.scalafmt.Scalafmt
import org.scalafmt.Versions
import org.scalafmt.config.ProjectFiles
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.config.ScalafmtConfigException
import org.scalafmt.sysops.PlatformRunOps

import scala.meta.parsers.ParseException
import scala.meta.tokenizers.TokenizeException

import scala.annotation.tailrec
import scala.concurrent.Future

object ScalafmtCoreRunner extends ScalafmtRunner {
  import org.scalafmt.sysops.PlatformRunOps.executionContext

  override private[cli] def run(
      options: CliOptions,
      termDisplayMessage: String,
  ): Future[ExitCode] = options.scalafmtConfig.fold { e =>
    options.common.err.println(s"${e.msg}")
    ExitCode.UnexpectedError.future
  } { implicit scalafmtConf =>
    options.common.debug.println(s"parsed config (v${Versions.version})")
    try runWithFilterMatcher(options, termDisplayMessage)(
        ProjectFiles.FileMatcher(scalafmtConf.project, options.customExcludes),
      )
    catch {
      case e: ScalafmtConfigException =>
        options.common.err.println(e.getMessage)
        ExitCode.UnexpectedError.future
    }
  }

  private def runWithFilterMatcher(
      options: CliOptions,
      termDisplayMessage: String,
  )(
      filterMatcher: ProjectFiles.FileMatcher,
  )(implicit scalafmtConf: ScalafmtConfig): Future[ExitCode] = {
    val inputMethods = getInputMethods(options, filterMatcher.matchesPath)
    if (inputMethods.isEmpty) ExitCode.Ok.future
    else {
      val adjustedScalafmtConf = {
        if (scalafmtConf.needGitAutoCRLF) options.gitOps.getAutoCRLF else None
      }.fold(scalafmtConf)(scalafmtConf.withGitAutoCRLF)

      runInputs(options, inputMethods, termDisplayMessage)(inputMethod =>
        handleFile(inputMethod, options, adjustedScalafmtConf)
          .recover { case e: Error.MisformattedFile =>
            options.common.err.println(e.customMessage)
            ExitCode.TestError
          },
      )
    }
  }

  private[this] def handleFile(
      inputMethod: InputMethod,
      options: CliOptions,
      scalafmtConfig: ScalafmtConfig,
  ): Future[ExitCode] = {
    val path = inputMethod.path.toString
    @tailrec
    def handleError(e: Throwable): ExitCode = e match {
      case Error.WithCode(e, _) => handleError(e)
      case _: ParseException | _: TokenizeException =>
        options.common.err.println(e.toString)
        ExitCode.ParseError
      case e =>
        new FailedToFormat(path, e).printStackTrace(options.common.err)
        ExitCode.UnexpectedError
    }
    inputMethod.readInput(options).map { code =>
      val res = Scalafmt.formatCode(code, scalafmtConfig, options.range, path)
      res.formatted match {
        case x: Formatted.Success => Right(code -> x.formattedCode)
        case x: Formatted.Failure => Left(
            if (res.config.runner.ignoreWarnings) ExitCode.Ok // do nothing
            else handleError(x.e),
          )
      }
    }.flatMap {
      case Right((code, formattedCode)) => inputMethod
          .write(formattedCode, code, options)
      case Left(exitCode) => exitCode.future
    }(PlatformRunOps.ioExecutionContext)
  }

}
