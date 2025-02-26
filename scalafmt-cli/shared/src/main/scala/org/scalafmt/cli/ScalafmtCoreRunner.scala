package org.scalafmt.cli

import org.scalafmt._
import org.scalafmt.config._

import scala.meta.parsers.ParseException
import scala.meta.tokenizers.TokenizeException

import scala.annotation.tailrec
import scala.concurrent.Future

object ScalafmtCoreRunner extends ScalafmtRunner {

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
      case e @ (_: ScalafmtConfigException | _: sysops.ScalafmtSysException) =>
        options.common.err.println(e.getMessage)
        ExitCode.UnexpectedError.future
    }
  }

  private def runWithFilterMatcher(options: CliOptions, displayMsg: String)(
      filterMatcher: ProjectFiles.FileMatcher,
  )(implicit scalafmtConf: ScalafmtConfig): Future[ExitCode] = {
    val inputMethods = getInputMethods(options, filterMatcher.matchesPath)
    if (inputMethods.isEmpty) ExitCode.Ok.future
    else {
      val adjustedScalafmtConf = {
        if (scalafmtConf.needGitAutoCRLF) options.gitOps.getAutoCRLF else None
      }.fold(scalafmtConf)(scalafmtConf.withGitAutoCRLF)
      runInputs(options, inputMethods, displayMsg) { case (code, path) =>
        handleFile(code, path.toString, options, adjustedScalafmtConf)
      }
    }
  }

  private[this] def handleFile(
      code: String,
      path: String,
      options: CliOptions,
      scalafmtConfig: ScalafmtConfig,
  ): Either[ExitCode, String] = {
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
    val res = Scalafmt.formatCode(code, scalafmtConfig, options.range, path)
    res.formatted match {
      case x: Formatted.Success => Right(x.formattedCode)
      case x: Formatted.Failure => Left(
          if (res.config.runner.ignoreWarnings) ExitCode.Ok // do nothing
          else handleError(x.e),
        )
    }
  }

}
