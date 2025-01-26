package org.scalafmt.cli

import org.scalafmt.Error
import org.scalafmt.dynamic.ScalafmtDynamicError
import org.scalafmt.interfaces.Scalafmt
import org.scalafmt.interfaces.ScalafmtSession
import org.scalafmt.sysops.PlatformFileOps
import org.scalafmt.sysops.PlatformRunOps

import java.nio.file.Path

import scala.concurrent.Future

object ScalafmtDynamicRunner extends ScalafmtRunner {
  import org.scalafmt.sysops.PlatformRunOps.executionContext

  override private[cli] def run(
      options: CliOptions,
      termDisplayMessage: String,
  ): Future[ExitCode] = {
    val reporter = new ScalafmtCliReporter(options)
    val scalafmtInstance = Scalafmt.create(this.getClass.getClassLoader)
      .withReporter(reporter).withRespectProjectFilters(false)
    try runWithSession(options, termDisplayMessage, reporter)(
        scalafmtInstance.createSession(options.configPath),
      )
    catch {
      case _: ScalafmtDynamicError.ConfigError => reporter.getExitCode.future
    }
  }

  private def runWithSession(
      options: CliOptions,
      termDisplayMessage: String,
      reporter: ScalafmtCliReporter,
  )(session: ScalafmtSession): Future[ExitCode] = {
    val sessionMatcher = session.matchesProjectFilters _
    val filterMatcher: Path => Boolean = options.customFilesOpt
      .fold(sessionMatcher) { customFiles =>
        val customMatcher = getFileMatcher(customFiles.map(_.path))
        x => customMatcher(x) && sessionMatcher(x)
      }
    val inputMethods = getInputMethods(options, filterMatcher)
    if (inputMethods.isEmpty) ExitCode.Ok.future
    else runInputs(options, inputMethods, termDisplayMessage)(inputMethod =>
      handleFile(inputMethod, session, options).recover {
        case x: Error.MisformattedFile => reporter.fail(x)(x.file)
      }.map(ExitCode.merge(_, reporter.getExitCode)),
    )
  }

  private[this] def handleFile(
      inputMethod: InputMethod,
      session: ScalafmtSession,
      options: CliOptions,
  ): Future[ExitCode] = inputMethod.readInput(options)
    .map(code => code -> session.format(inputMethod.path, code))
    .flatMap { case (code, formattedCode) =>
      inputMethod.write(formattedCode, code, options)
    }(PlatformRunOps.ioExecutionContext)

  private def getFileMatcher(paths: Seq[Path]): Path => Boolean = {
    val dirBuilder = Seq.newBuilder[Path]
    val fileBuilder = Set.newBuilder[Path]
    paths.foreach(path =>
      if (PlatformFileOps.isRegularFile(path)) fileBuilder += path
      else dirBuilder += path,
    )
    val dirs = dirBuilder.result()
    val files = fileBuilder.result()
    x =>
      files(x) || {
        val filename = x.toString
        val sep = x.getFileSystem.getSeparator
        dirs.exists { dir =>
          val dirname = dir.toString
          filename.startsWith(dirname) && {
            filename.length == dirname.length ||
            filename.startsWith(sep, dirname.length)
          }
        }
      }
  }

}
