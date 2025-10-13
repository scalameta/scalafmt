package org.scalafmt.cli

import org.scalafmt.dynamic.ScalafmtDynamicError
import org.scalafmt.interfaces.{Scalafmt, ScalafmtSession}
import org.scalafmt.sysops.PlatformFileOps

import java.nio.file.Path

import scala.concurrent.Future

object ScalafmtDynamicRunner extends ScalafmtRunner {

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
      displayMsg: String,
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
    else runInputs(options, inputMethods, displayMsg) { case (code, path) =>
      val formatted = session.format(path, code)
      val exitCode = reporter.getExitCode(path)
      if (exitCode eq null) Right(formatted) else Left(exitCode)
    }
  }

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
