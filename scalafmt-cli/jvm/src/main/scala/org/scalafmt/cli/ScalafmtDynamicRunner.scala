package org.scalafmt.cli

import org.scalafmt.Error
import org.scalafmt.dynamic.ScalafmtDynamicError
import org.scalafmt.interfaces.Scalafmt
import org.scalafmt.interfaces.ScalafmtSession
import org.scalafmt.sysops.PlatformFileOps

import java.nio.file.Path

import scala.concurrent.Future

object ScalafmtDynamicRunner extends ScalafmtRunner {
  override private[cli] def run(
      options: CliOptions,
      termDisplayMessage: String,
  ): ExitCode = {
    val reporter = new ScalafmtCliReporter(options)
    val scalafmtInstance = Scalafmt.create(this.getClass.getClassLoader)
      .withReporter(reporter).withRespectProjectFilters(false)

    val session =
      try scalafmtInstance.createSession(options.configPath)
      catch {
        case _: ScalafmtDynamicError.ConfigError => return reporter.getExitCode // XXX: returning
      }

    val sessionMatcher = session.matchesProjectFilters _
    val filterMatcher: Path => Boolean = options.customFilesOpt
      .fold(sessionMatcher) { customFiles =>
        val customMatcher = getFileMatcher(customFiles.map(_.path))
        x => customMatcher(x) && sessionMatcher(x)
      }
    val inputMethods = getInputMethods(options, filterMatcher)
    if (inputMethods.isEmpty) ExitCode.Ok
    else runInputs(options, inputMethods, termDisplayMessage) { inputMethod =>
      import org.scalafmt.sysops.PlatformRunOps.executionContext
      Future(handleFile(inputMethod, session, options)).recover {
        case x: Error.MisformattedFile => reporter.fail(x)(x.file)
      }.map(ExitCode.merge(_, reporter.getExitCode))
    }
  }

  private[this] def handleFile(
      inputMethod: InputMethod,
      session: ScalafmtSession,
      options: CliOptions,
  ): ExitCode = {
    val input = inputMethod.readInput(options)

    val formatResult = session.format(inputMethod.path, input)
    inputMethod.write(formatResult, input, options)
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
