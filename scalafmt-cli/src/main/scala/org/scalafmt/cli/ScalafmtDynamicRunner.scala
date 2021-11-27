package org.scalafmt.cli

import java.io.File
import java.nio.file.Path
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import org.scalafmt.CompatCollections.ParConverters._
import org.scalafmt.Error.{MisformattedFile, NoMatchingFiles}
import org.scalafmt.dynamic.ScalafmtDynamicError
import org.scalafmt.interfaces.Scalafmt
import org.scalafmt.interfaces.ScalafmtSession
import org.scalafmt.sysops.FileOps

import scala.meta.internal.tokenizers.PlatformTokenizerCache
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
        val customMatcher = getFileMatcher(customFiles.map(_.path))
        x => customMatcher(x) && sessionMatcher(x)
      }
    val inputMethods = getInputMethods(options, filterMatcher)
    if (inputMethods.isEmpty && options.mode.isEmpty && !options.stdIn)
      throw NoMatchingFiles

    val counter = new AtomicInteger()
    val termDisplay = newTermDisplay(options, inputMethods, termDisplayMessage)

    val exitCode = new AtomicReference(ExitCode.Ok)
    breakable {
      inputMethods.par.foreach { inputMethod =>
        try {
          val code = handleFile(inputMethod, session, options)
          exitCode.getAndUpdate(ExitCode.merge(code, _))
        } catch {
          case e: MisformattedFile =>
            reporter.error(e.file, e)
            if (options.check) break
        }
        PlatformTokenizerCache.megaCache.clear()
        termDisplay.taskProgress(termDisplayMessage, counter.incrementAndGet())
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

  private def getFileMatcher(paths: Seq[Path]): Path => Boolean = {
    require(paths.nonEmpty)
    val (files, dirs) = paths.partition(FileOps.isRegularFile)
    (x: Path) =>
      files.contains(x) || {
        val filename = x.toString()
        dirs.exists { dir =>
          val dirname = dir.toString()
          filename.startsWith(dirname) && (
            filename.length == dirname.length ||
              filename.charAt(dirname.length) == File.separatorChar
          )
        }
      }
  }

}
