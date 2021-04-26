package org.scalafmt.cli

import java.io.File
import java.nio.file.Path
import java.nio.file.Paths
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import org.scalafmt.CompatCollections.ParConverters._
import org.scalafmt.Error.{MisformattedFile, NoMatchingFiles}
import org.scalafmt.dynamic.ScalafmtDynamicError
import org.scalafmt.interfaces.Scalafmt
import org.scalafmt.interfaces.ScalafmtSession
import org.scalafmt.interfaces.ScalafmtSessionFactory
import org.scalafmt.util.AbsoluteFile

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
        scalafmtInstance match {
          case t: ScalafmtSessionFactory =>
            t.createSession(options.configPath)
          case _ => new MyInstanceSession(options, scalafmtInstance)
        }
      } catch {
        case _: ScalafmtDynamicError.ConfigError =>
          return reporter.getExitCode // XXX: returning
      }

    def sessionMatcher(x: AbsoluteFile): Boolean =
      session.matchesProjectFilters(x.jfile.toPath)
    val filterMatcher: AbsoluteFile => Boolean =
      if (options.customFiles.isEmpty) sessionMatcher
      else {
        val customMatcher = getFileMatcher(options.customFiles)
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
            reporter.error(e.file.toPath, e)
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

  private final class MyInstanceSession(opts: CliOptions, instance: Scalafmt)
      extends ScalafmtSession {
    // check config first
    instance.format(opts.configPath, opts.configPath, "")
    private val customFiles =
      if (opts.respectProjectFilters) Seq.empty
      else opts.customFiles.filter(_.jfile.isFile).map(_.jfile.toPath)
    override def format(file: Path, code: String): String = {
      // DESNOTE(2017-05-19, pjrt): A plain, fully passed file will (try to) be
      // formatted regardless of what it is or where it is.
      // NB: Unless respectProjectFilters is also specified.
      val formatter =
        if (customFiles.contains(file)) instance
        else instance.withRespectProjectFilters(true)
      formatter.format(opts.configPath, file, code)
    }
    override def matchesProjectFilters(file: Path): Boolean = true
  }

  private[this] def handleFile(
      inputMethod: InputMethod,
      session: ScalafmtSession,
      options: CliOptions
  ): ExitCode = {
    val input = inputMethod.readInput(options)

    val formatResult = session
      .format(
        Paths.get(inputMethod.filename),
        input
      )
    inputMethod.write(formatResult, input, options)
  }

  private def getFileMatcher(
      paths: Seq[AbsoluteFile]
  ): AbsoluteFile => Boolean = {
    require(paths.nonEmpty)
    val (files, dirs) = paths.partition(_.jfile.isFile)
    (x: AbsoluteFile) =>
      files.contains(x) || {
        val filename = x.path
        dirs.exists { dir =>
          val dirname = dir.path
          filename.startsWith(dirname) && (
            filename.length == dirname.length ||
              filename.charAt(dirname.length) == File.separatorChar
          )
        }
      }
  }

}
