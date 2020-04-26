package org.scalafmt.cli

import java.io.File
import java.nio.file.Paths
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.function.UnaryOperator

import org.scalafmt.Error.{MisformattedFile, NoMatchingFiles}
import org.scalafmt.interfaces.Scalafmt
import org.scalafmt.util.AbsoluteFile

import scala.meta.internal.tokenizers.PlatformTokenizerCache
import util.control.Breaks._

object ScalafmtDynamicRunner extends ScalafmtRunner {
  override private[cli] def run(
      options: CliOptions,
      termDisplayMessage: String
  ): ExitCode = {
    val customMatcher = getFileMatcher(options.customFiles)
    val inputMethods = getInputMethods(
      options,
      customMatcher
    )
    if (inputMethods.isEmpty && options.mode.isEmpty && !options.stdIn)
      throw NoMatchingFiles

    val counter = new AtomicInteger()
    val reporter = new ScalafmtCliReporter(options)
    val scalafmtInstance = Scalafmt
      .create(this.getClass.getClassLoader)
      .withReporter(reporter)
      .withRespectProjectFilters(false)

    val termDisplay = newTermDisplay(options, inputMethods, termDisplayMessage)

    val exitCode = new AtomicReference(ExitCode.Ok)
    breakable {
      inputMethods.foreach { inputMethod =>
        try {
          val code = handleFile(inputMethod, scalafmtInstance, options)
          exitCode.getAndUpdate(new UnaryOperator[ExitCode] {
            override def apply(t: ExitCode): ExitCode =
              ExitCode.merge(code, t)
          })
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

  private[this] def handleFile(
      inputMethod: InputMethod,
      scalafmtInstance: Scalafmt,
      options: CliOptions
  ): ExitCode = {
    val input = inputMethod.readInput(options)

    // DESNOTE(2017-05-19, pjrt): A plain, fully passed file will (try to) be
    // formatted regardless of what it is or where it is.
    val ignoreFilters =
      AbsoluteFile.fromPath(inputMethod.filename).exists { file =>
        options.customFiles.contains(file)
      }
    val formatter =
      if (ignoreFilters) scalafmtInstance
      else scalafmtInstance.withRespectProjectFilters(true)
    val formatResult = formatter
      .format(
        options.configPath,
        Paths.get(inputMethod.filename),
        input
      )
    inputMethod.write(formatResult, input, options)
  }

  private def getFileMatcher(
      paths: Seq[AbsoluteFile]
  ): AbsoluteFile => Boolean = {
    if (paths.isEmpty) _ => true
    else {
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

}
