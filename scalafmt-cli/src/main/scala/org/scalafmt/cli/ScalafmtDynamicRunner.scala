package org.scalafmt.cli
import java.nio.file.Paths
import java.util.concurrent.atomic.AtomicInteger

import org.scalafmt.Error.{MisformattedFile, NoMatchingFiles}
import org.scalafmt.interfaces.Scalafmt
import org.scalafmt.util.AbsoluteFile

import scala.meta.internal.tokenizers.PlatformTokenizerCache

object ScalafmtDynamicRunner extends ScalafmtRunner {
  override private[cli] def run(
      options: CliOptions,
      termDisplayMessage: String
  ): ExitCode = {
    val inputMethods = getInputMethods(options, None)
    if (inputMethods.isEmpty && options.diff.isEmpty && !options.stdIn)
      throw NoMatchingFiles

    val counter = new AtomicInteger()
    val reporter = new ScalafmtCliReporter(options)
    val scalafmtInstance = Scalafmt
      .create(this.getClass.getClassLoader)
      .withReporter(reporter)

    // Path names fully qualified by `customFiles`.
    // If there exists fqpns, create another instance of `scalafmt-dynamic`
    // that ignores `excludeFilters` because fully qualified files will (try to) be
    // formatted regardless of what it is or where it is (and excludeFilter).
    val fqpns = inputMethods.filter { input =>
      AbsoluteFile.fromPath(input.filename).forall { file =>
        options.customFiles.contains(file)
      }
    }
    val scalafmtInstanceIgnoreFilters =
      if (fqpns.isEmpty) scalafmtInstance
      else
        Scalafmt
          .create(this.getClass.getClassLoader)
          .withReporter(reporter)
          .withRespectProjectFilters(false)

    val termDisplay = newTermDisplay(options, inputMethods, termDisplayMessage)

    inputMethods.foreach { inputMethod =>
      val instance =
        // Use scalafmt-dynamic that ignores exclude filters for fully qualified paths
        if (fqpns.contains(inputMethod)) scalafmtInstanceIgnoreFilters
        else scalafmtInstance
      try handleFile(inputMethod, instance, options)
      catch {
        case e: MisformattedFile => reporter.error(e.file.toPath, e)
      }
      PlatformTokenizerCache.megaCache.clear()
      termDisplay.taskProgress(termDisplayMessage, counter.incrementAndGet())
    }

    val exit = reporter.getExitCode

    termDisplay.completedTask(termDisplayMessage, exit.isOk)
    termDisplay.stop()

    exit
  }

  private[this] def handleFile(
      inputMethod: InputMethod,
      scalafmtInstance: Scalafmt,
      options: CliOptions
  ): Unit = {
    val input = inputMethod.readInput(options)

    // DESNOTE(2017-05-19, pjrt): A plain, fully passed file will (try to) be
    // formatted regardless of what it is or where it is.
    val shouldRespectFilters =
      AbsoluteFile.fromPath(inputMethod.filename).forall { file =>
        !options.customFiles.contains(file)
      }
    val formatResult = scalafmtInstance
      .withRespectProjectFilters(shouldRespectFilters)
      .format(
        options.configPath,
        Paths.get(inputMethod.filename),
        input
      )
    inputMethod.write(formatResult, input, options)
  }
}
