package org.scalafmt.cli

import com.martiansoftware.nailgun.NGContext
import java.io.{InputStream, PrintStream}
import org.scalafmt.Versions
import org.scalafmt.util.AbsoluteFile

import scala.util.control.NoStackTrace

object Cli {
  def nailMain(nGContext: NGContext): Unit = {
    val workingDirectory =
      AbsoluteFile.fromPath(nGContext.getWorkingDirectory).getOrElse {
        throw new IllegalStateException(
          s"Expected absolute path, " +
            s"obtained nGContext.getWorkingDirectory = ${nGContext.getWorkingDirectory}"
        )
      }
    val exit = mainWithOptions(
      nGContext.getArgs,
      CliOptions.default.copy(
        common = CliOptions.default.common.copy(
          workingDirectory = workingDirectory,
          out = nGContext.out,
          in = nGContext.in,
          err = nGContext.err
        )
      )
    )
    nGContext.exit(exit.code)
  }

  def main(
      args: Array[String],
      in: InputStream,
      out: PrintStream,
      err: PrintStream,
      workingDirectory: String
  ): Unit = {
    val options = CliOptions.default.copy(
      common = CommonOptions(
        in = in,
        out = out,
        err = err,
        workingDirectory = AbsoluteFile.fromPath(workingDirectory).get
      )
    )
    mainWithOptions(args, options)
  }

  private def throwIfError(exit: ExitCode): Unit = {
    if (exit != ExitCode.Ok) {
      throw new RuntimeException(exit.toString) with NoStackTrace
    }
  }

  def main(args: Array[String]): Unit = {
    val exit = mainWithOptions(args, CliOptions())
    sys.exit(exit.code)
  }

  def exceptionThrowingMain(args: Array[String]): Unit = {
    val exit = mainWithOptions(args, CliOptions.default)
    throwIfError(exit)
  }

  def mainWithOptions(args: Array[String], options: CliOptions): ExitCode = {
    getConfig(args, options) match {
      case Some(x) => run(x)
      case None => ExitCode.CommandLineArgumentError
    }
  }

  def getConfig(args: Array[String], init: CliOptions): Option[CliOptions] = {
    CliArgParser.scoptParser.parse(args, init).map(CliOptions.auto(args, init))
  }

  private[cli] def run(options: CliOptions): ExitCode = {
    val termDisplayMessage =
      if (options.testing) "Looking for unformatted files..."
      else "Reformatting..."
    if (options.debug) {
      val pwd = options.common.workingDirectory.jfile.getPath
      val out = options.info
      out.println("Working directory: " + pwd)
    }

    // Run format using
    // - `scalafmt-dynamic` if the specified `version` setting doesn't match build version.
    // - `scalafmt-core` if the specified `version` setting match with build version
    //   (or if the `version` is not specified).
    val runner: ScalafmtRunner = options.version match {
      case None => ScalafmtCoreRunner
      case Some(v) if v == Versions.version =>
        ScalafmtCoreRunner
      case _ => ScalafmtDynamicRunner
    }
    val exit = runner.run(options, termDisplayMessage)

    if (options.testing) {
      if (exit.isOk) {
        options.common.out.println("All files are formatted with scalafmt :)")
      } else if (exit.is(ExitCode.TestError)) {
        options.common.out.println(
          "error: --test failed"
        )
        options.onTestFailure.foreach(options.common.out.println)
      } else {
        options.common.out.println(s"error: $exit")
      }
    }
    if (options.testing &&
      !options.fatalWarnings &&
      !exit.is(ExitCode.TestError)) {
      // Ignore parse errors etc.
      ExitCode.Ok
    } else {
      exit
    }
  }
}
