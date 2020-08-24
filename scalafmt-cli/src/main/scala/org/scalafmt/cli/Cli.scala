package org.scalafmt.cli

import com.martiansoftware.nailgun.NGContext
import java.io.{InputStream, PrintStream}
import java.nio.file.{Files, Paths}

import org.scalafmt.Versions
import org.scalafmt.util.AbsoluteFile

import scala.io.Source
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
    val expandedArguments = expandArguments(args)
    CliArgParser.scoptParser
      .parse(expandedArguments, init)
      .map(CliOptions.auto(expandedArguments, init))
  }

  private def expandArguments(args: Array[String]): Array[String] = {
    args.flatMap {
      case FileArgument(xs) => xs
      case x => List(x)
    }
  }
  private object FileArgument {
    def unapply(arg: String): Option[Iterator[String]] = {
      val atFile = arg.stripPrefix("@")
      if (atFile eq arg) None // doesn't start with @
      else if (atFile eq "-") Some(Source.stdin.getLines())
      else if (!Files.isRegularFile(Paths.get(atFile))) None
      else Some(Source.fromFile(atFile).getLines())
    }
  }

  private[cli] def run(options: CliOptions): ExitCode = {
    findRunner(options) match {
      case Left(message) =>
        options.common.err.println(message)
        ExitCode.UnsupportedVersion
      case Right(runner) =>
        runWithRunner(options, runner)
    }
  }

  private val isNativeImage: Boolean =
    "true" == System.getProperty("scalafmt.native-image", "false")

  private def findRunner(
      options: CliOptions
  ): Either[String, ScalafmtRunner] = {
    // Run format using
    // - `scalafmt-dynamic` if the specified `version` setting doesn't match build version.
    // - `scalafmt-core` if the specified `version` setting match with build version
    //   (or if the `version` is not specified).
    options.version match {
      case None =>
        Right(ScalafmtCoreRunner)
      case Some(v) =>
        if (v == Versions.version) {
          Right(ScalafmtCoreRunner)
        } else if (isNativeImage) {
          Left(
            s"""error: invalid Scalafmt version.
              |
              |This Scalafmt installation has version '${Versions.version}' and the version configured in '${options.configPath}' is '${v}'.
              |To fix this problem, add the following line to .scalafmt.conf:
              |```
              |version = '${Versions.version}'
              |```
              |
              |NOTE: this error happens only when running a native Scalafmt binary.
              |Scalafmt automatically installs and invokes the correct version of Scalafmt when running on the JVM.
              |""".stripMargin
          )
        } else {
          Right(ScalafmtDynamicRunner)
        }
    }

  }
  private[cli] def runWithRunner(
      options: CliOptions,
      runner: ScalafmtRunner
  ): ExitCode = {
    val termDisplayMessage =
      if (options.writeMode == WriteMode.Test)
        "Looking for unformatted files..."
      else "Reformatting..."
    options.common.debug.println(
      "Working directory: " + options.common.workingDirectory.jfile.getPath
    )

    val exit = runner.run(options, termDisplayMessage)

    if (options.writeMode == WriteMode.Test) {
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
    if (
      options.writeMode == WriteMode.Test &&
      !options.fatalWarnings &&
      !exit.is(ExitCode.TestError)
    ) {
      // Ignore parse errors etc.
      ExitCode.Ok
    } else {
      exit
    }
  }
}
