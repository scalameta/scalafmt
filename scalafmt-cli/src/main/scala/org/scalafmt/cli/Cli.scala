package org.scalafmt.cli

import com.martiansoftware.nailgun.NGContext
import java.nio.file.{Files, Paths}

import org.scalafmt.Versions.{stable => stableVersion}
import org.scalafmt.sysops.AbsoluteFile

import scala.io.Source
import scala.util.Using
import scala.util.control.NoStackTrace

object Cli {
  def nailMain(nGContext: NGContext): Unit = {
    val workingDirectory =
      AbsoluteFile.fromPathIfAbsolute(nGContext.getWorkingDirectory).getOrElse {
        throw new IllegalStateException(
          s"Expected absolute path, " +
            s"obtained nGContext.getWorkingDirectory = ${nGContext.getWorkingDirectory}"
        )
      }
    val exit = mainWithOptions(
      nGContext.getArgs,
      CliOptions.default.copy(
        common = CliOptions.default.common.copy(
          cwd = Some(workingDirectory),
          out = nGContext.out,
          in = nGContext.in,
          err = nGContext.err
        )
      )
    )
    nGContext.exit(exit.code)
  }

  private def throwIfError(exit: ExitCode): Unit = {
    if (exit != ExitCode.Ok) {
      throw new RuntimeException(exit.toString) with NoStackTrace
    }
  }

  def main(args: Array[String]): Unit = {
    val exit = mainWithOptions(args, CliOptions.default)
    sys.exit(exit.code)
  }

  def exceptionThrowingMain(args: Array[String]): Unit = {
    exceptionThrowingMainWithOptions(args, CliOptions.default)
  }

  def exceptionThrowingMainWithOptions(
      args: Array[String],
      options: CliOptions
  ): Unit = {
    val exit = mainWithOptions(args, options)
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
      .map(CliOptions.auto)
  }

  private def expandArguments(args: Seq[String]): Seq[String] = {
    val builder = Seq.newBuilder[String]
    args.foreach { arg =>
      val atFile = arg.stripPrefix("@")
      if (atFile == arg) builder += arg // doesn't start with @
      else if (atFile == "-") builder ++= Source.stdin.getLines()
      else if (!Files.isRegularFile(Paths.get(atFile))) builder += arg
      else Using.resource(Source.fromFile(atFile))(builder ++= _.getLines())
    }
    builder.result()
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

  private def getProposedConfigVersion(options: CliOptions): String =
    s"version = '$stableVersion'"

  private def findRunner(
      options: CliOptions
  ): Either[String, ScalafmtRunner] = {
    // Run format using
    // - `scalafmt-dynamic` if the specified `version` setting doesn't match build version.
    // - `scalafmt-core` if the specified `version` setting match with build version
    //   (or if the `version` is not specified).
    val versionOpt = options.hoconOpt.map(x => Right(x.version)).getOrElse {
      Left(s"""error: missing Scalafmt configuration file.
        |Consider creating '${options.getProposedConfigFile}' with the following:
        |${getProposedConfigVersion(options)}
        |""".stripMargin)
    }
    versionOpt.flatMap {
      case None =>
        val where = options.configStr match {
          case None =>
            options.canonicalConfigFile
              .fold(options.getProposedConfigFile)(_.get)
              .toString
          case _ => "--config-str option"
        }
        Left(s"""error: missing Scalafmt version.
          |Consider adding the following to $where:
          |${getProposedConfigVersion(options)}
          |""".stripMargin)
      case Some(`stableVersion`) =>
        options.common.debug.println(s"Using core runner [$stableVersion]")
        Right(ScalafmtCoreRunner)
      case Some(v) if isNativeImage =>
        Left(
          s"""error: invalid Scalafmt version.
            |
            |This Scalafmt installation has version '$stableVersion' and the version configured in '${options.configPath}' is '$v'.
            |To fix this problem, add the following line to .scalafmt.conf:
            |```
            |version = '$stableVersion'
            |```
            |
            |NOTE: this error happens only when running a native Scalafmt binary.
            |Scalafmt automatically installs and invokes the correct version of Scalafmt when running on the JVM.
            |""".stripMargin
        )
      case Some(v) =>
        options.common.debug.println(s"Using dynamic runner [$v]")
        Right(ScalafmtDynamicRunner)
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
    options.common.debug.println("Working directory: " + options.cwd)

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
      exit.is(ExitCode.ParseError)
    ) {
      // Ignore parse errors etc.
      ExitCode.Ok
    } else {
      exit
    }
  }
}
