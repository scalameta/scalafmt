package org.scalafmt.cli

import org.scalafmt.Versions.{stable => stableVersion}
import org.scalafmt.sysops.{FileOps, PlatformFileOps, PlatformRunOps}

import scala.concurrent.Future
import scala.util.{Failure, Success}

object Cli extends CliUtils {

  import PlatformRunOps.parasiticExecutionContext

  /** Wrap mainWithOptions in a Future, to make sure `inputExecutionContext` is
    * initialized before this method completes.
    *
    * When `main` returns, JVM will exit unless there are running non-daemon
    * threads, so let's make sure these threads exist.
    */
  def main(args: Array[String]): Unit = Future.unit.flatMap(_ =>
    mainWithOptions(CliOptions.default, args: _*),
  )(PlatformRunOps.inputExecutionContext).onComplete {
    case Failure(ex) =>
      ex.printStackTrace()
      PlatformRunOps.exit(ExitCode.UnexpectedError.code)
    case Success(exit) => PlatformRunOps.exit(exit.code)
  }

  def mainWithOptions(options: CliOptions, args: String*): Future[ExitCode] =
    getConfig(options, args: _*) match {
      case Some(x) => run(x)
      case None => ExitCode.CommandLineArgumentError.future
    }

  def getConfig(init: CliOptions, args: String*): Option[CliOptions] = {
    val expandedArguments = expandArguments(args)
    CliArgParser.scoptParser.parse(expandedArguments, init).map(CliOptions.auto)
  }

  private def expandArguments(args: Seq[String]): Seq[String] = {
    val builder = Seq.newBuilder[String]
    args.foreach { arg =>
      val atFile = arg.stripPrefix("@")
      if (atFile == arg) builder += arg // doesn't start with @
      else if (atFile == "-") builder ++= readInputLines
      else {
        val path = FileOps.getPath(atFile)
        if (!PlatformFileOps.isRegularFile(path)) builder += arg
        else PlatformFileOps.readFile(path).split('\n')
          .foreach(builder += _.trim)
      }
    }
    builder.result()
  }

  private[cli] def run(options: CliOptions): Future[ExitCode] =
    findRunner(options) match {
      case Left(message) =>
        options.common.err.println(message)
        ExitCode.UnsupportedVersion.future
      case Right(runner) =>
        val termDisplayMessage =
          if (options.writeMode != WriteMode.Test) "Reformatting..."
          else "Looking for unformatted files..."
        options.common.debug.println("Working directory: " + options.cwd)
        runner.run(options, termDisplayMessage).map(postProcess(options))
    }

  private val isNativeImage: Boolean = "true" ==
    System.getProperty("scalafmt.native-image", "false")

  private def getProposedConfigVersion(options: CliOptions): String =
    s"version = $stableVersion"

  private type MaybeRunner = Either[String, ScalafmtRunner]

  private def noDynamicRunner(version: String, options: CliOptions) = {
    val path = options.configPath
    s"""|error: invalid Scalafmt version.
        |
        |This Scalafmt installation has version '$stableVersion' and the version configured in '$path' is '$version'.
        |To fix this problem, add the following line to .scalafmt.conf:
        |```
        |version = $stableVersion
        |```
        |
        |NOTE: this error happens only when running a native Scalafmt binary.
        |Scalafmt automatically installs and invokes the correct version of Scalafmt when running on the JVM.
        |""".stripMargin
  }

  private def findRunner(options: CliOptions): MaybeRunner = options.hoconOpt
    .fold[MaybeRunner](Left(
      s"""|error: missing Scalafmt configuration file.
          |Consider creating '${options.getProposedConfigFile}'
          |with the following (other parameters may also be required):
          |${getProposedConfigVersion(options)}
          |""".stripMargin,
    )) {
      // Run format using
      // - `scalafmt-dynamic` if the specified `version` setting doesn't match build version.
      // - `scalafmt-core` if the specified `version` setting match with build version
      //   (or if the `version` is not specified).
      _.version.fold[MaybeRunner] {
        val where = options.configStr match {
          case None => options.canonicalConfigFile
              .fold(options.getProposedConfigFile)(_.get).toString
          case _ => "--config-str option"
        }
        Left(
          s"""|error: missing Scalafmt version.
              |Consider adding the following to $where:
              |${getProposedConfigVersion(options)}
              |""".stripMargin,
        )
      } {
        case Left(error) => Left(s"error: invalid configuration: $error")
        case Right(`stableVersion`) =>
          options.common.debug.println(s"Using core runner [$stableVersion]")
          Right(ScalafmtCoreRunner)
        case Right(v) =>
          val runnerOpt = if (isNativeImage) None else getDynamicRunner
          if (runnerOpt.isDefined) options.common.debug
            .println(s"Using dynamic runner [$v]")
          runnerOpt.toRight(noDynamicRunner(v, options))
      }
    }

  private def postProcess(options: CliOptions)(exit: ExitCode): ExitCode = {
    if (options.writeMode == WriteMode.Test)
      if (exit.isOk) options.common.out
        .println("All files are formatted with scalafmt :)")
      else if (exit.is(ExitCode.TestError)) {
        options.common.out.println("error: --test failed")
        options.onTestFailure.foreach(options.common.out.println)
      } else options.common.out.println(s"error: $exit")
    if (
      options.writeMode == WriteMode.Test && !options.fatalWarnings &&
      exit.is(ExitCode.ParseError)
    )
      // Ignore parse errors etc.
      ExitCode.Ok
    else exit
  }
}
