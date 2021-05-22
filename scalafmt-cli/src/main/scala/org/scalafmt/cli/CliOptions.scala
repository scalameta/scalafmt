package org.scalafmt.cli

import java.io.{File, InputStream, OutputStream, PrintStream}
import java.nio.file.{Files, Path}

import metaconfig.{Conf, ConfDecoderEx, ConfDynamic, Configured}
import org.scalafmt.Versions
import org.scalafmt.config.{Config, ScalafmtConfig}
import org.scalafmt.util.{AbsoluteFile, GitOps, GitOpsImpl, OsSpecific}

import scala.io.Codec
import scala.util.Try
import scala.util.matching.Regex

object CliOptions {
  val default = CliOptions()

  /** Tries to read configuration from
    *
    * 1. .scalafmt.conf in root dir of current git repo
    *     IF the following setting is enabled: project.git = true
    * 2. .scalafmt.conf from init.common.workingDirectory
    *
    * I am happy to add alternative fallback methods for other VCS.
    *
    * WARNING. Throws an exception if the .scalafmt.conf error exists but
    * contains an error. Why? Because this method is only supposed to be
    * called directly from main.
    */
  def auto(parsed: CliOptions): CliOptions = {
    val auxOut =
      if (
        parsed.noStdErr ||
        !(parsed.stdIn || parsed.writeMode == WriteMode.Stdout)
      )
        parsed.common.out
      else parsed.common.err

    parsed.copy(
      common = parsed.common.copy(
        out =
          guardPrintStream(parsed.quiet && !parsed.stdIn)(parsed.common.out),
        info = guardPrintStream(
          parsed.stdIn || parsed.writeMode == WriteMode.Stdout || parsed.quiet || parsed.writeMode == WriteMode.List
        )(auxOut),
        debug = guardPrintStream(parsed.quiet)(
          if (parsed.debug) auxOut else parsed.common.debug
        ),
        err = guardPrintStream(parsed.quiet)(parsed.common.err)
      )
    )
  }

  private def guardPrintStream(
      p: => Boolean
  )(candidate: PrintStream): PrintStream =
    if (p) NoopOutputStream.printStream else candidate

  private def tryGetConfigFile(dir: AbsoluteFile): Option[File] = {
    val file = (dir / ".scalafmt.conf").jfile
    if (file.isFile) Some(file) else None
  }

}

object NoopOutputStream extends OutputStream { self =>
  override def write(b: Int): Unit = ()

  override def write(b: Array[Byte]): Unit = ()

  override def write(b: Array[Byte], off: Int, len: Int): Unit = ()

  val printStream = new PrintStream(self)
}

case class CommonOptions(
    private val cwd: AbsoluteFile = null,
    out: PrintStream = System.out,
    in: InputStream = System.in,
    err: PrintStream = System.err,
    debug: PrintStream = NoopOutputStream.printStream,
    info: PrintStream = NoopOutputStream.printStream
) {
  lazy val workingDirectory: AbsoluteFile =
    Option(cwd).getOrElse(AbsoluteFile.userDir)
}

case class CliOptions(
    config: Option[Path] = None,
    configStr: Option[String] = None,
    range: Set[Range] = Set.empty[Range],
    customFiles: Seq[AbsoluteFile] = Nil,
    customExcludes: Seq[String] = Nil,
    respectProjectFilters: Boolean = false,
    nonInteractive: Boolean = false,
    mode: Option[FileFetchMode] = None,
    assumeFilename: String = "stdin.scala", // used when read from stdin
    migrate: Option[AbsoluteFile] = None,
    common: CommonOptions = CommonOptions(),
    gitOpsConstructor: AbsoluteFile => GitOps = x => new GitOpsImpl(x),
    writeModeOpt: Option[WriteMode] = None,
    debug: Boolean = false,
    quiet: Boolean = false,
    stdIn: Boolean = false,
    noStdErr: Boolean = false,
    error: Boolean = false,
    check: Boolean = false
) {
  import CliOptions._

  lazy val writeMode: WriteMode = writeModeOpt.getOrElse(WriteMode.Override)

  /** Create a temporary file that contains configuration string specified by `--config-str`.
    * This temporary file will be passed to `scalafmt-dynamic`.
    * See https://github.com/scalameta/scalafmt/pull/1367#issuecomment-464744077
    */
  private[this] val tempConfigPath: Option[Path] = configStr.map { s =>
    val file = Files.createTempFile(".scalafmt", ".conf")
    Files.write(file, s.getBytes)
    file
  }

  /** - If --config-str is specified (and tempConfigPath is defined),
    *   this returns the path to a temporary file.
    * - If both tempConfigPath and config are None,
    *   this return the path to `.scalafmt.conf` on the working directory.
    *
    * @return A path to a configuration file
    */
  def configPath: Path =
    configPathOpt.get

  private[cli] def configPathOpt: Option[Path] =
    (if (configStr.isEmpty) config else tempConfigPath)
      .orElse(defaultConfigFile.map(_.toPath))

  private lazy val defaultConfigFile: Option[File] =
    tryGetConfigFile(common.workingDirectory)
      .orElse(gitOps.rootDir.flatMap(tryGetConfigFile))

  /** Parse the scalafmt configuration and try to encode it to `ScalafmtConfig`.
    * If `--config-str` is specified, this will parse the configuration string specified by `--config-str`.
    * Otherwise, a contents of configuration file specified by `configPath` will be parsed.
    *
    * If `--config-str` is not specified and configuration file is missing, this will return the default configuration
    */
  def scalafmtConfig: Configured[ScalafmtConfig] =
    hoconOpt.fold(Configured.ok(ScalafmtConfig.default))(Config.fromConf(_))

  private lazy val hoconOpt: Option[Configured[Conf]] =
    configStr match {
      case Some(contents) => Some(Config.hoconStringToConf(contents, None))
      case None =>
        config
          .map { x =>
            AbsoluteFile.fromFile(x.toFile, common.workingDirectory).jfile
          }
          .orElse(defaultConfigFile)
          .map(Config.hoconFileToConf(_, None))
    }

  lazy val fileFetchMode: FileFetchMode =
    mode.getOrElse(if (isGit) GitFiles else RecursiveSearch)

  val files: Seq[AbsoluteFile] =
    if (customFiles.isEmpty)
      Seq(common.workingDirectory)
    else
      customFiles

  val gitOps: GitOps = gitOpsConstructor(common.workingDirectory)

  def withFiles(files: Seq[AbsoluteFile]): CliOptions = {
    this.copy(customFiles = files)
  }

  def excludeFilterRegexp: Regex =
    mkRegexp(customExcludes.map(OsSpecific.fixSeparatorsInPathPattern))

  private def mkRegexp(filters: Seq[String], strict: Boolean = false): Regex =
    filters match {
      case Nil => "$a".r // will never match anything
      case head :: Nil => head.r
      case _ if strict => filters.mkString("^(", "|", ")$").r
      case _ => filters.mkString("(", "|", ")").r
    }

  private def getHoconValueOpt[A](
      path: String*
  )(implicit ev: ConfDecoderEx[A]): Option[A] =
    hoconOpt.flatMap { x =>
      val conf = path.foldLeft(ConfDynamic(x))(_ selectDynamic _)
      conf.asConf.andThen(ev.read(None, _)): Option[A]
    }

  private def getHoconValue[A: ConfDecoderEx](default: A, path: String*): A =
    getHoconValueOpt[A](path: _*).getOrElse(default)

  private[cli] def isGit: Boolean =
    getHoconValue(ScalafmtConfig.default.project.git, "project", "git")

  private[cli] def fatalWarnings: Boolean =
    getHoconValue(
      ScalafmtConfig.default.runner.fatalWarnings,
      "runner",
      "fatalWarnings"
    )

  private[cli] def ignoreWarnings: Boolean =
    getHoconValue(
      ScalafmtConfig.default.runner.ignoreWarnings,
      "runner",
      "ignoreWarnings"
    )

  private[cli] def onTestFailure: Option[String] =
    getHoconValueOpt[String]("onTestFailure")

  private[cli] def encoding: Codec =
    getHoconValueOpt[String]("encoding")
      .flatMap(x => Try(Codec(x)).toOption)
      .getOrElse(ScalafmtConfig.default.encoding)

  /** Returns None if .scalafmt.conf is not found or
    * version setting is missing.
    */
  private[cli] def getVersionIfDifferent: Option[String] =
    getHoconValueOpt[String]("version").filter(_ != Versions.version)

}
