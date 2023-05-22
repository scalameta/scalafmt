package org.scalafmt.cli

import java.io.{InputStream, OutputStream, PrintStream}
import java.nio.file.{Files, NoSuchFileException, Path}

import metaconfig.Configured
import org.scalafmt.config.{Config, ConfParsed, ScalafmtConfig}
import org.scalafmt.sysops.{AbsoluteFile, GitOps, OsSpecific}

import scala.io.Codec
import scala.util.Try
import scala.util.matching.Regex

object CliOptions {
  val default = CliOptions()

  /** Tries to read configuration from
    *
    *   1. .scalafmt.conf in root dir of current git repo IF the following
    *      setting is enabled: project.git = true
    *   1. .scalafmt.conf from init.common.workingDirectory
    *
    * I am happy to add alternative fallback methods for other VCS.
    *
    * WARNING. Throws an exception if the .scalafmt.conf error exists but
    * contains an error. Why? Because this method is only supposed to be called
    * directly from main.
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

}

object NoopOutputStream extends OutputStream { self =>
  override def write(b: Int): Unit = ()

  override def write(b: Array[Byte]): Unit = ()

  override def write(b: Array[Byte], off: Int, len: Int): Unit = ()

  val printStream = new PrintStream(self)
}

case class CommonOptions(
    private val cwd: Option[AbsoluteFile] = None,
    out: PrintStream = System.out,
    in: InputStream = System.in,
    err: PrintStream = System.err,
    debug: PrintStream = NoopOutputStream.printStream,
    info: PrintStream = NoopOutputStream.printStream
) {
  private[cli] lazy val workingDirectory: AbsoluteFile =
    cwd.getOrElse(AbsoluteFile.userDir)
}

case class CliOptions(
    private[cli] val config: Option[Path] = None,
    private[cli] val baseConfig: ScalafmtConfig =
      ScalafmtConfig.uncheckedDefault,
    configStr: Option[String] = None,
    range: Set[Range] = Set.empty[Range],
    private[cli] val customFiles: Seq[Path] = Nil,
    customExcludes: Seq[String] = Nil,
    respectProjectFilters: Boolean = false,
    nonInteractive: Boolean = false,
    mode: Option[FileFetchMode] = None,
    assumeFilename: String = "stdin.scala", // used when read from stdin
    common: CommonOptions = CommonOptions(),
    gitOpsConstructor: GitOps.Factory = GitOps.FactoryImpl,
    writeModeOpt: Option[WriteMode] = None,
    debug: Boolean = false,
    quiet: Boolean = false,
    stdIn: Boolean = false,
    noStdErr: Boolean = false,
    error: Boolean = false,
    check: Boolean = false
) {
  val writeMode: WriteMode = writeModeOpt.getOrElse(WriteMode.Override)

  def cwd: AbsoluteFile = common.workingDirectory

  /** Create a temporary file that contains configuration string specified by
    * `--config-str`. This temporary file will be passed to `scalafmt-dynamic`.
    * See https://github.com/scalameta/scalafmt/pull/1367#issuecomment-464744077
    */
  private[this] val tempConfigPath: Option[Path] = configStr.map { s =>
    val file = Files.createTempFile(".scalafmt", ".conf")
    Files.write(file, s.getBytes)
    file
  }

  /**   - If --config-str is specified (and tempConfigPath is defined), this
    *     returns the path to a temporary file.
    *   - If both tempConfigPath and config are None, this return the path to
    *     `.scalafmt.conf` on the working directory.
    *
    * @return
    *   A path to a configuration file
    */
  def configPath: Path = tempConfigPath.getOrElse {
    canonicalConfigFile
      .fold(throw new NoSuchFileException("Config file not found"))(_.get)
  }

  private[cli] lazy val canonicalConfigFile: Option[Try[Path]] =
    gitOps.getCanonicalConfigFile(cwd, config)

  private[cli] def getProposedConfigFile: Path =
    canonicalConfigFile.flatMap(_.toOption).getOrElse {
      gitOps.getProposedConfigFile(cwd, config).path
    }

  /** Parse the scalafmt configuration and try to encode it to `ScalafmtConfig`.
    * If `--config-str` is specified, this will parse the configuration string
    * specified by `--config-str`. Otherwise, a contents of configuration file
    * specified by `configPath` will be parsed.
    *
    * If `--config-str` is not specified and configuration file is missing, this
    * will return the default configuration
    */
  def scalafmtConfig: Configured[ScalafmtConfig] =
    hoconOpt.fold(Configured.ok(baseConfig))(Config.fromConf(_, baseConfig))

  private[cli] lazy val hoconOpt: Option[ConfParsed] =
    configStr.map(ConfParsed.fromString(_)).orElse {
      canonicalConfigFile.map(
        _.fold(
          x => new ConfParsed(Configured.exception(x)),
          ConfParsed.fromPath(_)
        )
      )
    }

  lazy val fileFetchMode: FileFetchMode =
    mode.getOrElse(if (isGit) GitFiles else RecursiveSearch)

  lazy val customFilesOpt =
    if (customFiles.isEmpty) None else Some(cwd.join(customFiles))

  lazy val gitOps: GitOps = gitOpsConstructor(cwd)

  def addFile(file: Path): CliOptions = withFiles(file +: customFiles)

  def withFiles(files: Seq[Path]): CliOptions = this.copy(customFiles = files)

  def excludeFilterRegexp: Regex =
    mkRegexp(customExcludes.map(OsSpecific.fixSeparatorsInPathPattern))

  private def mkRegexp(filters: Seq[String], strict: Boolean = false): Regex =
    filters match {
      case Nil => "$a".r // will never match anything
      case head :: Nil => head.r
      case _ if strict => filters.mkString("^(", "|", ")$").r
      case _ => filters.mkString("(", "|", ")").r
    }

  private def getHoconValueOpt[A](f: ConfParsed => Option[A]): Option[A] =
    hoconOpt.flatMap(f)

  private def getHoconValue[A](default: A, f: ConfParsed => Option[A]): A =
    getHoconValueOpt[A](f).getOrElse(default)

  private[cli] def isGit: Boolean =
    getHoconValue(baseConfig.project.git, _.isGit)

  private[cli] def fatalWarnings: Boolean =
    getHoconValue(baseConfig.runner.fatalWarnings, _.fatalWarnings)

  private[cli] def ignoreWarnings: Boolean =
    getHoconValue(baseConfig.runner.ignoreWarnings, _.ignoreWarnings)

  private[cli] def onTestFailure: Option[String] =
    getHoconValueOpt(_.onTestFailure)

  private[cli] def encoding: Codec =
    getHoconValueOpt(_.encoding).getOrElse(baseConfig.encoding)

  /** Returns None if .scalafmt.conf is not found or version setting is missing.
    */
  private[cli] def getVersionOpt: Option[String] =
    getHoconValueOpt(_.version)

}
