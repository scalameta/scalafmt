package org.scalafmt.cli

import java.io.{InputStream, OutputStream, PrintStream}
import java.nio.file.{Files, Path}

import metaconfig.{Conf, ConfDecoderEx, ConfDynamic, Configured}
import org.scalafmt.Versions
import org.scalafmt.config.{Config, ScalafmtConfig}
import org.scalafmt.util.{AbsoluteFile, GitOps, OsSpecific}

import scala.io.Codec
import scala.util.Try
import scala.util.matching.Regex

object CliOptions {
  val default = CliOptions()

  /** Tries to read configuration from
    *
    *   1. .scalafmt.conf in root dir of current git repo IF the following
    *      setting is enabled: project.git = true 2. .scalafmt.conf from
    *      init.common.workingDirectory
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

  private def tryGetConfigFile(dir: AbsoluteFile): Option[Path] = {
    val file = dir / ".scalafmt.conf"
    if (file.isRegularFile) Some(file.asPath) else None
  }

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
  lazy val workingDirectory: AbsoluteFile =
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
  import CliOptions._

  val writeMode: WriteMode = writeModeOpt.getOrElse(WriteMode.Override)

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
  def configPath: Path =
    configPathOpt.get

  private[cli] def configPathOpt: Option[Path] =
    tempConfigPath.orElse(canonicalConfigFile)

  private lazy val canonicalConfigFile = config
    .map(common.workingDirectory.join(_).asPath)
    .orElse(tryGetConfigFile(common.workingDirectory))
    .orElse(gitOps.rootDir.flatMap(tryGetConfigFile))

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

  private lazy val hoconOpt: Option[Configured[Conf]] =
    configStr match {
      case Some(contents) => Some(Config.hoconStringToConf(contents, None))
      case None => canonicalConfigFile.map(Config.hoconFileToConf(_, None))
    }

  lazy val fileFetchMode: FileFetchMode =
    mode.getOrElse(if (isGit) GitFiles else RecursiveSearch)

  lazy val customFilesOpt =
    if (customFiles.isEmpty) None
    else Some(common.workingDirectory.join(customFiles))

  def files: Seq[AbsoluteFile] =
    customFilesOpt.getOrElse(Seq(common.workingDirectory))

  lazy val gitOps: GitOps = gitOpsConstructor(common.workingDirectory)

  def addFile(file: Path): CliOptions = withFiles(customFiles :+ file)

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

  private def getHoconValueOpt[A](
      path: String*
  )(implicit ev: ConfDecoderEx[A]): Option[A] =
    hoconOpt.flatMap { x =>
      val conf = path.foldLeft(ConfDynamic(x))(_ selectDynamic _)
      conf.asConf.andThen(ev.read(None, _)) match {
        case Configured.Ok(x) => Some(x)
        case _ => None
      }
    }

  private def getHoconValue[A: ConfDecoderEx](default: A, path: String*): A =
    getHoconValueOpt[A](path: _*).getOrElse(default)

  private[cli] def isGit: Boolean =
    getHoconValue(baseConfig.project.git, "project", "git")

  private[cli] def fatalWarnings: Boolean =
    getHoconValue(
      baseConfig.runner.fatalWarnings,
      "runner",
      "fatalWarnings"
    )

  private[cli] def ignoreWarnings: Boolean =
    getHoconValue(
      baseConfig.runner.ignoreWarnings,
      "runner",
      "ignoreWarnings"
    )

  private[cli] def onTestFailure: Option[String] =
    getHoconValueOpt[String]("onTestFailure")

  private[cli] def encoding: Codec =
    getHoconValueOpt[String]("encoding")
      .flatMap(x => Try(Codec(x)).toOption)
      .getOrElse(baseConfig.encoding)

  /** Returns None if .scalafmt.conf is not found or version setting is missing.
    */
  private[cli] def getVersionIfDifferent: Option[String] =
    getHoconValueOpt[String]("version").filter(_ != Versions.version)

}
