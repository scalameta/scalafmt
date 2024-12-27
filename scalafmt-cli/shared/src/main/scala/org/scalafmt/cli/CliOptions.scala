package org.scalafmt.cli

import org.scalafmt.config.ConfParsed
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.config.ScalafmtConfigException
import org.scalafmt.sysops.AbsoluteFile
import org.scalafmt.sysops.GitOps
import org.scalafmt.sysops.OsSpecific

import java.io.InputStream
import java.io.PrintStream
import java.io.PrintWriter
import java.nio.file.Files
import java.nio.file.NoSuchFileException
import java.nio.file.Path

import scala.io.Codec
import scala.util.Try
import scala.util.matching.Regex

import metaconfig.Configured

object CliOptions extends CliOptionsUtils {
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
    val info: Output.StreamOrWriter =
      if (parsed.quiet) Output.NoopStream
      else {
        val usesOut = parsed.stdIn || parsed.writeMode.usesOut
        val consWriterOpt = if (usesOut) getConsoleWriter() else None
        consWriterOpt match {
          case Some(writer) => new Output.FromWriter(writer)
          case None => new Output.FromStream(
              if (parsed.noStdErr || !usesOut) parsed.common.out
              else parsed.common.err,
            )
        }
      }
    val common = parsed.common.copy(
      out = guardPrintStream(parsed.quiet && !parsed.stdIn)(parsed.common.out),
      info = info,
      debug = (if (parsed.debug) info else Output.NoopStream).printWriter,
      err = guardPrintStream(parsed.quiet)(parsed.common.err),
    )

    parsed.copy(common = common)
  }

  private def guardPrintStream(p: Boolean)(
      candidate: PrintStream,
  ): PrintStream = if (p) Output.NoopStream.printStream else candidate

}

case class CommonOptions(
    private val cwd: Option[AbsoluteFile] = None,
    out: PrintStream = System.out,
    in: InputStream = System.in,
    err: PrintStream = System.err,
    debug: PrintWriter = Output.NoopStream.printWriter,
    info: Output.StreamOrWriter = Output.NoopStream,
) {
  private[cli] lazy val workingDirectory: AbsoluteFile = cwd
    .getOrElse(AbsoluteFile.userDir)
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
    check: Boolean = false,
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
  def configPath: Path = tempConfigPath.getOrElse(
    canonicalConfigFile
      .fold(throw new NoSuchFileException("Config file not found"))(_.get),
  )

  private[cli] lazy val canonicalConfigFile: Option[Try[Path]] = gitOps
    .getCanonicalConfigFile(cwd, config)

  private[cli] def getProposedConfigFile: Path = canonicalConfigFile
    .flatMap(_.toOption)
    .getOrElse(gitOps.getProposedConfigFile(cwd, config).path)

  /** Parse the scalafmt configuration and try to encode it to `ScalafmtConfig`.
    * If `--config-str` is specified, this will parse the configuration string
    * specified by `--config-str`. Otherwise, a contents of configuration file
    * specified by `configPath` will be parsed.
    *
    * If `--config-str` is not specified and configuration file is missing, this
    * will return the default configuration
    */
  def scalafmtConfig: Configured[ScalafmtConfig] = hoconOpt
    .fold(Configured.ok(baseConfig))(ScalafmtConfig.fromConf(_, baseConfig))

  private[cli] lazy val hoconOpt: Option[ConfParsed] = configStr
    .map(ConfParsed.fromString(_)).orElse(canonicalConfigFile.map(
      _.fold(x => new ConfParsed(Configured.exception(x)), ConfParsed.fromPath(_)),
    ))

  lazy val fileFetchMode: FileFetchMode = mode
    .getOrElse(if (isGit) GitFiles else RecursiveSearch)

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

  private def getHoconValueOpt[A](
      f: ConfParsed => Option[Either[String, A]],
  ): Option[A] = hoconOpt.flatMap(f).map {
    case Right(x) => x
    case Left(x) => throw new ScalafmtConfigException(x)
  }

  private def getHoconValue[A](
      default: A,
      f: ConfParsed => Option[Either[String, A]],
  ): A = getHoconValueOpt[A](f).getOrElse(default)

  private[cli] def isGit: Boolean =
    getHoconValue(baseConfig.project.git, _.isGit)

  private[cli] def fatalWarnings: Boolean =
    getHoconValue(baseConfig.runner.fatalWarnings, _.fatalWarnings)

  private[cli] def ignoreWarnings: Boolean =
    getHoconValue(baseConfig.runner.ignoreWarnings, _.ignoreWarnings)

  private[cli] def onTestFailure: Option[String] =
    getHoconValueOpt(_.onTestFailure)

  private[cli] def encoding: Codec = getHoconValueOpt(_.encoding)
    .getOrElse(baseConfig.encoding)

  /** Returns None if .scalafmt.conf is not found or version setting is missing.
    */
  private[cli] def getVersionOpt: Option[String] = getHoconValueOpt(_.version)

}
