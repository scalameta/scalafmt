package org.scalafmt.cli

import java.io.{IOException, InputStream, OutputStream, PrintStream}
import java.nio.charset.UnsupportedCharsetException
import java.nio.file.{Files, Path}

import com.typesafe.config.{ConfigException, ConfigFactory}
import metaconfig.Configured
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.config.Config
import org.scalafmt.util.{AbsoluteFile, GitOps, GitOpsImpl, OsSpecific}

import scala.io.Codec
import scala.util.control.NonFatal
import scala.util.matching.Regex
import scala.util.control.Exception.catching

object CliOptions {
  val default = CliOptions()

  /**
    * Tries to read configuration from
    *
    * 1. .scalafmt.conf in root dir of current git repo
    *     IF the following setting is enabled: project.git = true
    * 2. .scalafmt.conf from init.commong.workingDirectory
    *
    * I am happy to add alternative fallback methods for other VCS.
    *
    * WARNING. Throws an exception if the .scalafmt.conf error exists but
    * contains an error. Why? Because this method is only supposed to be
    * called directly from main.
    */
  def auto(args: Array[String], init: CliOptions)(
      parsed: CliOptions
  ): CliOptions = {
    val style: Option[Path] = if (init.config != parsed.config) {
      parsed.config
    } else {
      tryCurrentDirectory(parsed).orElse(tryGit(parsed))
    }

    val auxOut =
      if (
        parsed.noStdErr ||
        !(parsed.stdIn || parsed.writeMode == WriteMode.Stdout)
      )
        parsed.common.out
      else parsed.common.err

    parsed.copy(
      config = style,
      common = parsed.common.copy(
        out =
          guardPrintStream(parsed.quiet && !parsed.stdIn)(parsed.common.out),
        info = guardPrintStream(
          parsed.stdIn || parsed.writeMode == WriteMode.Stdout || parsed.quiet || parsed.writeMode == WriteMode.List
        )(auxOut),
        debug = guardPrintStream(parsed.quiet)(
          if (parsed.debug) auxOut else init.common.debug
        ),
        err = guardPrintStream(parsed.quiet)(parsed.common.err)
      )
    )
  }

  private def guardPrintStream(
      p: => Boolean
  )(candidate: PrintStream): PrintStream =
    if (p) NoopOutputStream.printStream else candidate

  private def getConfigJFile(file: AbsoluteFile): AbsoluteFile =
    file / ".scalafmt.conf"

  private def tryDirectory(dir: AbsoluteFile): Path =
    getConfigJFile(dir).jfile.toPath

  private def tryGit(options: CliOptions): Option[Path] = {
    for {
      rootDir <- options.gitOps.rootDir
      configFilePath <- Option(tryDirectory(rootDir)).filter(_.toFile.isFile)
    } yield configFilePath
  }

  private def tryCurrentDirectory(options: CliOptions): Option[Path] =
    Option(tryDirectory(options.common.workingDirectory))
      .filter(_.toFile.isFile)
}

object NoopOutputStream extends OutputStream { self =>
  override def write(b: Int): Unit = ()

  override def write(b: Array[Byte]): Unit = ()

  override def write(b: Array[Byte], off: Int, len: Int): Unit = ()

  val printStream = new PrintStream(self)
}

case class CommonOptions(
    workingDirectory: AbsoluteFile = AbsoluteFile.userDir,
    out: PrintStream = System.out,
    in: InputStream = System.in,
    err: PrintStream = System.err,
    debug: PrintStream = NoopOutputStream.printStream,
    info: PrintStream = NoopOutputStream.printStream
)

case class CliOptions(
    config: Option[Path] = None,
    configStr: Option[String] = None,
    range: Set[Range] = Set.empty[Range],
    customFiles: Seq[AbsoluteFile] = Nil,
    customExcludes: Seq[String] = Nil,
    respectProjectFilters: Boolean = false,
    git: Option[Boolean] = None,
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
  lazy val writeMode: WriteMode = writeModeOpt.getOrElse(WriteMode.Override)

  // These default values are copied from here.
  // https://github.com/scalameta/scalafmt/blob/f2154330afa0bc4a0a556598adeb116eafecb8e3/scalafmt-core/shared/src/main/scala/org/scalafmt/config/ScalafmtConfig.scala#L127-L162
  private[this] val DefaultGit = false
  private[this] val DefaultFatalWarnings = false
  private[this] val DefaultIgnoreWarnings = false
  private[this] val DefaultEncoding = Codec.UTF8

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
    tempConfigPath match {
      case Some(tempConf) => tempConf
      case None =>
        config.getOrElse(
          (common.workingDirectory / ".scalafmt.conf").jfile.toPath
        )
    }

  /** Parse the scalafmt configuration and try to encode it to `ScalafmtConfig`.
    * If `--config-str` is specified, this will parse the configuration string specified by `--config-str`.
    * Otherwise, a contents of configuration file specified by `configPath` will be parsed.
    *
    * If `--config-str` is not specified and configuration file is missing, this will return the default configuration
    */
  def scalafmtConfig: Configured[ScalafmtConfig] = {
    (configStr match {
      case Some(contents) => Some(Config.fromHoconString(contents))
      case None =>
        val file =
          AbsoluteFile.fromFile(configPath.toFile, common.workingDirectory)
        catching(classOf[IOException]).opt(Config.fromHoconFile(file.jfile))
    }).getOrElse(Configured.Ok(ScalafmtConfig.default))
  }

  val fileFetchMode: FileFetchMode =
    mode.orElse(Some(GitFiles).filter(_ => isGit)).getOrElse(RecursiveSearch)

  val files: Seq[AbsoluteFile] =
    if (customFiles.isEmpty)
      Seq(common.workingDirectory)
    else
      customFiles

  val gitOps: GitOps = gitOpsConstructor(common.workingDirectory)
  /*
  def withProject(projectFiles: ProjectFiles): CliOptions = {
    this.copy(config = config.copy(project = projectFiles))
  }
   */

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

  private[cli] def isGit: Boolean = readGit(configPath).getOrElse(DefaultGit)

  private[cli] def fatalWarnings: Boolean =
    readFatalWarnings(configPath).getOrElse(DefaultFatalWarnings)

  private[cli] def ignoreWarnings: Boolean =
    readIgnoreWarnings(configPath).getOrElse(DefaultIgnoreWarnings)

  private[cli] def onTestFailure: Option[String] = readOnTestFailure(configPath)

  private[cli] def encoding: Codec =
    readEncoding(configPath).getOrElse(DefaultEncoding)

  /** Returns None if .scalafmt.conf is not found or
    * version setting is missing.
    */
  private[cli] def version: Option[String] =
    readVersion(configPath)

  private def readGit(config: Path): Option[Boolean] = {
    try {
      Some(
        ConfigFactory
          .parseFile(config.toFile)
          .getConfig("project")
          .getBoolean("git")
      )
    } catch {
      case _: ConfigException.Missing => None
      case NonFatal(_) => None
    }
  }

  private def readOnTestFailure(config: Path): Option[String] = {
    try {
      Some(ConfigFactory.parseFile(config.toFile).getString("onTestFailure"))
    } catch {
      case _: ConfigException.Missing => None
      case NonFatal(_) => None
    }
  }

  private def readFatalWarnings(config: Path): Option[Boolean] = {
    try {
      Some(
        ConfigFactory
          .parseFile(config.toFile)
          .getConfig("runner")
          .getBoolean("fatalWarnings")
      )
    } catch {
      case _: ConfigException.Missing => None
      case NonFatal(_) => None
    }
  }

  private def readIgnoreWarnings(config: Path): Option[Boolean] = {
    try {
      Some(
        ConfigFactory
          .parseFile(config.toFile)
          .atPath("runner")
          .getBoolean("ignoreWarnings")
      )
    } catch {
      case _: ConfigException.Missing => None
      case NonFatal(_) => None
    }
  }

  private def readEncoding(config: Path): Option[Codec] = {
    try {
      val codecStr =
        ConfigFactory.parseFile(config.toFile).getString("encoding")
      Some(Codec.apply(codecStr))
    } catch {
      case _: ConfigException.Missing => None
      case _: UnsupportedCharsetException => None
      case NonFatal(_) => None
    }
  }

  private def readVersion(config: Path): Option[String] = {
    try {
      Some(ConfigFactory.parseFile(config.toFile).getString("version"))
    } catch {
      case _: ConfigException.Missing => None
      case NonFatal(_) => None
    }
  }
}
