package org.scalafmt.cli

import java.io.File
import java.io.InputStream
import java.io.PrintStream

import org.scalafmt.config.Config
import org.scalafmt.config.ProjectFiles
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.FileOps
import org.scalafmt.util.GitOps
import org.scalafmt.util.GitOpsImpl

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
  def auto(init: CliOptions): CliOptions = {
    val style =
      tryGit(init)
        .orElse(tryCurrentDirectory(init))
        .getOrElse(init.config)
    init.copy(config = style)
  }

  private def getConfigJFile(file: File): File =
    new File(file, ".scalafmt.conf")

  private def tryDirectory(options: CliOptions)(
      dir: File): Option[ScalafmtConfig] = {
    for {
      configFile <- Option(getConfigJFile(dir))
      if configFile.isFile
      parsedConfig <- {
        Config.fromHocon(FileOps.readFile(configFile)) match {
          case Right(e) => Some(e)
          // fail fast, if .scalafmt.conf exists it should not contain errors.
          case Left(e) => throw e
        }
      }
      if parsedConfig.project.git
    } yield parsedConfig
  }

  private def tryGit(options: CliOptions): Option[ScalafmtConfig] = {
    options.gitOps.rootDir.flatMap(tryDirectory(options))
  }

  private def tryCurrentDirectory(
      options: CliOptions): Option[ScalafmtConfig] = {
    tryDirectory(options)(new File(options.common.workingDirectory))
  }
}
case class CommonOptions(
    workingDirectory: String = System.getProperty("user.dir"),
    out: PrintStream = System.out,
    in: InputStream = System.in,
    err: PrintStream = System.err
)

case class CliOptions(
    config: ScalafmtConfig = ScalafmtConfig.default,
    range: Set[Range] = Set.empty[Range],
    inPlace: Boolean = true,
    testing: Boolean = false,
    stdIn: Boolean = false,
    assumeFilename: String = "foobar.scala", // used when read from stdin
    migrate: Option[File] = None,
    common: CommonOptions = CommonOptions(),
    gitOps: GitOps = new GitOpsImpl
) {

  require(!(inPlace && testing), "inPlace and testing can't both be true")

  def withProject(projectFiles: ProjectFiles): CliOptions = {
    this.copy(config = config.copy(project = projectFiles))
  }

  def withFiles(files: Seq[File]): CliOptions = {
    this.copy(
      config = config.copy(
        project = config.project.copy(
          files = files.map(_.getPath)
        )
      )
    )
  }
}
