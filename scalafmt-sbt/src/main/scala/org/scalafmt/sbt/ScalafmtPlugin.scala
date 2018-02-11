package org.scalafmt.sbt

import org.scalafmt.config.{Config, ScalafmtConfig}
import org.scalafmt.{Formatted, Scalafmt}
import sbt.Keys._
import sbt.{Def, _}
import complete.DefaultParsers._
import metaconfig.Configured
import sbt.util.Logger

import scala.meta.internal.tokenizers.PlatformTokenizerCache
import scala.util.{Failure, Success, Try}

import org.scalafmt.cli.StyleCache

object ScalafmtPlugin extends AutoPlugin {
  override def trigger: PluginTrigger = allRequirements

  object autoImport {
    val scalafmt = taskKey[Unit]("Format Scala sources.")
    val scalafmtCheck =
      taskKey[Boolean](
        "Fails if a Scala source is mis-formatted. Does not write to files.")
    val scalafmtOnCompile =
      settingKey[Boolean](
        "Format Scala source files on compile, off by default.")
    val scalafmtConfig = taskKey[Option[File]](
      "Optional location of .scalafmt.conf file. If None the default config is used.")
    val scalafmtSbt = taskKey[Unit](
      "Format *.sbt and project/*.scala files for this sbt build.")
    val scalafmtSbtCheck =
      taskKey[Boolean](
        "Fails if a *.sbt or project/*.scala source is mis-formatted. Does not write to files.")
    val scalafmtOnly = inputKey[Unit]("Format only given file.")
  }
  import autoImport._

  private val scalafmtDoFormatOnCompile =
    taskKey[Unit]("Format Scala source files if scalafmtOnCompile is on.")

  private val scalaConfig =
    scalafmtConfig
      .map(_.flatMap(f => StyleCache.getStyleForFile(f.toString)))
      .getOrElse(ScalafmtConfig.default)
  private val sbtConfig = scalaConfig.map(_.forSbt)

  private def filterSource(source: File, config: ScalafmtConfig): Boolean =
    config.project.matcher.matches(source.toString)
  private def filterScala(source: File): Boolean =
    source.toString.endsWith(".scala")
  private def filterSbt(source: File): Boolean =
    source.toString.endsWith(".sbt")

  private type Input = String
  private type Output = String

  private def withFormattedSources[T](
      sources: Seq[File],
      config: ScalafmtConfig
  )(
      onError: (File, Throwable) => T,
      onFormat: (File, Input, Output) => T
  ): Seq[Option[T]] = {
    sources
      .withFilter(filterSource(_, config))
      .map(
        file => {
          val input = IO.read(file)
          val output = Scalafmt.format(input, config)

          output match {
            case Formatted.Failure(e) =>
              if (config.runner.fatalWarnings) {
                throw e
              } else if (config.runner.ignoreWarnings) {
                // do nothing
                None
              } else {
                Some(onError(file, e))
              }
            case Formatted.Success(code) =>
              Some(onFormat(file, input, code))
          }
        }
      )
  }

  private def formatSources(
      sources: Seq[File],
      config: ScalafmtConfig,
      log: Logger,
      cache: File
  ): Unit = {

    def handleUpdate(
        in: ChangeReport[File],
        out: ChangeReport[File]): Set[File] = {
      val files = in.modified -- in.removed
      val cnt = withFormattedSources(files.toSeq, config)(
        (file, e) => {
          log.error(s"Error in ${file.toString}: $e")
          0
        },
        (file, input, output) => {
          if (input != output) {
            IO.write(file, output)
            1
          } else {
            0
          }
        }
      ).flatten.sum
      if (cnt > 1) {
        log.info(s"Reformatted $cnt Scala sources")
      }
      files
    }

    FileFunction.cached(cache)(FilesInfo.hash, FilesInfo.exists)(handleUpdate)(
      sources.toSet)

    PlatformTokenizerCache.megaCache.clear()
  }

  private def checkSources(
      sources: Seq[File],
      config: ScalafmtConfig,
      log: Logger
  ): Boolean = {
    val res = withFormattedSources(sources, config)(
      (file, e) => {
        log.error(s"Error in ${file.toString}: $e")
        false
      },
      (file, input, output) => {
        val diff = input != output
        if (diff) {
          throw new MessageOnlyException(
            s"${file.toString} isn't formatted properly!")
        }
        diff
      }
    ).flatten.forall(x => x)
    PlatformTokenizerCache.megaCache.clear()
    res
  }

  private lazy val sbtSources = thisProject.map(
    proj => {
      val rootSbt =
        BuildPaths.configurationSources(proj.base).filterNot(_.isHidden)
      val projectSbt =
        (BuildPaths.projectStandard(proj.base) * GlobFilter("*.sbt")).get
          .filterNot(_.isHidden)
      rootSbt ++ projectSbt
    }
  )
  private lazy val projectSources = thisProject.map(proj =>
    (BuildPaths.projectStandard(proj.base) * GlobFilter("*.scala")).get)

  lazy val scalafmtConfigSettings: Seq[Def.Setting[_]] = Seq(
    scalafmt := formatSources(
      (unmanagedSources in scalafmt).value.filter(filterScala),
      scalaConfig.value,
      streams.value.log,
      streams.value.cacheDirectory / "scalafmt"),
    scalafmtSbt := {
      formatSources(
        sbtSources.value,
        sbtConfig.value,
        streams.value.log,
        streams.value.cacheDirectory / "scalafmt")
      formatSources(
        projectSources.value,
        scalaConfig.value,
        streams.value.log,
        streams.value.cacheDirectory / "scalafmt")
    },
    scalafmtCheck :=
      checkSources(
        (unmanagedSources in scalafmt).value.filter(filterScala),
        scalaConfig.value,
        streams.value.log),
    scalafmtSbtCheck := {
      checkSources(sbtSources.value, sbtConfig.value, streams.value.log)
      checkSources(projectSources.value, scalaConfig.value, streams.value.log)
    },
    scalafmtDoFormatOnCompile := Def.settingDyn {
      if (scalafmtOnCompile.value) {
        scalafmt in resolvedScoped.value.scope
      } else {
        Def.task(())
      }
    }.value,
    compileInputs in compile := (compileInputs in compile)
      .dependsOn(scalafmtDoFormatOnCompile)
      .value,
    scalafmtOnly := {
      val files = spaceDelimited("<files>").parsed
      val absFiles = files.flatMap(fileS => {
        Try { IO.resolve(baseDirectory.value, new File(fileS)) } match {
          case Failure(e) =>
            streams.value.log.error(s"Error with $fileS file: $e")
            None
          case Success(file) => Some(file)
        }
      })

      val scalaFiles = absFiles.filter(filterScala)
      formatSources(scalaFiles, scalaConfig.value, streams.value.log)
      val sbtFiles = absFiles.filter(filterSbt)
      formatSources(sbtFiles, sbtConfig.value, streams.value.log)
    }
  )

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(Compile, Test).flatMap(inConfig(_)(scalafmtConfigSettings))

  override def buildSettings: Seq[Def.Setting[_]] = Seq(
    scalafmtConfig := {
      val path = (baseDirectory in ThisBuild).value / ".scalafmt.conf"
      if (path.exists()) {
        Some(path)
      } else {
        None
      }
    }
  )

  override def globalSettings: Seq[Def.Setting[_]] = Seq(
    scalafmtOnCompile := false
  )
}
