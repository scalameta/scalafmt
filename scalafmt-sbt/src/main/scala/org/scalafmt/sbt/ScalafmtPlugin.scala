package org.scalafmt.sbt

import org.scalafmt.{Formatted, Scalafmt}
import org.scalafmt.config.{Config, ScalafmtConfig, ScalafmtRunner}
import sbt.Keys._
import sbt.{Def, _}
import complete.DefaultParsers._
import sbt.util.Logger

import scala.util.{Failure, Success, Try}

object ScalafmtPlugin extends AutoPlugin {
  override def trigger: PluginTrigger = allRequirements

  object autoImport {
    lazy val scalafmt = TaskKey[Unit]("scalafmt", "Format Scala sources")
    lazy val scalafmtCheck = TaskKey[Boolean](
      "scalafmt-check",
      "Check that Scala sources is formatted properly")
    lazy val scalafmtOnCompile =
      SettingKey[Boolean]("scalafmt-on-compile", "Format source when compiling")
    lazy val scalafmtConfig =
      TaskKey[File]("scalafmt-config", "Scalafmtter config file")
    lazy val scalafmtSbt = TaskKey[Unit]("scalafmt-sbt", "Format SBT sources")
    lazy val scalafmtSbtCheck = TaskKey[Boolean](
      "scalafmtSbtCheck",
      "Check that SBT sources is formatted properly")
    lazy val scalafmtOnly =
      InputKey[Unit]("scalafmt-only", "Format only given file")
  }
  import autoImport._

  private lazy val scalaConfig =
    scalafmtConfig.map(Config.fromHoconFile(_).get)
  private lazy val sbtConfig =
    scalaConfig.map(conf => conf.copy(runner = conf.runner.forSbt))

  private type Input = String
  private type Output = String

  private def withFormattedSources[T](
      sources: Seq[File],
      config: ScalafmtConfig
  )(
      onError: (File, Throwable) => T,
      onFormat: (File, Input, Output) => T
  ): Seq[Option[T]] = {
    sources.map(
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
      log: Logger
  ): Unit = {
    withFormattedSources(sources, config)(
      (file, e) => log.error(s"Error in ${file.toString}: $e"),
      (file, input, output) => {
        if (input != output) {
          log.info(s"Successfully formatted ${file.toString}.")
          IO.write(file, output)
        }
      }
    )
  }

  private def checkSources(
      sources: Seq[File],
      config: ScalafmtConfig,
      log: Logger
  ): Boolean = {
    withFormattedSources(sources, config)(
      (file, e) => {
        log.error(s"Error in ${file.toString}: $e")
        false
      },
      (file, input, output) => {
        val diff = input != output
        if (diff) {
          log.error(s"${file.toString} isn't formatted properly!")
        }
        diff
      }
    ).flatten.forall(x => x)
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

  lazy val scalafmtSettings: Seq[Def.Setting[_]] = Seq(
    scalafmtOnCompile := false,
    scalafmt := formatSources(
      unmanagedSources.value,
      scalaConfig.value,
      streams.value.log),
    scalafmtSbt := {
      formatSources(sbtSources.value, sbtConfig.value, streams.value.log)
      formatSources(projectSources.value, scalaConfig.value, streams.value.log)
    },
    scalafmtCheck :=
      checkSources(
        unmanagedSources.value,
        scalaConfig.value,
        streams.value.log),
    scalafmtSbtCheck := {
      checkSources(sbtSources.value, sbtConfig.value, streams.value.log)
      checkSources(projectSources.value, scalaConfig.value, streams.value.log)
    },
    compile := Def.taskDyn {
      val defaultCompile = compile.taskValue
      if (scalafmtOnCompile.value) {
        scalafmt.value
      }
      Def.task(defaultCompile.value)
    }.value,
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

      val scalaFiles = absFiles.filter(_.toString.endsWith(".scala"))
      formatSources(scalaFiles, scalaConfig.value, streams.value.log)
      val sbtFiles = absFiles.filter(_.toString.endsWith(".sbt"))
      formatSources(sbtFiles, sbtConfig.value, streams.value.log)
    }
  )

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(Compile, Test).flatMap(inConfig(_)(scalafmtSettings))

  override def buildSettings: Seq[Def.Setting[_]] = Seq(
    scalafmtConfig := (baseDirectory in ThisBuild).value / ".scalafmt.conf"
  )
}
