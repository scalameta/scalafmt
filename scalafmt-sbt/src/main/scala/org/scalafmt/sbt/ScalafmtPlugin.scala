package org.scalafmt.sbt

import org.scalafmt.{Formatted, Scalafmt}
import org.scalafmt.config.{Config, ScalafmtConfig, ScalafmtRunner}
import sbt.Keys._
import sbt._
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
      TaskKey[Option[File]](
        "scalafmt-config",
        "Scalafmtter config file for *.scala files")
    lazy val scalafmtSbtConfig =
      TaskKey[Option[File]](
        "scalafmt-sbt-config",
        "Scalafmtter config file for *.sbt files")
    lazy val scalafmtSbt = TaskKey[Unit]("scalafmt-sbt", "Format SBT sources")
    lazy val scalafmtSbtCheck = TaskKey[Boolean](
      "scalafmtSbtCheck",
      "Check that SBT sources is formatted properly")
    lazy val scalafmtOnly =
      InputKey[Unit]("scalafmt-only", "Format only given file")
  }
  import autoImport._

  private def configFromFileOrElse(
      file: Option[File],
      elseConfig: ScalafmtConfig) =
    file
      .map(Config.fromHoconFile(_).getOrElse(elseConfig))
      .getOrElse(elseConfig)

  private val scalaConfig =
    scalafmtConfig.map(configFromFileOrElse(_, ScalafmtConfig.default))
  private val sbtConfig =
    scalafmtSbtConfig.map(
      configFromFileOrElse(
        _,
        ScalafmtConfig.default.copy(runner = ScalafmtRunner.sbt))
    )

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
      (_, input, output) => input != output
    ).flatten.forall(x => x)
  }

  private val sbtSources = thisProject.map(
    proj =>
      BuildPaths
        .configurationSources(proj.base)
        .filterNot(_.isHidden)
  )
  private lazy val projectSources = thisProject.map(proj =>
    (BuildPaths.projectStandard(proj.base) * GlobFilter("*.scala")).get)

  lazy val scalafmtSettings: Seq[Def.Setting[_]] = Seq(
    scalafmtConfig := None,
    scalafmtSbtConfig := None,
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
      if (scalafmtOnCompile.value) {
        scalafmt in resolvedScoped.value.scope
      }
      compile
    }.value,
    scalafmtOnly := {
      val files = spaceDelimited("<files>").parsed
      val absFiles = files.flatMap(fileS => {
        Try { new File(fileS) } match {
          case Failure(e) =>
            streams.value.log.error(s"Error with $fileS file: $e")
            None
          case Success(file) => Some(file)
        }
      })

      val scalaFiles = absFiles.filter(f =>
        unmanagedSources.value.contains(f) || projectSources.value.contains(f))
      formatSources(scalaFiles, scalaConfig.value, streams.value.log)
      val sbtFiles = absFiles.filter(sbtSources.value.contains)
      formatSources(sbtFiles, sbtConfig.value, streams.value.log)
    }
  )

  override def globalSettings: Seq[Def.Setting[_]] = ???

}
