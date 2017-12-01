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

object ScalafmtPlugin extends AutoPlugin {
  override def trigger: PluginTrigger = allRequirements

  object autoImport {
    val scalafmt = taskKey[Unit]("Format Scala sources")
    val scalafmtCheck =
      taskKey[Boolean]("Check that Scala sources is formatted properly")
    val scalafmtOnCompile =
      settingKey[Boolean]("Format source when compiling")
    val scalafmtConfig = taskKey[File]("Scalafmt config file")
    val scalafmtSbt = taskKey[Unit]("Format SBT sources")
    val scalafmtSbtCheck =
      taskKey[Boolean]("Check that SBT sources is formatted properly")
    val scalafmtOnly = inputKey[Unit]("Format only given file")
  }
  import autoImport._

  private val scalaConfig = scalafmtConfig.map(Config.fromHoconFile(_) match {
    case Configured.Ok(conf) => conf
    case Configured.NotOk(e) => throw new MessageOnlyException(e.msg)
  })
  private val sbtConfig = scalaConfig.map(_.forSbt)

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
      (unmanagedSources in scalafmt).value,
      scalaConfig.value,
      streams.value.log),
    scalafmtSbt := {
      formatSources(sbtSources.value, sbtConfig.value, streams.value.log)
      formatSources(projectSources.value, scalaConfig.value, streams.value.log)
    },
    scalafmtCheck :=
      checkSources(
        (unmanagedSources in scalafmt).value,
        scalaConfig.value,
        streams.value.log),
    scalafmtSbtCheck := {
      checkSources(sbtSources.value, sbtConfig.value, streams.value.log)
      checkSources(projectSources.value, scalaConfig.value, streams.value.log)
    },
    compileInputs in compile := Def.taskDyn {
      val task =
        if (scalafmtOnCompile.value) scalafmt in resolvedScoped.value.scope
        else Def.task(())
      val previousInputs = (compileInputs in compile).value
      task.map(_ => previousInputs)
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
    Seq(Compile, Test).flatMap(inConfig(_)(scalafmtConfigSettings))

  override def buildSettings: Seq[Def.Setting[_]] = Seq(
    scalafmtConfig := (baseDirectory in ThisBuild).value / ".scalafmt.conf"
  )

  override def globalSettings: Seq[Def.Setting[_]] = Seq(
    scalafmtOnCompile := false
  )
}
