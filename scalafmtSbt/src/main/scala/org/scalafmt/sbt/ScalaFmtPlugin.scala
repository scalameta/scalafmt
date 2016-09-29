/* Modified version of
https://github.com/sbt/sbt-scalariform/blob/61a0b7b75441b458e4ff3c6c30ed87d087a2e569/src/main/scala/com/typesafe/sbt/Scalariform.scala

Original licence:

Copyright 2011-2012 Typesafe Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */
package org.scalafmt.sbt

import java.net.URLClassLoader

import sbt.Keys._
import sbt._
import sbt.plugins.JvmPlugin
import sbt.{IntegrationTest => It}

import scala.collection.immutable.Seq
import scala.util.Failure
import scala.util.Success

object ScalaFmtPlugin extends AutoPlugin {

  object autoImport {
    // These should in fact be commands instead of tasks.
    lazy val scalafmt: TaskKey[Unit] =
      taskKey[Unit]("Format Scala sources using scalafmt")

    lazy val scalafmtTest: TaskKey[Unit] = taskKey[Unit](
      "Test for mis-formatted Scala sources, " +
        "exits with status 1 on failure.")

    lazy val scalafmtConfig: TaskKey[Option[File]] =
      taskKey[Option[File]]("Configuration file for scalafmt.")

    lazy val scalafmtSbtFiles: TaskKey[Seq[File]] =
      taskKey[Seq[File]]("SBT files that scalafmt should format.")

    lazy val hasScalafmt: TaskKey[HasScalaFmt] = taskKey[HasScalaFmt](
      "Classloaded Scalafmt210 instance to overcome 2.10 incompatibility issues.")

    lazy val formatSbtFiles: SettingKey[Boolean] =
      settingKey[Boolean]("If true, format *.sbt files in this project.")

    def scalafmtSettings: Seq[Setting[_]] =
      noConfigScalafmtSettings ++
        inConfig(Compile)(configScalafmtSettings) ++
        inConfig(Test)(configScalafmtSettings)

    lazy val reformatOnCompileSettings: Seq[Def.Setting[_]] = List(
      compileInputs in (Compile, compile) <<=
        (compileInputs in (Compile, compile)) dependsOn (scalafmt in Compile),
      compileInputs in (Test, compile) <<=
        (compileInputs in (Test, compile)) dependsOn (scalafmt in Test)
    )

    lazy val scalafmtSettingsWithIt: Seq[Setting[_]] =
      scalafmtSettings ++
        inConfig(IntegrationTest)(configScalafmtSettings)

    lazy val reformatOnCompileWithItSettings: Seq[Def.Setting[_]] =
      reformatOnCompileSettings ++ List(
        compileInputs in (It, compile) <<=
          (compileInputs in (It, compile)) dependsOn
            (scalafmt in It)
      )
  }
  import autoImport._

  override val projectSettings = scalafmtSettings

  override def trigger = allRequirements

  override def requires = JvmPlugin

  def noConfigScalafmtSettings: Seq[Setting[_]] =
    List(
      ivyConfigurations += config("scalafmt").hide,
      libraryDependencies ++= Seq(
        // scala-library needs to be explicitly added to fix
        // https://github.com/olafurpg/scalafmt/issues/190
        "org.scala-lang" % "scala-library" % org.scalafmt.Versions.scala % "scalafmt",
        "com.geirsson" % "scalafmt-cli_2.11" % org.scalafmt.Versions.nightly % "scalafmt"
      )
    )

  def configScalafmtSettings: Seq[Setting[_]] =
    List(
      (sourceDirectories in hasScalafmt) := unmanagedSourceDirectories.value,
      includeFilter in Global in hasScalafmt := {
        if (formatSbtFiles.value) "*.scala" || "*.sbt"
        else "*.scala"
      },
      formatSbtFiles in Global := true,
      scalafmtSbtFiles := {
        scala.collection.immutable.Seq(
          (baseDirectory in hasScalafmt).value
            .descendantsExcept("*.sbt", "*.scala")
            .get: _*
        )
      },
      scalafmtConfig in Global := None,
      hasScalafmt := {
        val report = update.value
        val jars = report.select(configurationFilter("scalafmt"))

        HasScalaFmt(
          getScalafmtLike(new URLClassLoader(jars.map(_.toURI.toURL).toArray,
                                             null),
                          streams.value),
          scalafmtConfig.value,
          streams.value,
          scalafmtSbtFiles.value ++
            (sourceDirectories in hasScalafmt).value.toList,
          (includeFilter in hasScalafmt).value,
          (excludeFilter in hasScalafmt).value,
          thisProjectRef.value)
      },
      scalafmt := hasScalafmt.value.writeFormattedContentsToFiles(),
      scalafmtTest := hasScalafmt.value.testProjectIsFormatted()
    )

  private def getScalafmtLike(classLoader: URLClassLoader,
                              streams: TaskStreams): ScalaFmtLike = {
    val loadedClass =
      new ReflectiveDynamicAccess(classLoader).createInstanceFor[ScalaFmtLike](
        "org.scalafmt.cli.Scalafmt210",
        Seq.empty)

    loadedClass match {
      case Success(x) => x
      case Failure(e) =>
        streams.log.error(
          s"""Unable to classload ScalaFmt, please file an issue:
               |https://github.com/olafurpg/scalafmt/issues
               |
               |URLs: ${classLoader.getURLs.mkString("\n")}
               |Version: ${org.scalafmt.Versions.nightly}
               |Error: ${e.getClass}
               |Message: ${e.getMessage}
               |${e.getStackTrace.mkString("\n")}""".stripMargin)
        throw e
    }
  }
}
