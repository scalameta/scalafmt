package org.scalafmt.sbt

import sbt.Keys._
import sbt._
import sbt.{IntegrationTest => It}

// This class is abstract since reformatOnCompile is not maintained.
// The easiest way to use this in your build is to copy-paste it into
// project/Scalafmt.scala and replace `abstract class` with `object`.
// Warning. It may be that formatting is slow if scalafmt needs to be JIT-ed
// on every compile invocation. I don't know enough about sbt to tell if
// the classloader is cached between each runMain.
//
// If you like this feature, please consider becoming a maintainer!
abstract class UnmaintainedScalafmtReformatOnCompilePlugin extends AutoPlugin {
  override def requires: Plugins = ScalafmtPlugin
  override def trigger: PluginTrigger = allRequirements
  object autoImport {
    lazy val scalafmtIncremental: TaskKey[Unit] =
      taskKey[Unit]("Reformat on compile")
    def scalafmtReformatOnCompile: Seq[Def.Setting[_]] = Seq(
      compileInputs.in(Compile, compile) := {
        scalafmtIncremental.in(Compile).value
        compileInputs.in(Compile, compile).value
      },
      compileInputs.in(Test, compile) := {
        scalafmtIncremental.in(Test).value
        compileInputs.in(Test, compile).value
      }
    )

    def scalafmtReformatOnCompileWithIt: Seq[Def.Setting[_]] =
      scalafmtReformatOnCompile :+
        (compileInputs.in(It, compile) := {
          scalafmtIncremental.in(It).value
          compileInputs.in(It, compile).value
        })
  }
  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] =
    inConfig(Compile)(incrementalReformatSettings) ++
      inConfig(Test)(incrementalReformatSettings) ++
      scalafmtReformatOnCompile

  def incrementalReformatSettings: Seq[Def.Setting[_]] = Seq(
    (sourceDirectories in scalafmtIncremental) := unmanagedSourceDirectories.value,
    includeFilter in scalafmtIncremental := "*.scala",
    scalafmtIncremental := Def.taskDyn {
      val cache = streams.value.cacheDirectory / "scalafmt"
      val include = (includeFilter in scalafmtIncremental).value
      val exclude = (excludeFilter in scalafmtIncremental).value
      val files: Set[File] =
        (sourceDirectories in scalafmtIncremental).value
          .descendantsExcept(include, exclude)
          .get
          .toSet
      val label = Reference.display(thisProjectRef.value)
      def handleUpdate(in: ChangeReport[File],
                       out: ChangeReport[File]): Set[File] = {
        val files = in.modified -- in.removed
        import sbt._
        inc.Analysis
          .counted("Scala source", "", "s", files.size)
          .foreach(count =>
            streams.value.log.info(s"Formatting $count $label..."))
        files
      }
      val toFormat = FileFunction.cached(cache)(
        FilesInfo.hash,
        FilesInfo.exists
      )(handleUpdate)(files)
      val filesFlag = toFormat.map(_.getAbsolutePath).mkString(",")
      val args = Seq("", "org.scalafmt.cli.Cli", "-i", "-f", filesFlag)
      (runMain in ScalafmtPlugin.scalafmtStub).toTask(args.mkString(" "))
    }.value
  )
}
