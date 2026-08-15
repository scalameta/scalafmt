// scalafmt: { maxColumn = 120 }

import sbt.*
import sbt.Keys.*

import scala.scalanative.build.Mode
import scala.scalanative.sbtplugin.ScalaNativePlugin.autoImport.*

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import org.scalajs.linker.interface.{ESVersion, ModuleKind}
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.*

object Extensions {

  import Dependencies.*

  def isScalaVer(ver: String) = Def.setting(scalaBinaryVersion.value == ver)
  def isScala212 = isScalaVer("2.12")
  def isScala213 = isScalaVer("2.13")
  def isScala3 = isScalaVer("3")

  val unpublished = publish / skip := true

  lazy val sharedTestSettings = Seq(libraryDependencies += munit.value % Test)

  val scalacJvmOptions = Def.setting {
    val cross = if (!isScala213.value) Nil else Seq("-Ymacro-annotations")

    val warningAsError =
      if (isScala212.value) Seq("-Xfatal-warnings", "-deprecation:false")
      else Seq("-Wconf:any:error,cat=deprecation:silent")

    val unused =
      if (isScala3.value) "-Wunused:all"
      else if (isScala213.value) "-Wunused:imports,privates,locals,patvars,implicits,explicits,params"
      else "-Ywarn-unused:imports,privates,locals,patvars,implicits"

    val javaver = if (isScala3.value) Seq("-java-output-version:8") else Seq("-target:8", "-release:8")

    cross ++ warningAsError ++ javaver :+ unused
  }

  val scalacSettings = Def.settings(
    javacOptions ++= Seq("-source", "8", "-target", "8"),
    Compile / compile / scalacOptions ++= scalacJvmOptions.value,
    Test / compile / scalacOptions ++= scalacJvmOptions.value,
  )

  lazy val scalaJsSettings = Seq(
    // to support Node.JS functionality
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
    // to support MULTILINE in regex
    scalaJSLinkerConfig ~= (_.withESFeatures(_.withESVersion(ESVersion.ES2018))),
  )

  lazy val scalaNativeConfig = nativeConfig ~= { _.withMode(Mode.releaseFull) }

  def parallelCollections = libraryDependencies ++=
    { if (!isScala213.value) Nil else Seq("org.scala-lang.modules" %%% "scala-parallel-collections" % "1.2.0") }

  lazy val communityTestsSettings: Seq[Def.Setting[_]] = Def
    .settings(unpublished, scalacSettings, sharedTestSettings, javaOptions += "-Dfile.encoding=UTF8")

}
