// scalafmt: { maxColumn = 120 }

import sbt.*
import sbt.Keys.*

import scala.scalanative.build.Mode
import scala.scalanative.sbtplugin.ScalaNativePlugin.autoImport.*

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*
import org.scalajs.linker.interface.{ESVersion, ModuleKind}
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.*

import sbtcrossproject.CrossPlugin.autoImport.*
import sbtcrossproject.CrossProject
import scalajscrossproject.ScalaJSCrossPlugin.autoImport.*
import scalanativecrossproject.ScalaNativeCrossPlugin.autoImport.*

object Extensions {

  import Dependencies.*

  val allPlatforms = Seq(JVMPlatform, NativePlatform, JSPlatform)
  val jvmAndNative = Seq(JVMPlatform, NativePlatform)

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

  implicit class CrossProjectBuilderExtensions(private val self: CrossProject.Builder) extends AnyVal {
    def apply(name: String): CrossProject = self.withoutSuffixFor(JVMPlatform).in(file(name))
  }

  // A crossProject declares its platforms up front, so the per-platform methods only carry
  // settings and the whole-set ones have nothing to do. Both become real once each row is
  // declared separately.
  implicit class CrossProjectExtensions(private val self: CrossProject) extends AnyVal {

    def crossJvm(ss: Def.SettingsDefinition*): CrossProject = self.jvmSettings(ss *)

    // JSPlatform already enables the plugin; naming it is how the row asks for it under a matrix
    def crossJs(ss: Def.SettingsDefinition*): CrossProject = self.jsEnablePlugins(ScalaJSPlugin).jsSettings(ss *)

    def crossNative(ss: Def.SettingsDefinition*): CrossProject = self.nativeSettings(ss *)

    // a JVM row carrying no Scala version, for Java-only sources
    def crossJvmJava(ss: Def.SettingsDefinition*): CrossProject = self.jvmSettings(ss *)

    def crossAll: CrossProject = self.crossJvm().crossJs().crossNative()

    def crossJsNative: CrossProject = self.crossJs().crossNative()

    def crossJvmNative(nativeOnly: Def.SettingsDefinition*): CrossProject = self.crossJvm().crossNative(nativeOnly *)

    def communityTest: CrossProject = self.settings(communityTestsSettings).crossJvmNative(scalaNativeConfig)
  }

}
