import sbt._
import sbt.Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
// scalafmt: { maxColumn = 120, style = defaultWithAlign }

object Dependencies {
  val metaconfigV = "0.11.1"
  val scalametaV  = "4.7.8"
  val scalacheckV = "1.17.0"
  val coursier    = "2.1.2"
  val munitV      = "0.7.29"

  val scalapb = Def.setting {
    ExclusionRule(
      organization = "com.thesamet.scalapb",
      name = s"scalapb-runtime_${scalaBinaryVersion.value}"
    )
  }

  val scalametaTestkit = "org.scalameta" %% "testkit" % scalametaV

  val scalacheck = "org.scalacheck" %% "scalacheck" % scalacheckV
  val munit      = Def.setting("org.scalameta" %%% "munit" % munitV)
  val scalameta  = Def.setting("org.scalameta" %%% "scalameta" % scalametaV excludeAll scalapb.value)

  val metaconfig         = Def.setting("com.geirsson" %%% "metaconfig-core" % metaconfigV)
  val metaconfigTypesafe = Def.setting("com.geirsson" %%% "metaconfig-typesafe-config" % metaconfigV)
  val metaconfigHocon    = Def.setting("com.geirsson" %%% "metaconfig-hocon" % metaconfigV)

}
