import sbt.Keys._
import sbt._

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

// scalafmt: { maxColumn = 100, align.preset = more, align.allowOverflow = true }

object Dependencies {
  val metaconfigV = "0.13.0"
  val scalametaV  = "4.9.9"
  val scalacheckV = "1.18.0"
  val coursier    = "2.1.10"
  val munitV      = "1.0.2"
  val mdocV       = "2.6.1"

  val scalapb = Def.setting {
    ExclusionRule(
      organization = "com.thesamet.scalapb",
      name = s"scalapb-runtime_${scalaBinaryVersion.value}",
    )
  }

  val scalametaTestkit = "org.scalameta" %% "testkit" % scalametaV

  val scalacheck = "org.scalacheck" %% "scalacheck" % scalacheckV
  val munit      = Def.setting("org.scalameta" %%% "munit" % munitV)
  val scalameta = Def
    .setting(("org.scalameta" %%% "scalameta" % scalametaV).excludeAll(scalapb.value))

  val metaconfig = Def.setting("org.scalameta" %%% "metaconfig-core" % metaconfigV)
  val metaconfigTypesafe = Def
    .setting("org.scalameta" %%% "metaconfig-typesafe-config" % metaconfigV)
  val metaconfigHocon = Def.setting("com.geirsson" %%% "metaconfig-hocon" % metaconfigV)

}
