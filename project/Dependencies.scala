import sbt.Keys._
import sbt._

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

// scalafmt: { maxColumn = 120, align.preset = more, align.allowOverflow = true }

object Dependencies {
  val metaconfigV = "0.18.2"
  val scalametaV  = "4.14.6"
  val scalacheckV = "1.18.1"
  val coursier    = "2.1.24"
  val munitV      = "1.2.1"
  val mdocV       = mdoc.BuildInfo.version

  private def smorg(pkg: => String, v: String) = Def.setting("org.scalameta" %%% pkg % v)

  val munit     = smorg("munit", munitV)
  val scalameta = Def.setting(
    smorg("scalameta", scalametaV).value
      .excludeAll("com.thesamet.scalapb" % s"scalapb-runtime_${scalaBinaryVersion.value}"),
  )
  val scalametaIO      = smorg("io", scalametaV)
  val scalametaTestkit = smorg("testkit", scalametaV)

  private def metaconfig(pkg: String) = smorg(s"metaconfig-$pkg", metaconfigV)
  val metaconfigCore                  = metaconfig("core")
  val metaconfigTypesafe              = metaconfig("typesafe-config")
  val metaconfigSconfig               = metaconfig("sconfig")

}
