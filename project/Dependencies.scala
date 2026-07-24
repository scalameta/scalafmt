import sbt.*
import sbt.Keys.*

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport.*

// scalafmt: { maxColumn = 120, align.preset = more, align.allowOverflow = true }

object Dependencies {
  val metaconfigV = "0.18.7"
  val scalametaV  = "4.17.3"
  val coursier    = "2.1.24"
  val munitV      = "1.3.4"
  val mdocV       = mdoc.BuildInfo.version

  val smorgN = "org.scalameta"
  val smpkgN = "scalameta"

  private def smorg(pkg: => String, v: String) = Def.setting(smorgN %%% pkg % v)

  val munit     = smorg("munit", munitV)
  val scalameta = Def.setting {
    val sm = smorg(smpkgN, scalametaV).value
    sm.excludeAll("com.thesamet.scalapb" % s"scalapb-runtime_${scalaBinaryVersion.value}")
  }
  val scalametaIO      = smorg("io", scalametaV)
  val scalametaTestkit = smorg("testkit", scalametaV)

  private def metaconfig(pkg: String) = smorg(s"metaconfig-$pkg", metaconfigV)
  val metaconfigCore                  = metaconfig("core")
  val metaconfigTypesafe              = metaconfig("typesafe-config")
  val metaconfigSconfig               = metaconfig("sconfig")

}
