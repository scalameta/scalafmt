import sbt.*
import sbt.Keys.*

// scalafmt: { maxColumn = 120 }

object Dependencies {
  val scala212 = "2.12.21"
  val scala213 = "2.13.18"
  val scala3 = "3.3.8"
  val scala2Versions = Seq(scala213, scala212)
  val scalaVersions = scala2Versions :+ scala3

  val metaconfigV = "0.18.8"
  val scalametaV = "4.17.4"
  val munitV = "1.3.6"
  val mdocV = mdoc.BuildInfo.version

  private val coursier212 = "2.1.24" // coursier does not publish Scala 2.12 artifacts after 2.1.24
  private val coursier213 = "2.1.26"
  val coursier = Def.setting(if (scalaBinaryVersion.value == "2.12") coursier212 else coursier213)

  val smorgN = "org.scalameta"
  val smpkgN = "scalameta"

  private def smorg(pkg: => String, v: String) = Def.setting(smorgN %% pkg % v)

  val munit = smorg("munit", munitV)
  val scalameta = Def.setting {
    val sm = smorg(smpkgN, scalametaV).value
    sm.excludeAll("com.thesamet.scalapb" % s"scalapb-runtime_${scalaBinaryVersion.value}")
  }
  val scalametaIO = smorg("io", scalametaV)
  val scalametaTestkit = smorg("testkit", scalametaV)

  private def metaconfig(pkg: String) = smorg(s"metaconfig-$pkg", metaconfigV)
  val metaconfigCore = metaconfig("core")
  val metaconfigTypesafe = metaconfig("typesafe-config")
  val metaconfigSconfig = metaconfig("sconfig")

}
