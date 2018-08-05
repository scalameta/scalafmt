import sbt._
import sbt.Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
// scalafmt: { maxColumn = 120, style = defaultWithAlign }

object Dependencies {
  val metaconfigV = "0.8.3"
  val scalametaV = "4.0.0-M1"
  val scalatestV = "3.2.0-SNAP10"
  val scalacheckV = "1.13.5"
  val coursier = "1.0.0-RC12"

  val scalapb = Def.setting {
    ExclusionRule(
      organization = "com.thesamet.scalapb",
      name = s"scalapb-runtime_${scalaBinaryVersion.value}"
    )
  }

  val scalametaTestkit = "org.scalameta" %% "testkit" % scalametaV
  val scalariform = "org.scalariform" %% "scalariform" % "0.1.8"

  val scalacheck = "org.scalacheck" %% "scalacheck" % scalacheckV
  val scalatest = Def.setting("org.scalatest" %%% "scalatest" % scalatestV)
  val scalameta = Def.setting("org.scalameta" %%% "scalameta" % scalametaV excludeAll scalapb.value)
  val metaconfig = Def.setting("com.geirsson" %%% "metaconfig-core" % metaconfigV)
  val metaconfigTypesafe = Def.setting("com.geirsson" %%% "metaconfig-typesafe-config" % metaconfigV)
  val metaconfigHocon = Def.setting("com.geirsson" %%% "metaconfig-hocon" % metaconfigV)

}
