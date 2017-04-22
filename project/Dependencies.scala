import sbt._
import sbt.Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
// scalafmt: { maxColumn = 120, style = defaultWithAlign }

object Dependencies {
  val metaconfigV = "0.3.0"
  val scalametaV  = "1.7.0"
  val paradiseV   = "3.0.0-M8"
  val scalatestV  = "3.0.1"
  val coursier    = "1.0.0-M15-5"

  val paradise         = "org.scalameta"   % "paradise"     % paradiseV cross CrossVersion.full
  val scalametaTestkit = "org.scalameta"   %% "testkit"     % scalametaV
  val scalatest        = "org.scalatest"   %% "scalatest"   % scalatestV
  val scalariform      = "org.scalariform" %% "scalariform" % "0.1.8"

  val scalameta  = Def.setting("org.scalameta" %%% "scalameta"                  % scalametaV)
  val metaconfig = Def.setting("com.geirsson"  %%% "metaconfig-typesafe-config" % metaconfigV)
}
