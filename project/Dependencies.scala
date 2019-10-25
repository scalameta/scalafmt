import sbt._
import sbt.Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
// scalafmt: { maxColumn = 120, style = defaultWithAlign }

object Dependencies {
  val metaconfigV = "0.9.4"
  val scalametaV = "4.2.3"
  val scalatestV = "3.0.8"
  val scalacheckV = "1.14.2"
  val coursier = "1.0.3"

  val scalapb = Def.setting {
    ExclusionRule(
      organization = "com.thesamet.scalapb",
      name = s"scalapb-runtime_${scalaBinaryVersion.value}"
    )
  }

  val scalametaTestkit = "org.scalameta" %% "testkit" % scalametaV

  val scalacheck = "org.scalacheck" %% "scalacheck" % scalacheckV
  val scalatest = Def.setting("org.scalatest" %%% "scalatest" % scalatestV)
  val scalameta = Def.setting {
    scalaBinaryVersion.value match {
      case "2.11" =>
        // Scala 2.11.12 complains about missing scalapb symbols from the classpath:
        // [error] /home/travis/build/scalameta/scalafmt/scalafmt-core/shared/src/main/scala/org/scalafmt/Error.scala:30:9: Symbol 'term <root>.scalapb' is missing from the classpath.
        // [error] This symbol is required by 'class scala.meta.internal.semanticdb.Range'.
        // [error] Make sure that term scalapb is in your classpath and check for conflicting dependencies with `-Ylog-classpath`.
        // [error] A full rebuild may help if 'Range.class' was compiled against an incompatible version of <root>.
        // [error]         pos.formatMessage(
        // [error]         ^
        // [error] one error found
        "org.scalameta" %%% "scalameta" % scalametaV
      case _ =>
        "org.scalameta" %%% "scalameta" % scalametaV excludeAll scalapb.value
    }
  }
  val metaconfig = Def.setting("com.geirsson" %%% "metaconfig-core" % metaconfigV)
  val metaconfigTypesafe = Def.setting("com.geirsson" %%% "metaconfig-typesafe-config" % metaconfigV)
  val metaconfigHocon = Def.setting("com.geirsson" %%% "metaconfig-hocon" % metaconfigV)

}
