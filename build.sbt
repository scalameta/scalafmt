import Dependencies._
import sbtcrossproject.CrossPlugin.autoImport.crossProject

def parseTagVersion: String = {
  import scala.sys.process._
  // drop `v` prefix
  "git describe --abbrev=0 --tags".!!.drop(1).trim
}
def localSnapshotVersion: String = s"$parseTagVersion-SNAPSHOT"
def isCI = System.getenv("CI") != null

def scala211 = "2.11.12"
def scala212 = "2.12.12"
def scala213 = "2.13.3"

inThisBuild(
  List(
    version ~= { dynVer =>
      if (isCI) dynVer
      else localSnapshotVersion // only for local publishng
    },
    organization := "org.scalameta",
    homepage := Some(url("https://github.com/scalameta/scalafmt")),
    licenses := List(
      "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
    ),
    developers := List(
      Developer(
        "olafurpg",
        "Ólafur Páll Geirsson",
        "olafurpg@gmail.com",
        url("https://geirsson.com")
      )
    ),
    scalaVersion := scala213,
    crossScalaVersions := List(scala213, scala212, scala211),
    resolvers += Resolver.sonatypeRepo("releases"),
    libraryDependencies ++= List(
      scalatest.value % Test,
      scalacheck % Test,
      scalametaTestkit % Test
    )
  )
)

name := "scalafmtRoot"
skip in publish := true

addCommandAlias("native-image", "cli/graalvm-native-image:packageBin")

commands += Command.command("ci-test") { s =>
  val scalaVersion = sys.env.get("TEST") match {
    case Some("2.11") => scala211
    case Some("2.12") => scala212
    case _ => scala213
  }
  val docsTest = if (scalaVersion == scala212) "docs/run" else "version"
  s"++$scalaVersion" ::
    "tests/test" ::
    "publishLocal" ::
    docsTest ::
    s
}

lazy val dynamic = project
  .in(file("scalafmt-dynamic"))
  .settings(
    moduleName := "scalafmt-dynamic",
    description := "Implementation of scalafmt-interfaces",
    buildInfoSettings,
    buildInfoPackage := "org.scalafmt.dynamic",
    buildInfoObject := "BuildInfo",
    libraryDependencies ++= List(
      "io.get-coursier" % "interface" % "0.0.17",
      "com.typesafe" % "config" % "1.4.1"
    ),
    scalacOptions ++= scalacJvmOptions.value
  )
  .dependsOn(interfaces)
  .enablePlugins(BuildInfoPlugin)

lazy val interfaces = project
  .in(file("scalafmt-interfaces"))
  .settings(
    moduleName := "scalafmt-interfaces",
    description := "Dependency-free, pure Java public interfaces to integrate with Scalafmt through a build tool or editor plugin.",
    crossVersion := CrossVersion.disabled,
    autoScalaLibrary := false,
    resourceGenerators.in(Compile) += Def.task {
      val out =
        managedResourceDirectories
          .in(Compile)
          .value
          .head / "scalafmt.properties"
      val props = new java.util.Properties()
      props.put("version", version.value)
      IO.write(props, "scalafmt properties", out)
      List(out)
    }
  )

lazy val core = crossProject(JVMPlatform)
  .in(file("scalafmt-core"))
  .settings(
    moduleName := "scalafmt-core",
    buildInfoSettings,
    scalacOptions ++= scalacJvmOptions.value,
    libraryDependencies ++= Seq(
      metaconfig.value,
      scalameta.value,
      // scala-reflect is an undeclared dependency of fansi, see #1252.
      // Scalafmt itself does not require scala-reflect.
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    ),
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 13)) =>
          Seq(
            "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.0"
          )
        case _ =>
          Seq(
            compilerPlugin(
              "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full
            )
          )
      }
    }
  )
  // .jsSettings(
  //   libraryDependencies ++= List(
  //     metaconfigHocon.value,
  //     scalatest.value % Test // must be here for coreJS/test to run anything
  //   )
  // )
  .jvmSettings(
    fork.in(run).in(Test) := true,
    libraryDependencies ++= List(
      metaconfigTypesafe.value
    )
  )
  .enablePlugins(BuildInfoPlugin)
lazy val coreJVM = core.jvm
// lazy val coreJS = core.js

import sbtassembly.AssemblyPlugin.defaultUniversalScript

val scalacJvmOptions = Def.setting {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 11)) => Seq("-target:jvm-1.8")
    case Some((2, 13)) =>
      Seq(
        "-Ymacro-annotations",
        "-Xfatal-warnings",
        "-Ywarn-unused:imports",
        "-deprecation:false"
      )
    case _ => Seq.empty
  }
}

lazy val cli = project
  .in(file("scalafmt-cli"))
  .settings(
    moduleName := "scalafmt-cli",
    mainClass in assembly := Some("org.scalafmt.cli.Cli"),
    assemblyOption in assembly := (assemblyOption in assembly).value
      .copy(prependShellScript = Some(defaultUniversalScript(shebang = false))),
    assemblyJarName.in(assembly) := "scalafmt.jar",
    assemblyMergeStrategy in assembly := {
      case "reflect.properties" => MergeStrategy.first
      case x =>
        val oldStrategy = (assemblyMergeStrategy in assembly).value
        oldStrategy(x)
    },
    libraryDependencies ++= Seq(
      "com.googlecode.java-diff-utils" % "diffutils" % "1.3.0",
      "com.martiansoftware" % "nailgun-server" % "0.9.1",
      "com.github.scopt" %% "scopt" % "3.7.1",
      // undeclared transitive dependency of coursier-small
      "org.scala-lang.modules" %% "scala-xml" % "1.3.0"
    ),
    scalacOptions ++= scalacJvmOptions.value,
    mainClass in GraalVMNativeImage := Some("org.scalafmt.cli.Cli"),
    graalVMNativeImageOptions ++= {
      sys.env
        .get("NATIVE_IMAGE_MUSL")
        .map(path => s"-H:UseMuslC=$path")
        .toSeq ++
        sys.env
          .get("NATIVE_IMAGE_STATIC")
          .map(_.toBoolean)
          .filter(identity)
          .map(_ => "--static")
          .toSeq
    }
  )
  .dependsOn(coreJVM, dynamic)
  .enablePlugins(GraalVMNativeImagePlugin)

lazy val tests = project
  .in(file("scalafmt-tests"))
  .settings(
    skip in publish := true,
    libraryDependencies ++= Seq(
      // Test dependencies
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 13)) => "com.lihaoyi" %% "scalatags" % "0.9.2"
        case _ => "com.lihaoyi" %% "scalatags" % "0.6.8"
      },
      "org.typelevel" %% "paiges-core" % "0.3.0",
      scalametaTestkit,
      scalatest.value
    ),
    scalacOptions ++= scalacJvmOptions.value,
    // Fork in CI to hit on memory limitation issues. Disable forking locally
    // because ScalaTest error reporting fails with serialization errors when
    // forking is disabled.
    fork := isCI
  )
  .dependsOn(coreJVM, dynamic, cli)

lazy val benchmarks = project
  .in(file("scalafmt-benchmarks"))
  .settings(
    skip in publish := true,
    moduleName := "scalafmt-benchmarks",
    libraryDependencies ++= Seq(
      scalametaTestkit
    ),
    javaOptions in run ++= Seq(
      "-Djava.net.preferIPv4Stack=true",
      "-XX:+AggressiveOpts",
      "-XX:+UseParNewGC",
      "-XX:+UseConcMarkSweepGC",
      "-XX:+CMSParallelRemarkEnabled",
      "-XX:+CMSClassUnloadingEnabled",
      "-XX:ReservedCodeCacheSize=128m",
      "-XX:MaxMetaspaceSize=1024m",
      "-XX:SurvivorRatio=128",
      "-XX:MaxTenuringThreshold=0",
      "-Xss8M",
      "-Xms512M",
      "-Xmx2G",
      "-server"
    )
  )
  .dependsOn(coreJVM)
  .enablePlugins(JmhPlugin)

lazy val docs = project
  .in(file("scalafmt-docs"))
  .settings(
    crossScalaVersions := List(scala212),
    skip in publish := true,
    mdoc := run.in(Compile).evaluated
  )
  .dependsOn(cli, dynamic)
  .enablePlugins(DocusaurusPlugin)

val V = "\\d+\\.\\d+\\.\\d+"
val ReleaseCandidate = s"($V-RC\\d+).*".r
val Milestone = s"($V-M\\d+).*".r

lazy val stableVersion = Def.setting {
  version.in(ThisBuild).value.replaceAll("\\+.*", "")
}

lazy val buildInfoSettings: Seq[Def.Setting[_]] = Seq(
  buildInfoKeys := Seq[BuildInfoKey](
    name,
    version,
    "scalameta" -> scalametaV,
    "nightly" -> version.value,
    "stable" -> stableVersion.value,
    "scala" -> scalaVersion.value,
    "scala211" -> scala211,
    "scala212" -> scala212,
    "coursier" -> coursier,
    "commit" -> sys.process.Process("git rev-parse HEAD").lineStream_!.head,
    "timestamp" -> System.currentTimeMillis().toString,
    scalaVersion,
    sbtVersion
  ),
  buildInfoPackage := "org.scalafmt",
  buildInfoObject := "Versions"
)
