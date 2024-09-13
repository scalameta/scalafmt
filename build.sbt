import Dependencies._
import sbtcrossproject.CrossPlugin.autoImport.crossProject

def parseTagVersion: String = {
  import scala.sys.process._
  // drop `v` prefix
  "git describe --abbrev=0 --tags".!!.drop(1).trim
}
def localSnapshotVersion: String = s"$parseTagVersion-SNAPSHOT"
def isCI = System.getenv("CI") != null

def scala212 = "2.12.19"
def scala213 = "2.13.13"

inThisBuild(List(
  version ~= { dynVer =>
    if (isCI) dynVer else localSnapshotVersion // only for local publishing
  },
  organization := "org.scalameta",
  homepage := Some(url("https://github.com/scalameta/scalafmt")),
  licenses :=
    List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  developers := List(Developer(
    "olafurpg",
    "Ólafur Páll Geirsson",
    "olafurpg@gmail.com",
    url("https://geirsson.com"),
  )),
  scalaVersion := scala213,
  crossScalaVersions := List(scala213, scala212),
  resolvers ++= Resolver.sonatypeOssRepos("releases"),
  resolvers ++= Resolver.sonatypeOssRepos("snapshots"),
  libraryDependencies ++=
    List(munit.value % Test, scalacheck % Test, scalametaTestkit % Test),
  testFrameworks += new TestFramework("munit.Framework"),
))

name := "scalafmtRoot"
publish / skip := true

addCommandAlias("native-image", "cli/nativeImage")

commands += Command.command("ci-test") { s =>
  val scalaVersion = sys.env.get("TEST") match {
    case Some("2.12") => scala212
    case _ => scala213
  }
  val docsTest = if (scalaVersion == scala212) "docs/run" else "version"
  s"++$scalaVersion" :: "tests/test" :: "dynamic/test" :: "publishLocal" ::
    docsTest :: s
}

lazy val dynamic = project.in(file("scalafmt-dynamic")).settings(
  moduleName := "scalafmt-dynamic",
  description := "Implementation of scalafmt-interfaces",
  buildInfoSettings,
  buildInfoPackage := "org.scalafmt.dynamic",
  buildInfoObject := "BuildInfo",
  libraryDependencies ++= List(
    "io.get-coursier" % "interface" % "0.0.17",
    "com.typesafe" % "config" % "1.4.3",
    munit.value % Test,
    scalametaTestkit % Test,
  ),
  scalacOptions ++= scalacJvmOptions.value,
).dependsOn(interfaces, sysops.jvm).dependsOn(core.jvm % "test")
  .enablePlugins(BuildInfoPlugin)

lazy val interfaces = project.in(file("scalafmt-interfaces")).settings(
  moduleName := "scalafmt-interfaces",
  description :=
    "Dependency-free, pure Java public interfaces to integrate with Scalafmt through a build tool or editor plugin.",
  crossVersion := CrossVersion.disabled,
  autoScalaLibrary := false,
  Compile / resourceGenerators += Def.task {
    val out = (Compile / managedResourceDirectories).value.head /
      "scalafmt.properties"
    val props = new java.util.Properties()
    props.put("version", version.value)
    IO.write(props, "scalafmt properties", out)
    List(out)
  },
)

lazy val sysops = crossProject(JVMPlatform).withoutSuffixFor(JVMPlatform)
  .in(file("scalafmt-sysops")).settings(
    moduleName := "scalafmt-sysops",
    description := "Scalafmt systems operations",
    scalacOptions ++= scalacJvmOptions.value,
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 12)) =>
          Seq("com.github.bigwheel" %% "util-backports" % "2.1")
        case Some((2, 13)) =>
          Seq("org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4")
        case _ => Seq()
      }
    },
  )

lazy val config = crossProject(JVMPlatform).withoutSuffixFor(JVMPlatform)
  .in(file("scalafmt-config")).settings(
    moduleName := "scalafmt-config",
    description := "Scalafmt config parsing",
    scalacOptions ++= scalacJvmOptions.value,
    libraryDependencies ++= Seq(metaconfig.value),
  ).jvmSettings(libraryDependencies ++= Seq(metaconfigTypesafe.value))
// .jsSettings(
//   libraryDependencies ++= Seq(
//     metaconfigHocon.value,
//   )
// )

lazy val core = crossProject(JVMPlatform).in(file("scalafmt-core")).settings(
  moduleName := "scalafmt-core",
  buildInfoSettings,
  scalacOptions ++= scalacJvmOptions.value,
  libraryDependencies ++= Seq(
    scalameta.value,
    "org.scalameta" %% "mdoc-parser" % mdocV,
    // scala-reflect is an undeclared dependency of fansi, see #1252.
    // Scalafmt itself does not require scala-reflect.
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  ),
  libraryDependencies ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 13)) => Seq()
      case _ => Seq(compilerPlugin(
          "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full,
        ))
    }
  },
)
  // .jsSettings(
  //   libraryDependencies ++= List(
  //     scalatest.value % Test // must be here for coreJS/test to run anything
  //   )
  // )
  .jvmSettings(Test / run / fork := true).dependsOn(sysops, config)
  .enablePlugins(BuildInfoPlugin)
lazy val coreJVM = core.jvm
// lazy val coreJS = core.js
import sbtassembly.AssemblyPlugin.defaultUniversalScript

val scalacJvmOptions = Def.setting {
  val cross = CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 13)) =>
      Seq("-Ymacro-annotations", "-Xfatal-warnings", "-deprecation:false")
    case _ => Seq.empty
  }

  val unused = Seq("imports", "privates", "locals", "patvars", "implicits")
    .map(x => s"-Ywarn-unused:$x")

  cross ++ unused
}

lazy val cli = project.in(file("scalafmt-cli")).settings(
  moduleName := "scalafmt-cli",
  assembly / mainClass := Some("org.scalafmt.cli.Cli"),
  assembly / assemblyOption := (assembly / assemblyOption).value
    .withPrependShellScript(Some(defaultUniversalScript(shebang = false))),
  assembly / assemblyJarName := "scalafmt.jar",
  assembly / assemblyMergeStrategy := {
    case "reflect.properties" => MergeStrategy.first
    case x =>
      val oldStrategy = (assembly / assemblyMergeStrategy).value
      oldStrategy(x)
  },
  libraryDependencies ++= Seq(
    "com.googlecode.java-diff-utils" % "diffutils" % "1.3.0",
    "com.martiansoftware" % "nailgun-server" % "0.9.1",
    "com.github.scopt" %% "scopt" % "4.1.0",
  ),
  scalacOptions ++= scalacJvmOptions.value,
  Compile / mainClass := Some("org.scalafmt.cli.Cli"),
  nativeImageVersion := "22.3.0",
  nativeImageInstalled := isCI,
  nativeImageOptions ++= {
    val isMusl = sys.env.get("NATIVE_IMAGE_MUSL").exists(_.toBoolean)
    if (isMusl) Seq("--libc=musl") else Nil
  },
  nativeImageOptions ++= {
    val isStatic = sys.env.get("NATIVE_IMAGE_STATIC").exists(_.toBoolean)
    if (isStatic) Seq("--static") else Nil
  },
).dependsOn(coreJVM, dynamic).enablePlugins(NativeImagePlugin)

lazy val tests = project.in(file("scalafmt-tests")).settings(
  publish / skip := true,
  libraryDependencies ++= Seq(
    // Test dependencies
    "com.lihaoyi" %% "scalatags" % "0.13.1",
    scalametaTestkit,
    munit.value,
  ),
  scalacOptions ++= scalacJvmOptions.value,
  javaOptions += "-Dfile.encoding=UTF8",
  buildInfoPackage := "org.scalafmt.tests",
  buildInfoKeys :=
    Seq[BuildInfoKey]("resourceDirectory" -> (Test / resourceDirectory).value),
).enablePlugins(BuildInfoPlugin).dependsOn(coreJVM, dynamic, cli)

lazy val benchmarks = project.in(file("scalafmt-benchmarks")).settings(
  publish / skip := true,
  moduleName := "scalafmt-benchmarks",
  libraryDependencies ++= Seq(scalametaTestkit),
  run / javaOptions ++= Seq(
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
    "-server",
  ),
).dependsOn(coreJVM).enablePlugins(JmhPlugin)

lazy val docs = project.in(file("scalafmt-docs")).settings(
  crossScalaVersions := List(scala212),
  publish / skip := true,
  mdoc := (Compile / run).evaluated,
).dependsOn(cli, dynamic).enablePlugins(DocusaurusPlugin)

val V = "\\d+\\.\\d+\\.\\d+"
val ReleaseCandidate = s"($V-RC\\d+).*".r
val Milestone = s"($V-M\\d+).*".r

lazy val stableVersion = Def
  .setting((ThisBuild / version).value.replaceAll("\\+.*", ""))

lazy val buildInfoSettings: Seq[Def.Setting[_]] = Seq(
  buildInfoKeys := Seq[BuildInfoKey](
    name,
    version,
    "scalameta" -> scalametaV,
    "nightly" -> version.value,
    "stable" -> stableVersion.value,
    "previousStable" ->
      previousStableVersion.value.getOrElse(stableVersion.value),
    "scala" -> scalaVersion.value,
    "scala212" -> scala212,
    "coursier" -> coursier,
    "commit" -> sys.process.Process("git rev-parse HEAD").lineStream_!.head,
    "timestamp" -> System.currentTimeMillis().toString,
    scalaVersion,
    sbtVersion,
  ),
  buildInfoPackage := "org.scalafmt",
  buildInfoObject := "Versions",
)
