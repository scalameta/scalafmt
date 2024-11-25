import java.nio.file.Paths

import scala.scalanative.build._

import Dependencies._
import sbtcrossproject.CrossPlugin.autoImport.crossProject

def parseTagVersion: String = {
  import scala.sys.process._
  // drop `v` prefix
  "git describe --abbrev=0 --tags".!!.drop(1).trim
}
def localSnapshotVersion: String = s"$parseTagVersion-SNAPSHOT"
def isCI = System.getenv("CI") != null

def scala212 = "2.12.20"
def scala213 = "2.13.15"

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
  testFrameworks += new TestFramework("munit.Framework"),
  // causes native image issues
  dependencyOverrides += "org.jline" % "jline" % "3.27.1",
))

name := "scalafmtRoot"
publish / skip := true

addCommandAlias("native-image", "cli/nativeImage")
addCommandAlias("scala-native", "cliNative/compile;cliNative/nativeLink")

commands += Command.command("ci-test-jvm") { s =>
  val scalaVersion = sys.env.get("TEST") match {
    case Some("2.12") => scala212
    case _ => scala213
  }
  val docsTest = if (scalaVersion == scala212) "docs/run" else "version"
  s"++$scalaVersion" :: "tests/test" :: "dynamic/test" :: "publishLocal" ::
    docsTest :: s
}

commands += Command.command("ci-test-native") { s =>
  val scalaVersion = sys.env.get("TEST") match {
    case Some("2.12") => scala212
    case _ => scala213
  }
  s"++$scalaVersion" :: "testsNative/test" :: s
}

lazy val dynamic = crossProject(JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform).in(file("scalafmt-dynamic")).settings(
    moduleName := "scalafmt-dynamic",
    description := "Implementation of scalafmt-interfaces",
    buildInfoSettings,
    buildInfoPackage := "org.scalafmt.dynamic",
    buildInfoObject := "BuildInfo",
    libraryDependencies ++= List(
      "io.get-coursier" % "interface" % "1.0.24",
      "com.typesafe" % "config" % "1.4.3",
      munit.value % Test,
      scalametaTestkit.value % Test,
    ),
    scalacOptions ++= scalacJvmOptions.value,
  ).dependsOn(interfaces, sysops).dependsOn(core % "test")
  .enablePlugins(BuildInfoPlugin)

lazy val interfaces = crossProject(JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform).in(file("scalafmt-interfaces")).settings(
    moduleName := "scalafmt-interfaces",
    description :=
      "Dependency-free, pure Java public interfaces to integrate with Scalafmt through a build tool or editor plugin.",
    Compile / resourceGenerators += Def.task {
      val out = (Compile / managedResourceDirectories).value.head /
        "scalafmt.properties"
      val props = new java.util.Properties()
      props.put("version", version.value)
      IO.write(props, "scalafmt properties", out)
      List(out)
    },
  ).jvmSettings(crossVersion := CrossVersion.disabled, autoScalaLibrary := false)

lazy val sysops = crossProject(JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform).in(file("scalafmt-sysops")).settings(
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

lazy val config = crossProject(JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform).in(file("scalafmt-config")).settings(
    moduleName := "scalafmt-config",
    description := "Scalafmt config parsing",
    scalacOptions ++= scalacJvmOptions.value,
    libraryDependencies ++= Seq(metaconfig.value),
  ).jvmSettings(libraryDependencies ++= Seq(metaconfigTypesafe.value))
  .nativeSettings(libraryDependencies ++= Seq(metaconfigSconfig.value))
// .jsSettings(
//   libraryDependencies ++= Seq(
//     metaconfigHocon.value,
//   )
// )

lazy val core = crossProject(JVMPlatform, NativePlatform)
  .in(file("scalafmt-core")).settings(
    moduleName := "scalafmt-core",
    buildInfoSettings,
    scalacOptions ++= scalacJvmOptions.value,
    libraryDependencies ++= Seq("org.scalameta" %%% "mdoc-parser" % mdocV),
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
  .nativeSettings(libraryDependencies += "com.lihaoyi" %%% "fastparse" % "3.1.1")
  .jvmSettings(Test / run / fork := true).dependsOn(sysops, config, macros)
  .enablePlugins(BuildInfoPlugin)
lazy val coreJVM = core.jvm
// lazy val coreJS = core.js

lazy val macros = crossProject(JVMPlatform, NativePlatform)
  .in(file("scalafmt-macros")).settings(
    moduleName := "scalafmt-macros",
    scalacOptions ++= scalacJvmOptions.value,
    libraryDependencies ++= Seq(
      scalameta.value,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    ),
  )

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

lazy val cli = crossProject(JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform).in(file("scalafmt-cli")).settings(
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
      "org.scalameta" %%% "munit-diff" % "1.0.2",
      "com.martiansoftware" % "nailgun-server" % "0.9.1",
      "com.github.scopt" %%% "scopt" % "4.1.0",
    ),
    scalacOptions ++= scalacJvmOptions.value,
    Compile / mainClass := Some("org.scalafmt.cli.Cli"),
  ).jvmSettings(
    nativeImageInstalled := isCI,
    nativeImageOptions ++= {
      // https://www.graalvm.org/22.3/reference-manual/native-image/guides/build-static-executables/
      // https://www.graalvm.org/latest/reference-manual/native-image/guides/build-static-executables/
      sys.env.get("NATIVE_IMAGE_STATIC") match {
        case Some("nolibc") => Seq(
            "-H:+UnlockExperimentalVMOptions",
            "-H:+StaticExecutableWithDynamicLibC",
            "-H:-UnlockExperimentalVMOptions",
          )
        case Some("musl") => Seq("--static", "--libc=musl")
        case _ => Nil
      }
    },
  ).nativeSettings(scalaNativeConfig).dependsOn(core, dynamic)
  .jvmEnablePlugins(NativeImagePlugin)

lazy val tests = crossProject(JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform).in(file("scalafmt-tests")).settings(
    publish / skip := true,
    libraryDependencies ++= Seq(
      // Test dependencies
      "com.lihaoyi" %%% "scalatags" % "0.13.1",
      scalametaTestkit.value,
      munit.value,
    ),
    scalacOptions ++= scalacJvmOptions.value,
    buildInfoPackage := "org.scalafmt.tests",
    buildInfoKeys := Seq[BuildInfoKey]("resourceDirectory" -> {
      val sharedTests = (baseDirectory.value.getParentFile / "shared").toPath
      (Test / resourceDirectories).value.find(_.toPath.startsWith(sharedTests))
        .get
    }),
  ).enablePlugins(BuildInfoPlugin)
  .jvmSettings(javaOptions += "-Dfile.encoding=UTF8")
  .dependsOn(core, dynamic, cli)

lazy val communityTestsCommon = project
  .in(file("scalafmt-tests-community/common")).settings(
    communityTestsSettings,
    libraryDependencies ++= Seq(
      // Test dependencies
      "com.lihaoyi" %% "scalatags" % "0.13.1",
      scalametaTestkit.value,
      munit.value,
    ),
  ).enablePlugins(BuildInfoPlugin).dependsOn(coreJVM)

lazy val communityTestsScala2 = project
  .in(file("scalafmt-tests-community/scala2")).settings(communityTestsSettings)
  .enablePlugins(BuildInfoPlugin).dependsOn(communityTestsCommon)

lazy val communityTestsScala3 = project
  .in(file("scalafmt-tests-community/scala3")).settings(communityTestsSettings)
  .enablePlugins(BuildInfoPlugin).dependsOn(communityTestsCommon)

lazy val communityTestsSpark = project
  .in(file("scalafmt-tests-community/spark")).settings(communityTestsSettings)
  .enablePlugins(BuildInfoPlugin).dependsOn(communityTestsCommon)

lazy val communityTestsIntellij = project
  .in(file("scalafmt-tests-community/intellij"))
  .settings(communityTestsSettings).enablePlugins(BuildInfoPlugin)
  .dependsOn(communityTestsCommon)

lazy val communityTestsOther = project
  .in(file("scalafmt-tests-community/other")).settings(communityTestsSettings)
  .enablePlugins(BuildInfoPlugin).dependsOn(communityTestsCommon)

lazy val benchmarks = project.in(file("scalafmt-benchmarks")).settings(
  publish / skip := true,
  moduleName := "scalafmt-benchmarks",
  libraryDependencies ++= Seq(scalametaTestkit.value),
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
).dependsOn(cli.jvm, dynamic.jvm).enablePlugins(DocusaurusPlugin)

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

lazy val communityTestsSettings: Seq[Def.Setting[_]] = Seq(
  publish / skip := true,
  scalacOptions ++= scalacJvmOptions.value,
  javaOptions += "-Dfile.encoding=UTF8",
  buildInfoPackage := "org.scalafmt.tests",
)

lazy val scalaNativeConfig = nativeConfig ~= { _.withMode(Mode.releaseFull) }
