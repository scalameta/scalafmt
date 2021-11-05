import Dependencies._
import sbtcrossproject.CrossPlugin.autoImport.crossProject
import scala.scalanative.build._

def parseTagVersion: String = {
  import scala.sys.process._
  // drop `v` prefix
  "git describe --abbrev=0 --tags".!!.drop(1).trim
}
def localSnapshotVersion: String = s"$parseTagVersion-SNAPSHOT"
def isCI = System.getenv("CI") != null

def scala212 = "2.12.15"
def scala213 = "2.13.6"

inThisBuild(
  List(
    version ~= { dynVer =>
      if (isCI) dynVer
      else localSnapshotVersion // only for local publishing
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
    crossScalaVersions := List(scala213, scala212),
    resolvers ++= Seq(
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots")
    ),
    testFrameworks += new TestFramework("munit.Framework")
  )
)

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
  s"++$scalaVersion" ::
    "tests/test" ::
    "publishLocal" ::
    docsTest ::
    s
}

commands += Command.command("ci-test-native") { s =>
  val scalaVersion = sys.env.get("TEST") match {
    case Some("2.12") => scala212
    case _ => scala213
  }
  s"++$scalaVersion" ::
    "testsNative/test" ::
    s
}

lazy val dynamic = crossProject(JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .in(file("scalafmt-dynamic"))
  .settings(
    moduleName := "scalafmt-dynamic",
    description := "Implementation of scalafmt-interfaces",
    buildInfoSettings,
    buildInfoPackage := "org.scalafmt.dynamic",
    buildInfoObject := "BuildInfo",
    libraryDependencies ++= List(
      "io.get-coursier" % "interface" % "0.0.17",
      "com.typesafe" % "config" % "1.4.1",
      munit.value % Test,
      scalametaTestkit.value % Test
    ),
    scalacOptions ++= scalacJvmOptions.value
  )
  .nativeSettings(
    libraryDependencies ++= List(
      "org.ekrich" %%% "sjavatime" % "1.1.5"
    ),
    scalaNativeNativeConfig
  )
  .dependsOn(interfaces)
  .enablePlugins(BuildInfoPlugin)

lazy val dynamicNative = dynamic.native

lazy val interfaces = crossProject(JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .in(file("scalafmt-interfaces"))
  .settings(
    moduleName := "scalafmt-interfaces",
    description := "Dependency-free, pure Java public interfaces to integrate with Scalafmt through a build tool or editor plugin.",
    crossVersion := CrossVersion.disabled,
    autoScalaLibrary := false,
    Compile / resourceGenerators += Def.task {
      val out =
        (Compile / managedResourceDirectories).value.head / "scalafmt.properties"
      val props = new java.util.Properties()
      props.put("version", version.value)
      IO.write(props, "scalafmt properties", out)
      List(out)
    }
  )

lazy val interfacesNative = interfaces.native

lazy val core = crossProject(JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .in(file("scalafmt-core"))
  .settings(
    moduleName := "scalafmt-core",
    buildInfoSettings,
    scalacOptions ++= scalacJvmOptions.value,
    libraryDependencies ++= Seq(
      scalameta.value,
      // scala-reflect is an undeclared dependency of fansi, see #1252.
      // Scalafmt itself does not require scala-reflect.
      "org.scala-lang" % "scala-reflect" % scalaVersion.value
    ),
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 13)) =>
          Seq(
            "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
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
  .nativeSettings(
    libraryDependencies ++= List(
      metaconfigSconfig.value
    ),
    scalaNativeNativeConfig
  )
  .jvmSettings(
    Test / run / fork := true,
    libraryDependencies ++= List(
      metaconfigTypesafe.value,
      metaconfig.value
    )
  )
  .enablePlugins(BuildInfoPlugin)

lazy val coreNative = core.native

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

lazy val scalaNativeNativeConfig =
  nativeConfig ~= {
    _.withMode(Mode.debug)
      .withLinkStubs(true)
  }

lazy val cli = crossProject(JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .in(file("scalafmt-cli"))
  .settings(
    moduleName := "scalafmt-cli",
    libraryDependencies ++= Seq(
      "com.github.scopt" %%% "scopt" % "4.0.1",
      // undeclared transitive dependency of coursier-small
      "org.scala-lang.modules" %% "scala-xml" % "1.3.0"
    ),
    scalacOptions ++= scalacJvmOptions.value,
    Compile / mainClass := Some("org.scalafmt.cli.Cli")
  )
  .jvmSettings(
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
    nativeImageVersion := "20.1.0",
    nativeImageOptions ++= {
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
    },
    libraryDependencies += "com.martiansoftware" % "nailgun-server" % "0.9.1"
  )
  .nativeSettings(
    scalaNativeNativeConfig
  )
  .dependsOn(core, dynamic)
  .enablePlugins(NativeImagePlugin)

lazy val cliNative = cli.native

lazy val tests = crossProject(JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .in(file("scalafmt-tests"))
  .settings(
    publish / skip := true,
    libraryDependencies ++= Seq(
      // Test dependencies
      "com.lihaoyi" %%% "scalatags" % "0.10.0",
      scalametaTestkit.value,
      munit.value
    ),
    scalacOptions ++= scalacJvmOptions.value,
    buildInfoPackage := "org.scalafmt.tests",
    buildInfoKeys := Seq[BuildInfoKey](
      "resourceDirectory" -> (baseDirectory.value / ".." / "shared" / "src" / "test" / "resources")
    )
  )
  .enablePlugins(BuildInfoPlugin)
  .dependsOn(core, dynamic, cli)
  .nativeSettings(
    scalaNativeNativeConfig
  )
  .jvmSettings(
    javaOptions += "-Dfile.encoding=UTF8"
  )

lazy val testsNative = tests.native

lazy val benchmarks = project
  .in(file("scalafmt-benchmarks"))
  .settings(
    publish / skip := true,
    moduleName := "scalafmt-benchmarks",
    libraryDependencies ++= Seq(
      scalametaTestkit.value
    ),
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
      "-server"
    )
  )
  .dependsOn(core.jvm)
  .enablePlugins(JmhPlugin)

lazy val docs = project
  .in(file("scalafmt-docs"))
  .settings(
    crossScalaVersions := List(scala212),
    publish / skip := true,
    mdoc := (Compile / run).evaluated
  )
  .dependsOn(cli.jvm, dynamic.jvm)
  .enablePlugins(DocusaurusPlugin)

val V = "\\d+\\.\\d+\\.\\d+"
val ReleaseCandidate = s"($V-RC\\d+).*".r
val Milestone = s"($V-M\\d+).*".r

lazy val stableVersion = Def.setting {
  (ThisBuild / version).value.replaceAll("\\+.*", "")
}

lazy val buildInfoSettings: Seq[Def.Setting[_]] = Seq(
  buildInfoKeys := Seq[BuildInfoKey](
    name,
    version,
    "scalameta" -> scalametaV,
    "nightly" -> version.value,
    "stable" -> stableVersion.value,
    "scala" -> scalaVersion.value,
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
