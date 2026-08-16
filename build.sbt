import scala.util.Properties

import Dependencies._
import Extensions._

def isCI = System.getenv("CI") != null

inThisBuild {
  List(
    // version is set dynamically by sbt-dynver, but let's adjust it
    version := {
      val curVersion = version.value
      def dynVer(out: sbtdynver.GitDescribeOutput): String =
        if (out.isCleanAfterTag || isCI) curVersion
        else s"${out.ref.dropPrefix}-next-SNAPSHOT" // modified for local builds
      dynverGitDescribeOutput.value.mkVersion(dynVer, curVersion)
    },
    organization := smorgN,
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
    resolvers += Resolver.sonatypeCentralSnapshots,
    testFrameworks += TestFrameworks.MUnit,
    // causes native image issues
    dependencyOverrides += "org.jline" % "jline" % "3.30.16",
  )
}

lazy val runAssembly = inputKey[Unit]("Run assembly")

lazy val copyScalaNative = taskKey[Unit]("Copy Scala Native output to root")

def rootSettings = Def.settings(
  name := "scalafmtRoot",
  unpublished,
  crossScalaVersions := Nil, // the cells answer for their own versions
  copyScalaNative := {
    val binaryVersion = (cliNative / scalaBinaryVersion).value
    val suffix = if (Properties.isWin) ".exe" else ""
    val nativeOutput = (cliNative / Compile / target).value /
      s"scala-$binaryVersion" / s"scalafmt-cli$suffix"
    val output = baseDirectory.value / s"scalafmt$suffix"
    IO.copyFile(nativeOutput, output)
  },
)

def allMatrices = Seq(
  interfaces,
  sysops,
  config,
  macros,
  core,
  dynamicCore,
  dynamic,
  cli,
  tests,
  communityTestsCommon,
  communityTestsScala2,
  communityTestsScala3,
  communityTestsSpark,
  communityTestsIntellij,
  communityTestsOther,
  benchmarks,
  docs,
).flatMap(_.projectRefs)

// An explicit root, rather than the one sbt generates: in sbt 2 a bare
// top-level setting applies to every project, so `publish / skip` must have a
// project to live in or it would silently disable publishing build-wide.
lazy val root = project.in(file(".")).withId("scalafmt-root")
  .aggregate(allMatrices: _*).settings(rootSettings)

addCommandAlias("native-image", s"${cli.jvm(scala213).id}/nativeImage")
addCommandAlias(
  "scala-native",
  tasks(tasksOf(cliNative, "compile", "nativeLink") :+ "copyScalaNative"),
)
testAliases(scalaVersions, tests, cli)

lazy val dynamicCore = projectMatrix("scalafmt-dynamic-core").settings(
  moduleName := "scalafmt-dynamic-core",
  description := "Implementation of scalafmt-interfaces",
  buildInfoSettings("org.scalafmt.dynamic", "BuildInfo"),
  libraryDependencies ++= List("com.typesafe" % "config" % "1.4.9"),
  sharedTestSettings,
  scalacSettings,
  assembly / assemblyMergeStrategy := {
    case PathList("META-INF", "versions", "9", "module-info.class") =>
      MergeStrategy.discard
    case PathList("META-INF", "sisu", "javax.inject.Named") =>
      MergeStrategy.concat
    case x =>
      val oldStrategy = (assembly / assemblyMergeStrategy).value
      oldStrategy(x)
  },
).crossJvm().dependsOn(interfaces, sysops).dependsOn(core % "test")
  .enablePlugins(BuildInfoPlugin)

lazy val dynamic = projectMatrix("scalafmt-dynamic").settings(
  moduleName := "scalafmt-dynamic",
  description := "Implementation of scalafmt-dynamic using coursier",
  libraryDependencies += {
    val pkg = "io.get-coursier" %% "coursier" % coursier
    if (isScala3.value) (pkg cross CrossVersion.for3Use2_13)
      .exclude("org.scala-lang.modules", "scala-collection-compat_2.13")
    else pkg
  },
  sharedTestSettings,
  scalacSettings,
).crossJvm().dependsOn(dynamicCore).dependsOn(core % "test")

def interfacesSettings = Def.settings(
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
)

// The JVM sources are Java, so this row carries no Scala version.
def interfacesJvmSettings = Def.settings(
  javacOptions ++= Seq("-source", "8", "-target", "8"),
  Compile / doc / javacOptions := Seq("-Xdoclint:none", "-quiet"),
  Compile / doc / scalacOptions ++=
    Seq("-no-link-warnings", "-Wconf:cat=doc:silent"),
)

lazy val interfaces = projectMatrix("scalafmt-interfaces")
  .settings(interfacesSettings).crossJvmJava(interfacesJvmSettings)
  .crossJsNative

def sysopsSettings = Def.settings(
  moduleName := "scalafmt-sysops",
  description := "Scalafmt systems operations",
  scalacSettings,
  sharedTestSettings,
)

def sysopsJsSettings = Def.settings(
  libraryDependencies +=
    smorgN %%% "io" % scalametaV cross CrossVersion.for3Use2_13,
  scalaJsSettings,
)

lazy val sysops = projectMatrix("scalafmt-sysops").settings(sysopsSettings)
  .crossJvm().crossNative().crossJs(sysopsJsSettings)

def configSettings = Def.settings(
  moduleName := "scalafmt-config",
  description := "Scalafmt config parsing",
  scalacSettings,
  libraryDependencies += metaconfigCore.value,
)

def configJvmSettings = libraryDependencies += metaconfigTypesafe.value
def configNativeSettings = libraryDependencies += metaconfigSconfig.value
def configJsSettings = Def.settings(configNativeSettings, scalaJsSettings)

lazy val config = projectMatrix("scalafmt-config").settings(configSettings)
  .crossJvm(configJvmSettings).crossNative(configNativeSettings)
  .crossJs(configJsSettings)

def coreSettings = Def.settings(
  moduleName := "scalafmt-core",
  buildInfoSettings("org.scalafmt", "Versions"),
  scalacSettings,
  libraryDependencies += scalameta.value,
  libraryDependencies ++= Seq(smorgN %%% "mdoc-parser" % mdocV),
  libraryDependencies ++= {
    if (!isScala212.value) Nil
    else Seq(compilerPlugin(
      "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full,
    ))
  },
)

def coreNativeSettings = libraryDependencies +=
  "com.lihaoyi" %%% "fastparse" % "3.1.1"

lazy val core = projectMatrix("scalafmt-core").settings(coreSettings).crossJvm()
  .crossJs().crossNative(coreNativeSettings).aggregate(sysops, config, macros)
  .dependsOn(sysops, config, macros).enablePlugins(BuildInfoPlugin)
lazy val coreJVM = core.jvm(scala213)

def macrosSettings = Def.settings(
  moduleName := "scalafmt-macros",
  scalacSettings,
  libraryDependencies += scalameta.value,
  libraryDependencies ++= {
    if (isScala3.value) Nil
    else Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value)
  },
)

lazy val macros = projectMatrix("scalafmt-macros").settings(macrosSettings)
  .crossAll

import sbtassembly.AssemblyPlugin.defaultUniversalScript

def cliJsSettings = Def
  .settings(scalaJsSettings, scalaJSUseMainModuleInitializer := true)

def cliJvmSettings = Def.settings(
  libraryDependencies += "com.facebook" % "nailgun-server" % "1.0.1",
  nativeImageInstalled := isCI,
  nativeImageOptions += "-march=compatibility",
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
  runAssembly := {
    val jar = (assembly / assemblyOutputPath).value
    val args = sbt.complete.DefaultParsers.spaceDelimited("<args>").parsed
    val cmd = Seq("java", "-jar", jar.getAbsolutePath) ++ args
    val exit = scala.sys.process.Process(cmd).!
    if (exit != 0) sys.error(s"runAssembly failed with exit code $exit")
  },
)

def cliSettings = Def.settings(
  moduleName := "scalafmt-cli",
  assembly / aggregate := false,
  assembly / mainClass := Some("org.scalafmt.cli.Cli"),
  assembly / assemblyOption := (assembly / assemblyOption).value
    .withPrependShellScript(Some(defaultUniversalScript(shebang = false))),
  assembly / assemblyJarName := "scalafmt.jar",
  assembly / assemblyMergeStrategy := {
    case "reflect.properties" => MergeStrategy.first
    case PathList("scala-collection-compat.properties") => MergeStrategy.first
    case PathList("META-INF", "versions", "9", "module-info.class") =>
      MergeStrategy.discard
    case PathList("META-INF", "sisu", "javax.inject.Named") =>
      MergeStrategy.concat
    case x =>
      val oldStrategy = (assembly / assemblyMergeStrategy).value
      oldStrategy(x)
  },
  libraryDependencies ++= Seq(
    smorgN %%% "munit-diff" % munitV,
    "com.github.scopt" %%% "scopt" % "4.1.0",
  ),
  scalacSettings,
  Compile / mainClass := Some("org.scalafmt.cli.Cli"),
  sharedTestSettings,
)

lazy val cli = projectMatrix("scalafmt-cli").settings(cliSettings)
  .crossJvmRow(scalaVersions: _*)(cliJvmRow).crossNative(scalaNativeConfig)
  .dependsOn(core, interfaces)
  // TODO: enable NPM publishing
  .crossJs(cliJsSettings)
def cliNative = cli.native(scala213)

// `dynamic` is JVM-only, so a matrix-wide dependency would leave the JS and
// native rows with no row to resolve against.
def cliJvmRow(v: String): Project => Project = _.enablePlugins(NativeImagePlugin)
  .dependsOn(dynamic.jvm(v)).aggregate(dynamic.jvm(v)).settings(cliJvmSettings)

def testsSettings = Def.settings(
  unpublished,
  sharedTestSettings,
  libraryDependencies += scalametaTestkit.value % Test,
  libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.13.1" % Test,
  scalacSettings,
  buildInfoPackage := "org.scalafmt.tests",
  // a cell's baseDirectory is a generated .sbt/matrix directory, so name the
  // shared tree from the build root
  buildInfoKeys := Seq[BuildInfoKey](
    "resourceDirectory" -> (ThisBuild / baseDirectory).value /
      "scalafmt-tests" / "shared" / "src" / "test" / "resources",
  ),
)

def testsJvmSettings = Def
  .settings(javaOptions += "-Dfile.encoding=UTF8", parallelCollections)

lazy val tests = projectMatrix("scalafmt-tests").settings(testsSettings)
  .enablePlugins(BuildInfoPlugin).dependsOn(core).aggregate(core)
  .crossJvm(testsJvmSettings).crossJs(scalaJsSettings).crossNative()

lazy val communityTestsCommon = projectMatrix("scalafmt-tests-community/common")
  .communityTest.dependsOn(core)

lazy val communityTestsScala2 = projectMatrix("scalafmt-tests-community/scala2")
  .communityTest.dependsOn(communityTestsCommon % "test->test")

lazy val communityTestsScala3 = projectMatrix("scalafmt-tests-community/scala3")
  .communityTest.dependsOn(communityTestsCommon % "test->test")

lazy val communityTestsSpark = projectMatrix("scalafmt-tests-community/spark")
  .communityTest.dependsOn(communityTestsCommon % "test->test")

lazy val communityTestsIntellij =
  projectMatrix("scalafmt-tests-community/intellij").communityTest
    .dependsOn(communityTestsCommon % "test->test")

lazy val communityTestsOther = projectMatrix("scalafmt-tests-community/other")
  .communityTest.dependsOn(communityTestsCommon % "test->test")

lazy val benchmarks = projectMatrix("scalafmt-benchmarks").settings(
  unpublished,
  moduleName := "scalafmt-benchmarks",
  libraryDependencies += scalametaTestkit.value,
  libraryDependencies += munit.value % Test,
  run / javaOptions ++= Seq(
    "-Djava.net.preferIPv4Stack=true",
    "-XX:ReservedCodeCacheSize=128m",
    "-XX:MaxMetaspaceSize=1024m",
    "-Xss8M",
    "-Xms512M",
    "-Xmx2G",
  ),
).crossJvm().dependsOn(core, cli).enablePlugins(JmhPlugin)

lazy val docs = projectMatrix(
  "scalafmt-docs",
  VirtualAxis.jvm,
  VirtualAxis.scalaABIVersion(scala212),
).settings(unpublished, mdoc := (Compile / run).evaluated)
  .jvmPlatform(Seq(scala212)).dependsOn(cli, dynamic)
  .enablePlugins(DocusaurusPlugin)

val V = "\\d+\\.\\d+\\.\\d+"
val ReleaseCandidate = s"($V-RC\\d+).*".r
val Milestone = s"($V-M\\d+).*".r

lazy val stableVersion = Def
  .setting((ThisBuild / version).value.replaceAll("\\+.*", ""))

def buildInfoSettings(pkg: String, obj: String): Seq[Def.Setting[_]] = Seq(
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
    "scala213" -> scala213,
    "scala3" -> scala3,
    "coursier" -> coursier,
    "commit" -> sys.process.Process("git rev-parse HEAD").lineStream_!.head,
    "timestamp" -> System.currentTimeMillis().toString,
    scalaVersion,
    sbtVersion,
  ),
  buildInfoPackage := pkg,
  buildInfoObject := obj,
)
